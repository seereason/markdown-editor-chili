{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
module SimpleEditor where

-- NEXT STEP: move layout recalculation into a single location
-- NEXT STEP: when inserting a foreign patch, we need to update font metrics with any new characters

{-

NOTES:

It would be nice to have a better document representation. At the moment the following operations are rather difficult:

 1. calculating the index from an (x,y) position

 2. finding the atom corresponding to an index

 3. finding the DOM node corresponding to an index

-}

import Common
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue)
import Control.Lens ((^.), (.~), (?~), (&), (%~), (^?), _Just, set, _2, foldrOf, folded)
import Control.Lens.At (at, ix)
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Chili.HSX
import Chili.Loop (loop)
import Chili.Types
import           Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import Data.Char (chr)
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable (foldl')
import qualified Data.Foldable as F
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.List (findIndex, groupBy, nub, null, splitAt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, maybe, catMaybes, isNothing)
import Data.Monoid ((<>))
import Data.Patch (Patch, Edit(..), toList, fromList, applicable, apply, diff)
import qualified Data.Patch as Patch
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Sequence (Seq, (|>), (<|), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import Data.UserId (UserId(..))
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Debug.Trace (trace)
import           GHCJS.Buffer               (toByteString, fromByteString, getArrayBuffer, createFromArrayBuffer)
import qualified GHCJS.Buffer as Buffer
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback)
import           GHCJS.Marshal (FromJSVal(..))
import           GHCJS.Marshal.Pure (PFromJSVal(..), PToJSVal(..))
import GHCJS.Types (JSVal(..), isNull, isUndefined)
import           JavaScript.TypedArray.ArrayBuffer (ArrayBuffer, MutableArrayBuffer)
import qualified JavaScript.TypedArray.ArrayBuffer as ArrayBuffer
import JavaScript.Web.MessageEvent (MessageEvent(..), MessageEventData(..))
import qualified JavaScript.Web.MessageEvent as MessageEvent
import qualified JavaScript.Web.WebSocket as WebSockets
import JavaScript.Web.WebSocket (WebSocket, WebSocketRequest(..), connect, send)
import Language.Haskell.HSX.QQ (hsx)
import Web.Editor.API
import System.IO.Unsafe (unsafePerformIO)

debugStrLn :: String -> IO ()
debugStrLn = putStrLn

debugPrint :: (Show a) => a -> IO ()
debugPrint = print

safeApply :: (Eq a) => Patch a -> Vector a -> Vector a
safeApply p v =
  if applicable p v
  then apply p v
  else unsafePerformIO $ do js_alert "Patch can not be applied." >> pure v

fontWeightJS :: FontWeight -> JS.JSString
fontWeightJS weight =
  case weight of
    FW100 -> "100"
    FW200 -> "200"
    FW300 -> "300"
    FW400 -> "400"
    FW500 -> "500"
    FW600 -> "600"
    FW700 -> "700"
    FW800 -> "800"
    FW900 -> "900"

data SelectionData = SelectionData
  { _selection       :: Selection
  , _selectionString :: String
  , _rangeCount      :: Int
  }
makeLenses ''SelectionData

data LocalDocument = LocalDocument
  { _document     :: Document -- Seq (Patch Atom) -- ^ expected to always be free of conflicts.
--  , _currentEdit :: [Edit Atom]
  , _inflightPatch :: Maybe (Patch Atom)
  , _forkedAt      :: Int
  , _pendingPatches :: Seq (Patch Atom)
  , _pendingEdit   :: [Edit Atom]
  }
  deriving Show
makeLenses ''LocalDocument

flattenDocument :: LocalDocument -> Vector Atom
flattenDocument localDoc =
  let committedDoc = foldl' (\doc patch -> apply patch doc) mempty (localDoc ^. document ^. patches) -- conflict-free patches
--      inflight' = fromMaybe (Patch.fromList []) (localDoc ^. inflightPatch)
      pendingPatch = Patch.fromList (localDoc ^. pendingEdit)
      pendingPatches' = (localDoc ^. pendingPatches) |> pendingPatch
  in let doc' =
           case localDoc ^. inflightPatch of
             (Just ifp) ->
               if applicable ifp committedDoc
               then apply ifp committedDoc
               else committedDoc -- FIXME
             Nothing -> committedDoc
     in foldl' (\d p -> if applicable p d
                     then apply p d
                        else d {-FIXME -}) doc' pendingPatches'

data MouseActivity
     = MouseNoop
     | MouseDowned MouseEventObject
--     | MouseMoved MouseEventObject

-- could have a cache version of the Document with all the edits applied, then new edits would just mod that document
data Model = Model
  { _localDocument :: LocalDocument -- Vector Atom -- ^ all of the patches applied but not the _currentEdit
  , _itemizing     :: Bool
  , _connectionId  :: Maybe ConnectionId
  , _editState     :: EditState
  , _index         :: Index -- ^ current index for patch edit operations
  , _caret         :: Index -- ^ position of caret
  , _fontMetrics   :: FontMetrics
  , _currentFont   :: Font
  , _debugMsg      :: Maybe Text
  , _mousePos      :: Maybe (Double, Double)
  , _editorPos     :: Maybe DOMClientRect
  , _targetPos     :: Maybe DOMClientRect
  , _layout        :: VBox [HBox [AtomBox]]
  , _maxWidth      :: Double -- ^ in pixels
  , _selectionData :: Maybe SelectionData
  , _currentRange  :: Maybe Range
  , _documentRange :: Maybe (Int, Int)
  , _rangeAnchor   :: Maybe (JSNode, Int) -- ^ where the selection is anchored -- could be at beginning or end depending on direction of mouse drag
  , _userId        :: UserId
  , _lastActivity  :: POSIXTime
  , _mouseActivity :: MouseActivity
  }
makeLenses ''Model

data EditAction
  = InsertAtom Atom
  | DeleteAtom
  | MoveCaret Index
    deriving Show

bullet :: RichText
bullet = RichText [(defaultFont, " • ")]

calcSizes :: [[AtomBox]] -> VBox [HBox [AtomBox]]
calcSizes [] = Box 0 0 False []
calcSizes lines =
  mkHBox (map mkHBox lines)
--  mkVBox [] -- (mkHBox line : calcSizes lines)
  where
--    mkVBox [] = Box 0 0 []
--    mkHBox [] = Box 0 0 []
    mkHBox boxes =
      Box { _boxWidth     = sum (map _boxWidth boxes)
          , _boxHeight    = maximum (map _boxHeight boxes)
          , _boxLineBreak = True
          , _boxContent   = boxes
          }

charToBox :: FontMetrics
          -> RichChar
          -> AtomBox
charToBox fm rc =
  Box { _boxWidth     = getWidth fm rc
      , _boxHeight    = getHeight fm rc
      , _boxLineBreak = False
      , _boxContent   = RC rc
      }
  where
    getWidth, getHeight :: FontMetrics -> RichChar -> Double
    getWidth  fm c = maybe 0 fst (Map.lookup c fm)
    getHeight fm c = maybe 0 snd (Map.lookup c fm)

-- | convert a 'Text' to a 'Box'.
--
-- Note that the Text will be treated as 'unbreakable'. If you want to
-- be able to break on whitespace, first use 'textToWords'.
textToBox :: FontMetrics
          -> RichText
          -> AtomBox
textToBox fm input
  | null (input ^. text) = Box { _boxWidth   = 0
                               , _boxHeight  = 0
                               , _boxLineBreak = False
                               , _boxContent = RT input
                               }
  | otherwise =
      Box { _boxWidth     = foldr (\(f,txt) w' -> Text.foldr (\c w -> w + getWidth fm (RichChar f c)) w' txt) 0 (input ^. text)
          , _boxHeight    = maximum (map (getHeight fm) [ RichChar f (Text.head txt) | (f, txt) <- input ^. text ])
          , _boxLineBreak = False
          , _boxContent   = RT input
          }
  where
    getWidth, getHeight :: FontMetrics -> RichChar -> Double
    getWidth  fm c = maybe 0 fst (Map.lookup c fm)
    getHeight fm c = maybe 0 snd (Map.lookup c fm)

-- | similar to words except whitespace is preserved at the end of a word
richCharsToWords :: [RichChar] -> [[RichChar]]
richCharsToWords [] = []
richCharsToWords txt =
      let whiteIndex   = fromMaybe 0 $ findIndex (\rc -> (rc ^. char) ==' ') txt
          charIndex    = fromMaybe 0 $ findIndex (\rc -> (rc ^. char) /= ' ') (drop whiteIndex txt)
      in case whiteIndex + charIndex of
          0 -> [txt]
          _ -> let (word, rest) = splitAt (whiteIndex + charIndex) txt
               in (word : richCharsToWords rest)

-- | FIXME: possibly not tail recursive -- should probably use foldr or foldl'
layoutBoxes :: Double -> (Double, [[Box c a]]) -> [[Box c a]]
layoutBoxes maxWidth (currWidth, []) = []
layoutBoxes maxWidth (currWidth, (hbox@(b0:bs):boxes))

  -- handle case were the box starts with a linebreak
  | b0 ^. boxLineBreak =
--      [] : layoutBoxes maxWidth (0, (box & boxLineBreak .~ False):boxes)
    (layoutBoxes maxWidth (currWidth, ((b0 & boxLineBreak .~ False):bs):[]) ++ (layoutBoxes maxWidth (0, boxes)))


  | currWidth + (foldrOf (folded . boxWidth) (+) 0 hbox) {- (hbox ^. boxWidth) -} <= maxWidth =
      case layoutBoxes maxWidth ((currWidth + (foldrOf (folded . boxWidth) (+) 0 hbox)), boxes) of
       [] -> [hbox] -- this is the last box
       (line:lines) -> (hbox++line):lines

  -- if a box is longer than the maxWidth we will place it at the start of a line and let it overflow
  | (currWidth == 0) && ((foldrOf (folded . boxWidth) (+) 0 hbox) > maxWidth) =
      hbox : layoutBoxes maxWidth (0, boxes)

  | otherwise =
      ([]:layoutBoxes maxWidth (0, hbox:boxes))

hboxIt :: [Box o a] -> HBox [Box o a]
hboxIt items =
  let maxHeight  = maximum [h | Box _ h _ _ <- items]
      totalWidth = sum     [w | Box w _ _ _ <- items]
  in Box totalWidth maxHeight False items

-- |
-- FIXME: should probably use a fold or something
boxify :: FontMetrics
       -> Vector Atom
       -> [[AtomBox]]
boxify fm v
  | Vector.null v = []
  | otherwise =
    case (Vector.head v) of
      RC {} ->
        case Vector.span isRichChar v of
          (richChars, rest) ->
            (map (map (charToBox fm)) $ richCharsToWords $ (Vector.toList (Vector.map unRichChar richChars))) ++ (boxify fm rest)

{-
      RC {} ->
        case Vector.span isRichChar v of
          (richChars, rest) ->
            (map (textToBox fm) $ map richCharsToRichText $ richCharsToWords $ (Vector.toList (Vector.map unRichChar richChars))) ++ (boxify fm rest)
-}
      atom@(Img img) ->
        ([Box (img ^. imageWidth) (img ^. imageHeight) False atom] : boxify fm (Vector.tail v))
      LineBreak ->
        ([[Box 0 16 True LineBreak]]  ++ boxify fm (Vector.tail v)) -- FIXME: height should probably be the height of a space char?
      Item ->
        let atomBox = (textToBox fm bullet) { _boxLineBreak = True
                                            , _boxContent = Item
                                            }
        in ([atomBox] : boxify fm (Vector.tail v))
      atom -> error $ "boxify does not handle this atom yet: " ++ show atom

atomsToLines :: FontMetrics
             -> Double       -- ^ maximum width of a line
             -> Vector Atom
             -> VBox [HBox [AtomBox]]
atomsToLines fm maxWidth atoms =
  let boxes = boxify fm atoms -- map (textToBox fm) (textToWords txt)
  in calcSizes $ layoutBoxes maxWidth (0, boxes)

-- | create a layout using: the current document + currentEdit + othersEditsn
calcLayout :: Model
           -> VBox [HBox [AtomBox]]
calcLayout model =
  atomsToLines (model ^. fontMetrics) (model ^. maxWidth) (flattenDocument $ model ^. localDocument)

updateLayout :: Model -> Model
updateLayout m = m { _layout = calcLayout m }

insertChar :: Atom
           -> Model
           -> Model
insertChar atom model =
  let newEdit = (model ^. localDocument ^. pendingEdit) ++ [Insert (model ^. index) atom] -- (if c == '\r' then RichChar '\n' else RichChar c)]
  in
     updateLayout $ model { _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
                          , _caret       = succ (model ^. caret)
                          }

backspaceChar :: Model -> Model
backspaceChar model
 | (model ^. index) > 0 = -- FIXME: how do we prevent over deletion?
  let index'  = pred (model ^. index)
      c       = (flattenDocument $ model ^. localDocument) ! index' -- FIXME: probably does not handle RichText correctly
      newEdit = (model ^. localDocument ^. pendingEdit) ++ [Delete index' c]
  in
     updateLayout $
       model {  _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
             , _index       = pred (model ^. index)
             , _caret       = pred (model ^. caret)
             }
 | otherwise = model


moveCaret :: Int -> Model -> Model
moveCaret i' model =
  let i = min (max i' 0) (Vector.length (flattenDocument (model ^. localDocument)))
  in updateLayout $ model { _index = i
                          , _caret = i
                          }

{- We only have one patch inflight at time.

This is our state machine.

One bit of state is what mode the editor is current in. Are we current inserting characters, deleting characters, moving the caret, etc.

The reason we care about this is because we only want to send a new patch when we change modes. Our rules for sending patches have other parameters though.

Instead of current mode -- where everything always has to be the same type, perhaps we only care about 'last action'.

When ever we get a new action, we want to transition to a new legal state.

-}

sendPatch :: (WebSocketReq -> IO ()) -> Model -> IO Model
sendPatch sendWS model =
  if (not $ null $ model ^. localDocument ^. pendingEdit) then
    do let newPatch = Patch.fromList (model ^. localDocument ^. pendingEdit)
           model' = model { _localDocument = (model ^. localDocument) & pendingPatches %~ (\pending -> pending |> newPatch )
                                                                      & pendingEdit    .~ []
                          , _index = model ^. caret
                          }
       case model' ^. localDocument ^. inflightPatch of
         Nothing ->
           case viewl (model' ^. localDocument ^. pendingPatches) of
             EmptyL -> pure model'
             (p :< ps) ->
               do sendWS (WebSocketReq (ReqAddPatch (model' ^. localDocument ^. forkedAt) p))
                  pure $ model' & (localDocument . pendingPatches) .~ ps
                                & (localDocument . inflightPatch) .~ Just p
         (Just ifp) -> error "Handle inflight patch support in sendPatch"
  else pure model

handleAction :: (WebSocketReq -> IO ()) -> EditAction -> Model -> IO (Maybe Model)
handleAction sendWS editAction model =
  do debugStrLn $ "editAction = " ++ show editAction
     now <- getPOSIXTime
     let newEditState = toEditState editAction
         b = isNewPatchCondition (model ^. localDocument ^. inflightPatch) (model ^. editState) newEditState
         model''' = model & editState    .~ newEditState
                          & lastActivity .~ now
     model'  <- if b then sendPatch sendWS model''' else pure model'''
     model'' <- applyEdit model' editAction
     pure (Just $ model'' & currentFont      .~
                           case model ^. documentRange of
                             Nothing ->
                               case editAction of
                                 (MoveCaret _) ->
                                   case model'' ^. caret of
                                     0 ->
                                       case findFontRight (flattenDocument (model'' ^. localDocument)) 0 of
                                       Nothing -> case findFontRight (flattenDocument (model'' ^. localDocument)) 0 of
                                         Nothing -> (model'' ^. currentFont) -- error $ "could not findFontRight or findFontLeft when caret = " ++ show (model'' ^. caret) -- f
                                         (Just f) -> f
                                       (Just f) -> f
                                     n -> case findFontLeft (flattenDocument (model'' ^. localDocument)) (pred n) of
                                       Nothing -> error $ "could not findFontLeft when n = " ++ show n -- f
                                       (Just f) -> f
                                 _ -> model'' ^. currentFont
                             (Just (b,e))
                               | b == e && b == 0 && Vector.length (flattenDocument (model'' ^. localDocument)) == 0 -> setBold False (model'' ^. currentFont)
                               | b == e && b == 0 -> setBold (allBold (extractRange (0, 1) (model'' ^. localDocument))) (model'' ^. currentFont)
                               | b == e -> setBold (allBold (extractRange (pred b, b) (model'' ^. localDocument))) (model'' ^. currentFont)
                               | otherwise -> setBold (allBold (extractRange (b,e) (model'' ^. localDocument))) (model'' ^. currentFont))


       where
         toEditState :: EditAction -> EditState
         toEditState (InsertAtom {}) = Inserting
         toEditState (DeleteAtom {}) = Deleting
         toEditState (MoveCaret  {}) = MovingCaret

         isNewPatchCondition :: Maybe (Patch Atom) -> EditState -> EditState -> Bool
--         isNewPatchCondition (Just _) _            _            = False -- a patch is already inflight, so we can just continue our pendingEdit
         isNewPatchCondition _  oldEditState newEditState = not (oldEditState == newEditState)

         applyEdit :: Model -> EditAction -> IO Model
         applyEdit model editAction =
           case editAction of
             (InsertAtom c) -> pure $ insertChar c model
             DeleteAtom     -> pure $ backspaceChar model
             (MoveCaret i)  -> pure $ moveCaret i model

-- | convert from window(?) coordinates to coordinates relative to the
-- top, left corner of the editor div.
--
relativeClickPos :: Model
                 -> Double
                 -> Double
                 -> Maybe (Double, Double)
relativeClickPos model mx my =
   case model ^. editorPos of
    Nothing     -> Nothing
    (Just epos) -> Just (mx - (rectLeft epos), my - (rectTop epos))


lineAtY :: VBox [HBox a] -> Double -> Maybe Int
lineAtY vbox y = go (vbox ^. boxContent) y 0
  where
    go [] _ n
      | n > 0 = Nothing -- Just (pred n) -- if Y is greater than any line, return the last line ?
      | otherwise = Nothing
--      | n > 0 = error $ "lineAtY: " ++ show n -- Just (n - 1) -- Nothing
--      | otherwise = Nothing
    go (hbox:hboxes) y n =
      if y < hbox ^. boxHeight
      then {- trace ("lineAtY " ++ show n) -} (Just n)
      else go hboxes (y - hbox ^. boxHeight) (succ n)


indexAtX :: FontMetrics -> HBox [AtomBox] -> Double -> (Int, Int)
indexAtX fm hbox x = go (hbox ^. boxContent) x 0
  where
    indexAtX' :: [RichChar] -> Double -> (Int, Int) -> (Int, Int)
    indexAtX' [] x (i, si) = (i, si)
    indexAtX' (c:cs) x (i, si) =
      let cWidth =  fst $ fromJust $ Map.lookup c fm
      in if x < cWidth
         then (i, si)
         else indexAtX' cs (x - cWidth) (succ i, succ si)
    go :: [AtomBox] -> Double -> Int -> (Int, Int)
    go [] x i = (i,0) -- FIXME? error "(i,0)" -- (i, 0)
    go (box:boxes) x i =
      case box ^. boxContent of
        (RC rc)
          | x < (box ^. boxWidth) -> (i, 0)
          | otherwise -> go boxes (x - box ^. boxWidth) (i + 1)
        (RT txt')
         | x < (box ^. boxWidth) ->
            case txt' of
              (RichText (txt:txts)) -> indexAtX' (richTextToRichChars (RichText (txt:txts))) x (i, 0)
              _ -> error "indexAtX -> go -> multiple txts not handle correctly."
         | boxes == [] ->
                case txt' of
                  (RichText ((_,txt):[])) ->
                    let len = Text.length txt in
                    (i + len , len)
                  _ -> error "indexAtX 2 -> go -> multiple txts not handle correctly."

         | otherwise -> go boxes (x - box ^. boxWidth) (i + richTextLength txt')
        (Img img)
         | x < (box ^. boxWidth) -> (i, i)
         | otherwise ->  go boxes (x - box ^. boxWidth) (i + 1)
        LineBreak
          | boxes == [] -> (i, 0)
          | otherwise -> go boxes x (i + 1)
        Item
         | x < (box ^. boxWidth) -> (i, i)
         | otherwise -> go boxes (x - box ^. boxWidth) (i + 1)
        EOF -> (i,i) -- fixme?

-- If we put Characters in boxes then we could perhaps generalize this
--
-- (index relative to start, index relative to hbox it is in)
{-
indexAtX :: FontMetrics -> HBox [AtomBox] -> Double -> (Int, Int)
indexAtX fm hbox x = go (hbox ^. boxContent) x 0
  where
    indexAtX' :: [RichChar] -> Double -> (Int, Int) -> (Int, Int)
    indexAtX' [] x (i, si) = (i, si)
    indexAtX' (c:cs) x (i, si) =
      let cWidth =  fst $ fromJust $ Map.lookup c fm
      in if x < cWidth
         then (i, si)
         else indexAtX' cs (x - cWidth) (succ i, succ si)
    go :: [AtomBox] -> Double -> Int -> (Int, Int)
    go [] x i = error "(i,0)" -- (i, 0)
    go (box:boxes) x i =
      case box ^. boxContent of
        (RT txt')
         | x < (box ^. boxWidth) ->
            case txt' of
              (RichText (txt:txts)) -> indexAtX' (richTextToRichChars (RichText (txt:txts))) x (i, 0)
              _ -> error "indexAtX -> go -> multiple txts not handle correctly."
         | boxes == [] ->
                case txt' of
                  (RichText ((_,txt):[])) ->
                    let len = Text.length txt in
                    (i + len , len)
                  _ -> error "indexAtX 2 -> go -> multiple txts not handle correctly."

         | otherwise -> go boxes (x - box ^. boxWidth) (i + richTextLength txt')
        (Img img)
         | x < (box ^. boxWidth) -> (i, i)
         | otherwise ->  go boxes (x - box ^. boxWidth) (i + 1)
        LineBreak
          | boxes == [] -> (i, 0)
          | otherwise -> go boxes x (i + 1)
        Item
         | x < (box ^. boxWidth) -> (i, i)
         | otherwise -> go boxes (x - box ^. boxWidth) (i + 1)
        EOF -> (i,i) -- fixme?
-}

getAtomNode :: Int -> [AtomBox] -> JSNode -> Word -> IO (Maybe JSNode)
getAtomNode 0 [] parent childNum = error $ "getAtomNode 0 []" -- pure Nothing
getAtomNode 0 _ {- (atom:atoms) -} parent childNum =
  do putStrLn $ "getAtomNode childNum=" ++ show childNum
     children <- childNodes parent
     item children childNum
getAtomNode n a@(atom:atoms) parent childNum =
  case atomLength (atom ^. boxContent) of
    n' | n > n' -> getAtomNode (n - n') atoms parent (childNum + (atomNumNodes atom))

       | n == n' ->
           do putStrLn "getAtomNode n == n'"
              getAtomNode (n - n') atoms parent (childNum + (atomNumNodes atom))
--              pure Nothing

       | otherwise ->
           case atom ^. boxContent of
             (RT (RichText (txt:txts))) -> do
               putStrLn $ "getAtomNode: " ++ show (RichText (txt:txts))
               if Text.length (snd txt) >= n
               then getAtomNode 0 a parent childNum
--               else getAtomNode 0 a parent childNum
               else do putStrLn $ "getAtomNode length txt = " ++ show (Text.length (snd txt)) ++ " , n = " ++ show n ++ " txt = " ++ show txt
                       getAtomNode (n - (Text.length (snd txt))) ((atom & boxContent .~ (RT (RichText txts))):[]) parent (childNum + 1)
             LineBreak ->
               do putStrLn $ "LineBreak n = " ++ show n ++ " n' = " ++show n'
                  getAtomNode 0 (atom:[]) parent (childNum )
             EOF ->
               do getAtomNode 0 (atom:[]) parent childNum
             o -> error $ "getAtomNode does not handle" ++ show (atom ^. boxContent)

      where
        atomNumNodes :: AtomBox -> Word
        atomNumNodes ab =
          case ab ^. boxContent of
            (RT (RichText txts)) -> fromIntegral $ length txts
            _ -> 1

{-
           do putStrLn $ "getAtomNode childNum=" ++ show childNum
              children <- childNodes parent
              item children childNum
-}
atomboxLength :: AtomBox -> Int
atomboxLength ab = atomLength (ab ^. boxContent)

hboxLength :: HBox [AtomBox] -> Int
hboxLength hbox =
  sum (map atomboxLength (hbox ^. boxContent))

vboxLength :: VBox [HBox [AtomBox]] -> Int
vboxLength vbox =
  sum (map (\hbox -> sum (map atomboxLength (hbox ^. boxContent))) (vbox ^. boxContent))

{-
Return the node which contains the element at index n.

The index refers to atom on the right
-}
getHBoxNode :: Int -> [HBox [AtomBox]] -> JSNode -> Word -> IO (Maybe JSNode)

{-
Here we want the 0th element. So we just get the childNum of the parent.
-}
getHBoxNode n [] _ _ = pure Nothing -- error $ "getHBoxNode: looking for n="++show n ++ " but the document is now []"
-- getHBoxNode 1 (hbox:[]) parent childNum 
getHBoxNode 0 (hbox:hboxes) parent childNum =
  do children <- childNodes parent
     mchild <- item children childNum
     case mchild of
       Nothing -> error $ "getHBoxNode could not find childNum=" ++ show childNum --  pure Nothing
       (Just child) -> getAtomNode 0 (hbox ^. boxContent) child 0

{-
Here n is greater than 0. So we need to look at the hbox and figure out which one contains the atom.

hboxLength is the number of atoms in the hbox. Keep in mind that we are 0-indexing our request.

If the length of an hbox is 2 and we are requesting index 2, then we need to look in the next hbox. If the length is 0 or 1, we look in the current hbox.

Since each hbox is a child of the parent, we increment the childNum by one.

-}
getHBoxNode n (hbox:hboxes) parent childNum =
  case hboxLength hbox of
    n' | n >= n' ->
           do putStrLn $ "getHBoxNode n = " ++ show n ++ " n' = " ++ show n'
              getHBoxNode (n - n') hboxes parent (childNum + 1)
{-
       | n == n' ->
           do putStrLn "getHBoxNode n == n'"
              children <- childNodes parent
              mchild <- item children childNum
              case mchild of
                Nothing      -> error $ "getHBoxNode: mchild is Nothing. Was looking for childNum =" ++ show childNum -- pure Nothing
                (Just child) -> getAtomNode n (hbox ^. boxContent) child 0
-}
       | otherwise ->
           do putStrLn $ "getHBoxNode n = " ++ show n ++ " n' = " ++ show n'
              children <- childNodes parent
              mchild <- item children childNum
              case mchild of
                Nothing      -> error $ "getHBoxNode: mchild is Nothing. Was looking for childNum =" ++ show childNum -- pure Nothing
                (Just child) -> getAtomNode n (hbox ^. boxContent) child 0

allP :: (Atom -> Bool) -> VBox [HBox [AtomBox]] -> Bool
allP p vbox =
  all (allP' p) (vbox ^. boxContent)
  where
    allP' :: (Atom -> Bool) -> HBox [AtomBox] -> Bool
    allP' p hbox =
      all (allP'' p) (hbox ^. boxContent)

    allP'' :: (Atom -> Bool) -> AtomBox -> Bool
    allP'' p atomBox = p (atomBox ^. boxContent)

-- | calculate the index to Atom in the document which is the hit
-- target of the provided (x,y) coordinates
--
indexAtPos :: FontMetrics           -- ^ font metrics for characters in this document
           -> VBox [HBox [AtomBox]] -- ^ current layout of the document
           -> (Double, Double)      -- ^ (x,y) coordinates relative to the top-left of the editor div
           -> Maybe (Int, Int)             -- ^ index of atom if a match was found
indexAtPos fm vbox (x,y) =
  case lineAtY vbox y of
   Nothing -> Just (sumPrevious (vbox ^. boxContent), 0) -- Nothing -- error "indexAtPos: Nothing"
   (Just i) ->
     let (ix, subIx) = (indexAtX fm ((vbox ^. boxContent)!!i) x)
     in Just $ ((sumPrevious $ take i (vbox ^. boxContent)) + ix, subIx)
   where
--     sumPrevious :: VBox [HBox [TextBox]] -> Maybe Int
     sumPrevious vboxes = sum (map sumLine vboxes)
--     sumPrevious :: VBox [HBox [TextBox]] -> Maybe Int
     sumLine vbox = sum (map (\box -> atomLength (box ^. boxContent)) (vbox ^. boxContent))

-- | calculate width and height of a 'RichChar'
getFontMetric :: JSElement -- ^ reference to hidden \<span\> which is used to meassure character sizes
              -> RichChar  -- ^ 'RichChar' to measure
              -> IO (RichChar, (Double, Double)) -- ^ (RichChar, (width, height))
getFontMetric measureElm rc@(RichChar font c) =
  do setStyle measureElm "font-size"   (JS.pack $ (show $ font ^. fontSize) ++ "px")
     setStyle measureElm "font-weight" (fontWeightJS $ font ^. fontWeight)
     setStyle measureElm "font-style"  (JS.pack $ show $ font ^. fontStyle)
     setInnerHTML measureElm (JS.pack $ replicate 100 (if c == ' ' then '\160' else c))
     domRect <- getBoundingClientRect measureElm
     -- FIXME: width and height are not official properties of DOMClientRect
     let w = width domRect / 100
         h = height domRect
     pure (rc, (w, font ^. fontSize)) -- FIXME: for some reason `height domRect` does not return the right value

getSelectionData :: Model -> IO (Maybe SelectionData)
getSelectionData m =
  do w <- window
     (Just d) <- currentDocument
     sel <- getSelection w
--     js_alert =<< (selectionToString sel)
     c <- getRangeCount sel
     txt <- selectionToString sel
--     range0 <- getRangeAt sel 0
--     bounds <- getBoundingClientRect range0
--     startElem <- startContainer range0
--     s <- js_rangeToString range0
--     print ("rangeToString", s)
--     rects <- getClientRects range0
--     print (clientRectsLength rects)
--     when (clientRectsLength rects > 0) $ do
--       s <- clientRectIx rects 0
--       print (crLeft s, crTop s)
--     rangeS <- createRange d
--     selectNode rangeS startElem
--     startElemBounds <- getBoundingClientRect startElem
--     endElem <- fmap (JSElement . unJSNode) (endContainer range0)
--     endElemBounds <- getBoundingClientRect endElem
     pure $ Just $ SelectionData { _selection       = sel
                                 , _selectionString = JS.unpack txt
                                 , _rangeCount      = c
                                 }


{-
foreign import javascript unsafe "$1[\"clipboardData\"][\"getData\"](\"text/plain\")" getClipboardData ::
        ClipboardEventObject -> IO JS.JSString
-}

foreign import javascript unsafe "$1[\"focus\"]()" js_focus ::
        JSElement -> IO ()

-- foreign import javascript unsafe "window[\"setTimeout\"]($1, $2)" js_setTimeout ::
--  Callback (IO ()) -> Int -> IO ()

-- | return a Map of any new metrics
calcMetrics :: FontMetrics -> [RichChar] -> IO (Map RichChar (Double, Double))
calcMetrics fm rcs =
  do (Just doc)    <- currentDocument
     mMeasureElem <- getElementById doc "measureElement"
     case mMeasureElem of
       Nothing -> pure Map.empty
       (Just measureElem) ->
         do metrics <- mapM (calcMetric measureElem) (nub rcs)
            pure (Map.fromList (catMaybes metrics))
  where
    calcMetric measureElem rc =
      case Map.lookup rc fm of
        (Just {}) -> pure Nothing
        Nothing ->
          do m <- getFontMetric measureElem rc
             pure (Just m)


-- | calculate tho CSS value for a 'Font'
fontToStyle :: Font -> Text
fontToStyle font =
  "font-size: " <> Text.pack (show $ font ^. fontSize) <>
  "px; height: " <> Text.pack (show $ font ^. fontSize) <>
  "px; font-weight: " <> (fontWeightText $ font ^. fontWeight) <>
  "; font-style: " <> (case (font ^. fontStyle) of
                          Normal  -> "normal;"
                          Italic  -> "italic;"
                          Oblique -> "oblique;")

-- | convert an 'AtomBox' to Html
--
-- note to self: a single 'RichText' can end up as multiple separate spans -- one for each element.
renderAtomBox :: AtomBox
              -> [Html Model]
-- renderTextBox box = CDATA True (box ^. boxContent)
renderAtomBox box =
  case box ^. boxContent of
    (RC (RichChar font c)) -> [[hsx| <span style=(fontToStyle font)><% nbsp (Text.singleton c) %></span> |]]
    (RT (RichText txts)) -> map renderText txts
    (Img img)            -> [[hsx|<img src=(img ^. imageUrl) />|]]
    LineBreak            -> [[hsx|<span style="display:inline-block;"></span>|]] -- FIXME: why do we explicitly render a LineBreak? Perhaps so we can have multiple empty lines between paragraphs? But isn't that handled by the line div? Perhaps the line div needs something in it or it is zero height?
    Item                 -> let (RichText txts) = bullet in map renderText txts
    EOF                  -> [[hsx|<span class="eof"></span> |]]
  where
    renderText :: (Font, Text) -> Html Model
    renderText (font, txt) = [hsx| <span style=(fontToStyle font)><% nbsp txt %></span> |]
    nbsp = Text.replace " " (Text.singleton '\160')

-- | convert a horizontal list of 'AtomBom' to Html
renderAtomBoxes' :: HBox [AtomBox] -> Html Model
renderAtomBoxes' box =
    [hsx| <div class=("line"::Text)><% concatMap renderAtomBox (box ^. boxContent)  %></div> |]
--  [hsx| <div class=(if line ^. lineHighlight then ("line highlight" :: Text) else "line")><% map renderTextBox (line ^. lineBoxes)  %></div> |]

-- | convert a vertical list of horizontal lists of 'AtomBom' to Html
renderAtomBoxes :: VBox [HBox [AtomBox]] -> Html Model
renderAtomBoxes lines =
  [hsx| <div class="lines"><% map renderAtomBoxes' (lines ^. boxContent) %></div> |]

-- render a layout (vertical list of horizontal lists of AtomBoxes) to Html
--
-- If you change this function or any of the functions it calls, it
-- can affect the getHBoxNode and friends functions. Obviously, we
-- should use a better abstraction so that things can not get out of
-- sync

renderLayout :: VBox [HBox [AtomBox]]
          -> Html Model
renderLayout lines =
  [hsx|
    <div id="editor-layout" data-path="root">
     <% renderAtomBoxes lines %>
--        <% textToHtml fm maxWidth 2 $ Text.pack $ Vector.toList (apply (Patch.fromList edits) mempty) %>
--      <% addP $ rlines $ Vector.toList (apply (Patch.fromList edits) mempty) %>
--      <% show $ map p $ rlines $ Vector.toList (apply (Patch.fromList edits) mempty) %>
     <span id="editor-end"></span>
    </div>
  |]

-- | calculate the x offset from the left side of an AtomBox to the
-- right side of the element at position specified by the index into
-- the AtomBox.
--
-- Some AtomBoxes only contain a single item (such as an
-- image). Others contain multiple items -- such as text.
indexToPosAtom :: Int
               -> FontMetrics
               -> AtomBox
               -> Maybe Double
indexToPosAtom index fm box =
  case box ^. boxContent of
    (RT rt)
      | index > richTextLength rt -> error $ "indexToPosAtom: " ++ show (index, richTextLength rt) -- Nothing
      | otherwise ->
          let w = Just $ foldr sumWidth 0 (take index (richTextToRichChars rt))
          in trace (show w) w
    (Img img) -> Just (img ^. imageWidth) -- box ^. boxWidth
    LineBreak -> Just 0 -- FIXME: do we have to account for the fact that we should be at a newline now?
    Item      -> Just (box ^. boxWidth)
    EOF        -> Just 0 -- FIXME?
  where
    sumWidth c acc =
      case Map.lookup c fm of
        Just (w, _) -> trace ("width = " ++ show w) $ acc + w
        Nothing -> error $ "missing width for " ++ show c -- acc

-- | given an index, calculate its (left, top, height) coordinates in the editor
--
-- NOTES:
--
-- It is a bit tricky to deal with the caret position that is at the very end of the document.
-- Where the caret goes depends if the last atom is a LineBreak or not.
--
{-
How to calculation position:

The layout is a list of nested boxes.

Things seem mostly straight forward -- we normally do not care if a line ends with an explicity LineBreak or is text-wrapped.

The tricky part seems to be when the caret is at the very end of the document. If there is no trailing LineBreak, the caret should be on the same line, if there is a LineBreak it should be on the next line.


-}

indexToPos :: Int
           -> Model
           -> Maybe (Double, Double, Double)
indexToPos i model = go (model ^. layout ^. boxContent) i (0,0,16) -- FIMXE: maybe shouldn't hardcode to 16
  where
    -- go over the lines
    go [] _ curPos  = trace ("go [] = " ++ show curPos) $ Just curPos -- Nothing
    go (hbox:hboxes) i curPos =
      -- walk over the current line
      case go' (hbox ^. boxContent) (hbox ^. boxHeight) i curPos of
       -- if the position is in current line, we are done
       (Right curPos') -> trace ("go = " ++ show curPos') curPos'
       -- otherwise add the height of that line and start
       -- looking in the next line
       (Left (i', (x,y,height), broke)) -- FIXME: deal with trailing newline versus no trailing newline
         | hboxes == [] -> trace ("go ended with i' = " ++ show i') $ Just (x,y,height)
         | otherwise -> go hboxes i' (0, if broke then y else y + hbox ^. boxHeight, height)

    -- go over the atoms in a line
    go' :: [AtomBox] -> Double -> Int -> (Double, Double, Double) -> Either (Int, (Double, Double, Double), Bool) (Maybe (Double, Double, Double))
    -- if there are no more atoms in the line, but we have not reached the index, return Left
    go' [] _ i curPos = {- trace ("go' [] =" ++ show (i, curPos)) -} (Left (i, curPos, False))
    -- if we are at index 0, then we have found the position, return Right
    go' _  _ 0 curPos = {- trace ("go' 0 = " ++ show (0, curPos)) -} (Right (Just curPos))
    -- if the last item in a line is a LineBreak
    go' (atom:[]) lineHeight i (x,y,height) | atom ^. boxContent == LineBreak = trace ("go' LineBreak " ++ show (x,y,height)) $ Left (pred i, (x,y + lineHeight,height), True) -- trace ("go' LineBreak") $ (Left (pred i, (0, y + lineHeight, height)))

    go' (box:boxes) lineHeight i (x,y,height) =
      -- if the index is greater than the length of the next atom
      if i >= atomLength (box ^. boxContent)
         -- subtract length of next atom, add width, update height, check next atom
         then go' boxes lineHeight (i - atomLength (box ^. boxContent)) (x + box ^. boxWidth, y, box ^. boxHeight)
         -- if we found the atom we are looking for, look for x position within the atom
         else trace "go' -> indexToPosAtom" $ case indexToPosAtom i (model ^. fontMetrics) box of
               Nothing   -> Right Nothing
               (Just x') ->
                 let boxForHeight = box
                     {-
                       case boxes of
                         (box':_) -> box'
                         _        -> box
-}
                 in Right (Just (x + x', y + (lineHeight - (boxForHeight ^. boxHeight)), boxForHeight ^. boxHeight))

caretPos :: Model -> Maybe (Double, Double, Double) -> [KV Text Text]
caretPos model Nothing = []
caretPos model (Just (x, y, height)) =
  case model ^. editorPos of
   Nothing -> []
   (Just ep) -> ["style" := ("top: "        <> (Text.pack $ show y)      <>
                             "px; left: "   <> (Text.pack $ show x)      <>
                             "px; height: " <> (Text.pack $ show height) <>
                             "px;"
                             )
                ]

-- | FIXME: having to call this explicitly is a source of bugs and silly repetition
updateEditorPos :: Model -> IO Model
updateEditorPos model =
  do (Just doc)        <- currentDocument
     (Just editorElem) <- getElementById doc "editor"
     focus editorElem
     rect <- getBoundingClientRect editorElem
     pure $ model { _editorPos = Just rect}

refocus :: Model -> IO ()
refocus model =
  do (Just doc)        <- currentDocument
     (Just editorElem) <- getElementById doc "editor"
     focus editorElem

keyPressed :: (WebSocketReq -> IO ()) -> KeyboardEventObject -> WithModel Model -> IO ()
keyPressed sendWS e withModel = withModel $ \model'' ->
  do model <- updateEditorPos model''
     let c = chr (charCode e)
         rc = RichChar (model ^. currentFont) c
     putStrLn $ "KeyPressed "++[c]
     newFontMetrics <-
       case Map.lookup rc (model ^. fontMetrics) of
         Nothing ->
           do (Just doc) <- currentDocument
              (Just measureElem) <- getElementById doc "measureElement"
              (_, metric) <- getFontMetric measureElem rc
              pure (Map.singleton rc metric)
         Just {} -> pure Map.empty
     let model' = case Map.lookup rc newFontMetrics of
                      Nothing -> model
                      (Just metric) -> set (fontMetrics . at rc) (Just metric) model
     handleAction sendWS (InsertAtom (RC rc)) model'

keyDowned :: (WebSocketReq -> IO ())
          -> KeyboardEventObject
          -> WithModel Model
          -> IO ()
keyDowned sendWS e withModel = withModel $ \model'' ->
 do putStrLn $ "KeyDowned "++ (show $ keyCode e)
    model <- updateEditorPos model''
    let c = (keyCode e)
    when (keyCode e == 8 || keyCode e == 32 || keyCode e == 13 || (keyCode e >= 37 && keyCode e <= 40)) (putStrLn "preventDefault" >> preventDefault e)
    newFontMetrics <-
       case () of
         () | c == 32 -> do
                          let rc = RichChar (model ^. currentFont) ' '
                          case Map.lookup rc (model ^. fontMetrics) of
                                Nothing ->
                                  do (Just document) <- currentDocument
                                     (Just measureElem) <- getElementById document "measureElement"
                                     (_, metric) <- getFontMetric measureElem rc
                                     pure (Map.singleton rc metric)
                                Just {} -> pure Map.empty
            | otherwise -> pure Map.empty
    case () of
     () | c == 8    -> handleAction sendWS DeleteAtom model
        | c == 13   -> case model ^. itemizing of
                         False -> handleAction sendWS (InsertAtom LineBreak) model
                         True  -> handleAction sendWS (InsertAtom Item) model
        | c == 32   -> let rc = RichChar (model ^. currentFont) ' '
                           model' = case Map.lookup rc newFontMetrics of
                             Nothing -> model
                             (Just metric) -> set (fontMetrics . at rc) (Just metric) model
                       in handleAction sendWS (InsertAtom (RC rc)) model'
        | c == 37   -> handleAction sendWS (MoveCaret ((model ^. caret) - 1)) model -- left
        | c == 38   -> pure Nothing -- model                                        -- up
        | c == 39   -> handleAction sendWS (MoveCaret ((model ^. caret) + 1)) model -- right
        | c == 40   -> pure Nothing -- model                                        -- down
        | otherwise -> pure Nothing -- model

buttonUp :: (WebSocketReq -> IO ()) -> MouseEventObject -> Model -> IO (Maybe Model)
buttonUp sendWS e model'' =
  do model <- updateEditorPos model''
     let elem = target e
     targetRect <- getBoundingClientRect elem
     let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
         mIndex = fst <$> indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
         model' = model & mousePos  ?~ (clientX e, clientY e)
                        & caret     .~ (fromMaybe (model ^. caret) mIndex)
                        & targetPos .~ (Just targetRect)
                        & documentRange .~ case model ^. documentRange of
                                             (Just (b,e)) | b == e -> Nothing
                                             dr -> dr
--                        & debugMsg  .~ Just (Text.pack (show (model ^. layout)))
     case mIndex of
       (Just i) -> handleAction sendWS (MoveCaret i) model'
       Nothing  -> pure $ Just $ model' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click.")
{-
The object that emits the MouseEvent is typically something like a
span. But we want to be able to select subtext, so we need to know the
offset into that span.
-}

commonSelection :: (WebSocketReq -> IO ()) -> Bool -> MouseEventObject -> Model -> IO (Maybe Model)
commonSelection sendWS newSelection e model'' =
  do -- first update our record of the position of the editor on the screen
     model <- updateEditorPos model''
     -- then get the bounding rectangle of whatever was clicked
     targetRect <- getBoundingClientRect (target e)

     let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
         mIndex = indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
         model' = model & mousePos  ?~ (clientX e, clientY e)
                        & caret     .~ (fromMaybe (model ^. caret) (fst <$> mIndex))
                        & targetPos .~ (Just targetRect)
     case mIndex of
       Nothing  -> pure $ Just $ model'' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click.")
       (Just (i, si)) ->
         do mRange <- if newSelection
              then do debugStrLn $ "Selection started at index="++ show i ++ ", subindex=" ++ show si
                      w     <- window
                      range <- newRange
                      sel   <- getSelection w
                      removeAllRanges sel
                      addRange sel range
                      pure (Just range)
              else do debugStrLn $ "update selection to index="++ show i ++ ", subindex=" ++ show si
                      pure $ model ^. currentRange

            case mRange of
              Nothing -> pure Nothing
              (Just range) ->
                   do (Just doc) <- currentDocument
                      (Just editorNode) <- getElementById  doc "editor-layout"
                      -- debugStrLn "editor-layout"
                      nt <- nodeType editorNode
                      -- debugStrLn $ nodeTypeString nt
                      nn <- nodeName editorNode
                      -- debugStrLn $ JS.unpack nn
                      (Just lines) <- fmap toJSNode <$> getFirstChild editorNode
                      mSelNode <- getHBoxNode i (model ^. layout ^. boxContent) lines 0
                      selNode <-
                        case mSelNode of
                          -- assumption is that the selection was past the end of the document
                          Nothing -> do debugPrint "commonSelection: apparent selection beyond end of document"
                                        (Just editorEndNode) <- getElementById  doc "editor-end"
                                        pure (toJSNode editorEndNode)
--                                        pure (Just model')
                          (Just selNode) -> pure selNode
                      -- nt <- nodeType selNode
                      -- debugStrLn $ nodeTypeString nt
                      -- nn <- nodeName selNode
                      -- debugStrLn $ JS.unpack nn
                      -- selNodeTxt <- getInnerHTML (JSElement (unJSNode selNode))
                      -- debugStrLn $ "selNodeTxt = " ++ JS.unpack selNodeTxt
                      mTextNode <- getFirstChild selNode
                      if newSelection
                               then do (node, off) <-
                                             case mTextNode of
                                               Nothing ->
                                                 do setStart range (toJSNode selNode) 0
                                                    setEnd   range (toJSNode selNode) 0
                                                    pure (toJSNode selNode, 0)
                                               (Just textNode) ->
                                                 do -- jstr <- nodeValue textNode
                                                    -- debugStrLn $ "nodeValue = " ++ JS.unpack jstr
                                                    setStart range (toJSNode textNode) si
                                                    setEnd range (toJSNode textNode) si
                                                    pure (toJSNode textNode, si)
                                       handleAction sendWS (MoveCaret i) (model' & currentRange .~ Just range
                                                                                 & documentRange .~ Just (i, i)
                                                                                 & rangeAnchor .~ Just (node, off)
                                                                         )
                               else do let Just (node, off) = model' ^. rangeAnchor
                                       case mTextNode of
                                         Nothing -> do
                                           case model' ^. documentRange of
                                             Nothing -> pure ()
                                             (Just (b,e))
                                               | i > b ->
                                                   do setStart range node off
                                                      setEnd range (toJSNode selNode) 0
                                               | i <= b ->
                                                   do setStart range (toJSNode selNode) 0
                                                      setEnd range node off
                                         (Just textNode) ->
                                           case model' ^. documentRange of
                                             Nothing -> pure ()
                                             (Just (b,e))
                                               | i > b ->
                                                 do setStart range node off
                                                    setEnd range (toJSNode textNode) si
{-
                                               | i == b ->
                                                 do setStart range (toJSNode textNode) si
                                                    setEnd range (toJSNode textNode) si
-}
                                               | i <= b ->
                                                   do setStart range (toJSNode textNode) si
                                                      setEnd range node off

                                       handleAction sendWS (MoveCaret i) (model' & documentRange %~ (\dr -> fmap (\(b,e) -> (b,i)) dr)) -- (min b i, max e i)) dr))
--                                       handleAction sendWS (MoveCaret i) (model' & documentRange %~ (\dr -> fmap (\(b,e) -> if i > b then (b, i) else (i, e)) dr)) -- (min b i, max e i)) dr))
                                       -- why do I want min/max here? It makes it so that you can make the selection bigger, but never smaller
--                                       handleAction sendWS (MoveCaret i) (model' & documentRange %~ (\dr -> fmap (\(b,e) -> (min b i, max e i)) dr))

{-

Manual Mouse Handling:

If the mouse is clicked and released in the same position then we move the caret.

If the mouse is click, dragged, and released, then we update the selection -- and move the caret to the release point?

-}

selectEditor :: (WebSocketReq -> IO ()) -> MouseEvent -> MouseEventObject -> WithModel Model -> IO ()
selectEditor sendWS mouseEV mouseEventObject withModel =
  do -- putStrLn $ "selectEditor: " ++ show mouseEV
     preventDefault mouseEventObject
     withModel $ \m ->
       case m ^. mouseActivity of
         MouseNoop ->
           case mouseEV of
             MouseDown ->
               do (Just m') <- commonSelection sendWS True mouseEventObject m
                  pure $ Just $ m' & mouseActivity .~ (MouseDowned mouseEventObject)
             _ -> pure Nothing
         MouseDowned oldMEO ->
           case mouseEV of
             MouseMove ->
               do (Just m') <- commonSelection sendWS False mouseEventObject m
                  pure (Just m')

             MouseUp ->
               do (Just m') <- buttonUp sendWS mouseEventObject m
                  pure $ Just $ m' & mouseActivity .~ MouseNoop
             _ -> pure Nothing

modifyFontRange :: (WebSocketReq -> IO ()) -> (Int, Int) -> (Vector Atom -> a) -> (a -> RichChar -> RichChar) -> Model -> IO (Maybe Model)
modifyFontRange sendWS (b, e) calcVal modifyRC model' =
  do model <- sendPatch sendWS model'
     let -- flattened = flattenDocument (model ^. localDocument)
         -- sub = Vector.slice b (e - b) flattened
         sub = extractRange (b, e) (model ^. localDocument)
         atoms = zip (Vector.toList sub) [b..e]
         newVal = calcVal sub -- if allBold sub then FW400 else FW700
         newEdit = catMaybes (map (\(a, i) ->
                             case a of
                               (RC rc@(RichChar f char)) -> Just $ Replace i a (RC (modifyRC newVal rc)) -- (RichChar (f & fontWeight .~  newFontWeight) char))
                               _ -> Nothing
                           ) atoms)
         chars = [ rc | (Replace _ _ (RC rc)) <- newEdit ]
     fms <- calcMetrics (model ^. fontMetrics) chars
--     print flattened
--     print sub
--     print atoms
--     print newEdit
     pure $ Just $ updateLayout $ model { _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
                                        -- , _caret         = succ (model ^. caret)
                                        , _fontMetrics   = (model ^. fontMetrics) `mappend` fms
                                        }

-- what if already bold? or a mix of bold and not bold?
boldRangeOld :: (WebSocketReq -> IO ()) -> (Int, Int) -> Model -> IO (Maybe Model)
boldRangeOld sendWS (b, e) model' =
  do model <- sendPatch sendWS model'
     let -- flattened = flattenDocument (model ^. localDocument)
         -- sub = Vector.slice b (e - b) flattened
         sub = extractRange (b, e) (model ^. localDocument)
         atoms = zip (Vector.toList sub) [b..e]
         newFontWeight = if allBold sub then FW400 else FW700
         newEdit = catMaybes (map (\(a, i) ->
                             case a of
                               (RC (RichChar f char)) -> Just $ Replace i a (RC (RichChar (f & fontWeight .~  newFontWeight) char))
                               _ -> Nothing
                           ) atoms)
         chars = [ rc | (Replace _ _ (RC rc)) <- newEdit ]
     fms <- calcMetrics (model ^. fontMetrics) chars
--     print flattened
--     print sub
--     print atoms
--     print newEdit
     pure $ Just $ updateLayout $ model { _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
                                        , _caret         = succ (model ^. caret)
                                        , _fontMetrics   = (model ^. fontMetrics) `mappend` fms
                                        }

boldRange :: (WebSocketReq -> IO ()) -> (Int, Int) -> Model -> IO (Maybe Model)
boldRange sendWS (b,e) model' =
  modifyFontRange sendWS (b,e) calcVal modifyRC model'
  where
    calcVal sub = if allBold sub then FW400 else FW700
    modifyRC v (RichChar f c) = RichChar (f & fontWeight .~ v) c

findFontLeft :: Vector Atom -> Int -> Maybe Font
findFontLeft atoms (-1) = Nothing
findFontLeft atoms n =
  case atoms ! n of
    (RC (RichChar f _)) -> Just f
    _ -> findFontLeft atoms (pred n)

findFontRight :: Vector Atom -> Int -> Maybe Font
findFontRight atoms n
  | n < Vector.length atoms =
    case atoms ! n of
      (RC (RichChar f _)) -> Just f
      _ -> findFontRight atoms (succ n)
  | otherwise = Nothing

--     (RT rt) -> findFontLeft' richTextToRichChars rt


allBold :: Vector Atom -> Bool
allBold atoms = F.foldr (\atom b -> isBold atom && b) True atoms
  where
    isBold :: Atom -> Bool
    isBold atom =
      case atom of
        (RC (RichChar f _)) -> isBoldFont f
        _ -> True

italicizeRange :: (WebSocketReq -> IO ()) -> (Int, Int) -> Model -> IO (Maybe Model)
italicizeRange sendWS (b,e) model' =
  modifyFontRange sendWS (b,e) calcVal modifyRC model'
  where
    calcVal sub = if allItalics sub then Normal else Italic
    modifyRC v (RichChar f c) = RichChar (f & fontStyle .~ v) c

italicizeRangeOld :: (WebSocketReq -> IO ()) -> (Int, Int) -> Model -> IO (Maybe Model)
italicizeRangeOld sendWS (b, e) model' =
  do model <- sendPatch sendWS model'
     let -- flattened = flattenDocument (model ^. localDocument)
         -- sub = Vector.slice b (e - b) flattened
         sub = extractRange (b, e) (model ^. localDocument)
         atoms = zip (Vector.toList sub) [b..e]
         newFontStyle = if allItalics sub then Normal else Italic
         newEdit = catMaybes (map (\(a, i) ->
                             case a of
                               (RC (RichChar f char)) -> Just $ Replace i a (RC (RichChar (f & fontStyle .~  newFontStyle) char))
                               _ -> Nothing
                           ) atoms)
--     print flattened
     print sub
     print atoms
     print newEdit
     pure $ Just $ updateLayout $ model { _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
                                        , _caret         = succ (model ^. caret)
                                        }

allItalics :: Vector Atom -> Bool
allItalics atoms = F.foldr (\atom b -> isItalic atom && b) True atoms
  where
    isItalic :: Atom -> Bool
    isItalic atom =
      case atom of
        (RC (RichChar (Common.Font _ _ fs) _)) -> fs == Italic
        _ -> True

extractRange :: (Int, Int) -> LocalDocument -> Vector Atom
extractRange (b', e') localDocument =
  let flattened = flattenDocument localDocument
      (b, e) = (min b' e', max b' e')
  in Vector.slice b (e - b) flattened

boldChange :: (WebSocketReq -> IO ()) -> MouseEventObject -> WithModel Model -> IO ()
boldChange sendWS ev withModel = withModel $ \model->
  do preventDefault ev
     stopPropagation ev
     refocus model
     case model ^. documentRange of
       Nothing ->
          do pure $ Just $ (model & (currentFont . fontWeight) %~ (\w -> if w == FW400 then FW700 else FW400)
--                                       & bolding .~ (not (model ^. bolding))
                                       & debugMsg .~ Just "BoldChange")
       (Just (b,e)) ->
         do boldRange sendWS (min b e, max b e) model

italicChange :: (WebSocketReq -> IO ()) -> MouseEventObject -> WithModel Model -> IO ()
italicChange sendWS ev withModel = withModel $ \model->
  do preventDefault ev
     stopPropagation ev
     refocus model
     case model ^. documentRange of
       Nothing -> pure $ Just $ (model & (currentFont . fontStyle) %~ (\fs -> if fs == Normal then Italic else Normal)
                                       & debugMsg .~ Just "ItalicChange")
       (Just (b,e)) ->
         do italicizeRange sendWS (min b e, max b e) model

itemize :: (WebSocketReq -> IO ()) -> MouseEventObject -> WithModel Model -> IO ()
itemize sendWS e withModel =  withModel $ \model->
  do preventDefault e
     case model ^. itemizing of
       False -> handleAction sendWS (InsertAtom Item) (model & itemizing .~ True)
       True  -> pure $ Just $ model & itemizing .~ False

editorCopy :: ClipboardEventObject -> WithModel Model -> IO ()
editorCopy e withModel =  withModel $ \model->
  do preventDefault e
     dt <- clipboardData e
     setDataTransferData dt "text/plain" "boo-yeah"
     pure $ Just $ model & debugMsg .~ Just "copy"

app :: (WebSocketReq -> IO ()) -> Model -> Html Model
app sendWS model =
  let setFontSize s = EL Click (\e withModel -> withModel $ \m -> pure $ Just $ m & (currentFont . fontSize) .~ s)
  in
         ([hsx|
           <div>
             <% if True
                 then <div style="position: absolute; left: 800px; width: 800px;">
                             <h1>Debug</h1>
{-
                             <p>userId: <% show (model ^. userId) %></p>
                             <p>debugMsg: <% show (model ^. debugMsg) %></p>
--                             <p>LocalDocument: <% show (model ^. localDocument) %></p>
--                             <p>Patches:  <% show (model  ^. patches) %></p>
                             <p>Index: <% show (model ^. index) %></p>
                             <p>Caret: <% show (model ^. caret) %></p>
                             <p>documentRange: <% show $ model ^. documentRange %></p>
                             <p>current font: <% show $ model ^. currentFont %></p>
                             <p>is bold <% show (isBoldFont $ model ^. currentFont) %></p>
{-
                             <% case model ^. selectionData of
                                     Nothing -> <p>No Selection</p>
                                     (Just selectionData) ->
                                       <div>
                                        <p>Selection: count=<% show $ selectionData ^. rangeCount %></p>
                                        <p>Selection: string len=<% show $ length $ selectionData ^. selectionString %></p>
                                        <p>Selection: toString()=<% selectionData ^. selectionString %></p>
                                       </div>
                             %>
-}

                             <p>indexToPos (x,y,h): <% show (indexToPos (model ^. caret) model) %> </p>
                             <p>caretPos: <% show $ caretPos model (indexToPos (model ^. caret) model) %></p>
                             <p>mousePos: <% show (model ^. mousePos) %></p>
                             <p>editorPos: <% let mpos = model ^. editorPos in
                                              case mpos of
                                                 Nothing -> "(,)"
                                                 (Just pos) -> show (rectLeft pos, rectTop pos) %></p>

                             <p>mousePos - editorPos: <% let mepos = model ^. editorPos in
                                   case mepos of
                                     Nothing -> "(,)"
                                     (Just epos) ->
                                       case model ^. mousePos of
                                         Nothing -> "(,)"
                                         (Just (mx, my)) -> show (mx - (rectLeft epos), my - (rectTop epos)) %></p>
                             <p>targetPos: <% let mpos = model ^. targetPos in
                                              case mpos of
                                                Nothing -> "(,)"
                                                (Just pos) -> show (rectLeft pos, rectTop pos) %></p>

                             <p>line heights: <% show (map _boxHeight (model ^. layout ^. boxContent)) %></p>
--                             <p>Font Metrics <% show (model ^. fontMetrics) %></p>
                             <p>Vector Atom: <div><% show $ flattenDocument (model ^. localDocument) %></div></p>
-}
--                             <p>layout: <div> <% map (\l -> <p><% show l %></p>) (model ^. layout ^. boxContent) %> </div></p>
                      </div>
                 else <span></span> %>
            <div class="form-line editor-toolbar row">
            <div class="col-md-6">
              <div class="btn-group" data-toggle="buttons">
               <label class=(if (isBoldFont (model ^. currentFont)) then ("btn btn-default active" :: Text) else ("btn btn-default" :: Text))
                    [ EL Click (boldChange sendWS) ] >
                <input type="checkbox" autocomplete="off" style="font-weight: 800;"/>B</label>
--               <label class=(if (model ^. italicizing) then ("btn btn-default active" :: Text) else ("btn btn-default" :: Text))  [ EL Click (italicChange sendWS) ] >
               <label class=(if (isItalicFont (model ^. currentFont)) then ("btn btn-default active" :: Text) else ("btn btn-default" :: Text))
                    [ EL Click (italicChange sendWS) ] >
                <input type="checkbox" autocomplete="off" style="font-style: italic;" />I</label>
               <label class="btn btn-default" [ EL Click (itemize sendWS) ] >
                <input type="checkbox" autocomplete="off" />•</label>
              </div>
             </div>
            </div>
            <div id="editor" tabindex="1" style="outline: 0; line-height: 1.0; height: 300px; width: 500px; border: 1px solid black; box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.2);" [ EL KeyPress (keyPressed sendWS)
                             , EL KeyDown  (keyDowned sendWS)
                             , EL MouseDown (selectEditor sendWS MouseDown)
                             , EL MouseUp   (selectEditor sendWS MouseUp)
                             , EL MouseMove (selectEditor sendWS MouseMove)
--                             , EL Click (selectEditor sendWS Click)
--                             , EL Click    (clickEditor sendWS)
--                             , EL Copy     editorCopy
                             , OnCreate (\el _ -> focus el)
                             ] >
            <div id="caret" class="editor-caret" (caretPos model (indexToPos (model ^. caret) model))></div>
               <% renderLayout (model ^. layout) %>
            </div>

           </div>
          |])

chili app initAction model url handleWS elemId =
  do (Just doc)   <- currentDocument
     (Just murv) <- getElementById doc elemId
--     (Just body)  <- item nodes 0
     loop doc (toJSNode murv) model initAction (Just url) handleWS app
--     initRemoteWS url (handleWS updateModel)
--     updateModel initAction


initModel = Model
  { _localDocument      = LocalDocument
      { _document = emptyDocument
      , _inflightPatch = Nothing
      , _forkedAt = 0
      , _pendingPatches = Seq.empty
      , _pendingEdit = []
      }
  , _itemizing     = False
  , _connectionId  = Nothing
  , _editState     = Inserting
  , _index         = 0
  , _caret         = 0
  , _fontMetrics   = Map.empty
  , _currentFont   = defaultFont
  , _debugMsg      = Nothing
  , _mousePos      = Nothing
  , _editorPos     = Nothing
  , _targetPos     = Nothing
  , _layout        = Box 0 0 False []
--  , _maxWidth      = 200
  , _maxWidth      = 200
  , _selectionData = Nothing
  , _currentRange  = Nothing
  , _documentRange = Nothing
  , _rangeAnchor   = Nothing
  , _userId        = UserId 0
  , _lastActivity  = 0
  , _mouseActivity = MouseNoop
  }

initAction :: (WebSocketReq -> IO ()) -> WithModel Model -> IO ()
initAction sendWS withModel = withModel $ \model ->
  do let rcs = nub $ richTextToRichChars bullet
     newMetrics <- calcMetrics (model ^. fontMetrics) rcs
     sendWS (WebSocketReq ReqInit)

     cb <- asyncCallback $ activityTimeout withModel
     js_setTimeout cb 2000

--     (Just doc) <- currentDocument
--     addEventListener doc SelectionChange (\e -> updateSelection e withModel) False

     pure (Just $ model & fontMetrics  %~ (\old -> old `mappend` newMetrics))
       where
         activityTimeout withModel =
           do now <- getPOSIXTime
              withModel $ \model ->
                if ((now > (model ^. lastActivity + 1)) && (not $ null $ model ^. localDocument . pendingEdit))
                   then do putStrLn "force patch send"
                           model' <- sendPatch sendWS model
                           pure $ Just $ model' & lastActivity .~ now
                   else pure Nothing
              cb <- asyncCallback $ activityTimeout withModel
              js_setTimeout cb 500

resInit withModel conn initDoc = withModel $ \model ->
  do let toRCs :: Atom -> [RichChar]
         toRCs (Img {})      = []
         toRCs (LineBreak)   = []
         toRCs EOF           = []
         toRCs Item          = richTextToRichChars bullet
         toRCs (RC rc) = [rc]
         toRCs (RT rt) = richTextToRichChars rt
         toRCs atom = error $ "resInit: unhandled Atom " ++ show atom
         toRCs' :: Edit Atom -> [RichChar]
         toRCs' (Insert _ a) = toRCs a
         toRCs' (Delete _ _) = []
         toRCs' (Replace _ _ a) = toRCs a
     newFontMetrics <- calcMetrics (model ^. fontMetrics) (concatMap toRCs' (concatMap Patch.toList (initDoc ^. patches)))
     pure (Just $ updateLayout $ let ld = (model ^. localDocument) & document .~ initDoc
                                                                   & inflightPatch .~ Nothing
                                                                   & forkedAt .~ (Seq.length (initDoc ^. patches) - 1)
                                                                   & pendingEdit .~ []
                                 in
                                       model & connectionId .~ Just conn
                                       & fontMetrics %~ (\old -> old `mappend` newFontMetrics)
                                       & localDocument .~ ld
                                       & currentFont .~ case findFontRight (flattenDocument ld) 0 of
                                                         Nothing -> model ^. currentFont
                                                         (Just f) -> f
          )

resAppendPatch sendWS withModel connId (patchId, newPatch) =
  withModel $ \model ->
   do model' <-  case () of
       () | model ^. connectionId == (Just connId) ->
            do putStrLn "resAppendPatch: Got own patch back"
               let model' = updateLayout $
                     model & (localDocument . document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                           & (localDocument . inflightPatch) .~ Nothing
                           & (localDocument . forkedAt) .~ patchId
               pure model' -- got our own patch back

          | otherwise              ->   -- FIXME: probably need to call transform on the patch in progress
              do let toRCs :: Atom -> [RichChar]
                     toRCs (Img {})      = []
                     toRCs (LineBreak)   = []
                     toRCs EOF           = []
                     toRCs (RC rc) = [rc]
                     toRCs (RT rt) = richTextToRichChars rt
                     toRCs' :: Edit Atom -> [RichChar]
                     toRCs' (Insert _ a) = toRCs a
                     toRCs' (Delete _ _) = []
                     toRCs' (Replace _ _ a) = toRCs a
                 putStrLn $ "resAppendPatch: Got someone elses patch"
                 newFontMetrics <- calcMetrics (model ^. fontMetrics) (concatMap toRCs' (Patch.toList newPatch))
                 let model' = updateLayout (model & fontMetrics %~ (\old -> old `mappend` newFontMetrics)
                                                  & (localDocument . document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                                                  & (localDocument . forkedAt) .~ patchId
                                           )
                     newCaret = if maxEditPos newPatch < (model ^. caret)
                                  then (model ^. caret) + patchDelta (newPatch)
                                  else (model ^. caret)
                 pure (model' & caret .~ newCaret)
      Just <$> sendPatch sendWS model'


foreign import javascript unsafe  "console[\"log\"]($1)" consoleLog :: JS.JSString -> IO ()

foreign import javascript unsafe "function(buf){ if (!buf) { throw \"checkArrayBuffer: !buf\"; }; if (!(buf instanceof ArrayBuffer)) { throw \"checkArrayBuffer: buf is not an ArrayBuffer\"; }}($1)" checkArrayBuffer :: MutableArrayBuffer -> IO ()

foreign import javascript unsafe
   "new ArrayBuffer($1)" js_create :: Int -> IO MutableArrayBuffer

create :: Int -> IO MutableArrayBuffer
create n = js_create n
{-# INLINE create #-}

logMessage :: MessageEvent -> IO ()
logMessage messageEvent =
  case MessageEvent.getData messageEvent of
    (StringData str)    -> consoleLog str
    (ArrayBufferData r) -> do consoleLog "Got ArrayBufferData"
                              marray <- ArrayBuffer.thaw r
                              let jsval = (pToJSVal marray)
                                  buf   = createFromArrayBuffer r :: Buffer.Buffer
                              ab <- create 10
                              checkArrayBuffer marray
                              consoleLog ("checkArrayBuffer passed.")
                              consoleLog (JS.pack (show (isUndefined jsval)))
                              consoleLog (JS.pack (show (isNull jsval)))
                              consoleLog (JS.pack (show (toByteString 0 Nothing buf)))
                              -- -- (show ((decodeStrict (toByteString 0 Nothing (createFromArrayBuffer r))) :: Maybe WebSocketRes)))

handleMessage :: (WebSocketReq -> IO ()) -> MessageEvent -> WithModel Model -> IO ()
handleMessage sendWS messageEvent withModel =
  do logMessage messageEvent
     case decodeRes messageEvent of
       Nothing ->
         do putStrLn "handleMessage: unable to decode response"
            pure ()
       (Just res) ->
         case res of
           (ResAppendPatch connId (i, path)) -> resAppendPatch sendWS withModel connId (i, path)
           (ResInit connId doc) -> resInit withModel connId doc

decodeRes :: MessageEvent -> (Maybe WebSocketRes)
decodeRes messageEvent =
  case MessageEvent.getData messageEvent of
    (StringData str) -> decodeStrict (CS.pack (JS.unpack str))

