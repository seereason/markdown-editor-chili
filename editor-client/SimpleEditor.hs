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
module Main where

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
import Control.Lens ((^.), (.~), (?~), (&), (%~), (^?), _Just, set, _2)
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
import Data.Maybe (fromJust, fromMaybe, maybe, catMaybes)
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

-- import Servant.API ()
-- import Servant.Isomaniac (HasIsomaniac, MUV(..), ReqAction(..), isomaniac, muv, muvWS, runIdent)
-- import Servant.Common.BaseUrl
-- import Servant.Common.Req (Req(..))
import Language.Haskell.HSX.QQ (hsx)
import Web.Editor.API
-- import Web.ISO.HSX
-- import Web.ISO.Types hiding (Context2D(Font))
import System.IO.Unsafe (unsafePerformIO)

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
  , _bolding       :: Bool
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
  , _maxWidth      :: Double
  , _selectionData :: Maybe SelectionData
  , _currentRange  :: Maybe Range
  , _documentRange :: Maybe (Int, Int)
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
      Box { _boxWidth   = sum (map _boxWidth boxes)
          , _boxHeight  = maximum (map _boxHeight boxes)
          , _boxLineBreak = True
          , _boxContent = boxes
          }

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
layoutBoxes :: Double -> (Double, [Box c a]) -> [[Box c a]]
layoutBoxes maxWidth (currWidth, []) = []
layoutBoxes maxWidth (currWidth, (box:boxes))
  -- handle case were the box starts with a linebreak
  | box ^. boxLineBreak =
      [] : layoutBoxes maxWidth (0, (box & boxLineBreak .~ False):boxes)
  | currWidth + (box ^. boxWidth) <= maxWidth =
      case layoutBoxes maxWidth ((currWidth + (box ^. boxWidth)), boxes) of
       [] -> [[box]] -- this is the last box
       (line:lines) -> (box:line):lines
  -- if a box is longer than the maxWidth we will place it at the start of a line and let it overflow
  | (currWidth == 0) && (box ^. boxWidth > maxWidth) =
      [box] : layoutBoxes maxWidth (0, boxes)
  | otherwise =
      ([]:layoutBoxes maxWidth (0, box:boxes))

-- |
-- FIXME: should probably use a fold or something
boxify :: FontMetrics
       -> Vector Atom
       -> [AtomBox]
boxify fm v
  | Vector.null v = []
  | otherwise =
    case (Vector.head v) of
      RC {} ->
        case Vector.span isRichChar v of
          (richChars, rest) ->
            (map (textToBox fm) $ map richCharsToRichText $ richCharsToWords $ (Vector.toList (Vector.map unRichChar richChars))) ++ (boxify fm rest)
      atom@(Img img) ->
        (Box (img ^. imageWidth) (img ^. imageHeight) False atom) : boxify fm (Vector.tail v)
      LineBreak ->
        (Box 0 16 True LineBreak : boxify fm (Vector.tail v)) -- FIXME: height should probably be the height of a space char?
      Item ->
        let atomBox = (textToBox fm bullet) { _boxLineBreak = True
                                            , _boxContent = Item
                                            }
        in (atomBox : boxify fm (Vector.tail v))
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
  let newEdit =  (model ^. localDocument ^. pendingEdit) ++ [Insert (model ^. index) atom] -- (if c == '\r' then RichChar '\n' else RichChar c)]
  in
     updateLayout $ model { _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
                          , _caret       = succ (model ^. caret)
                          }

backspaceChar :: Model -> Model
backspaceChar model
 | (model ^. index) > 0 = -- FIXME: how do we prevent over deletion?
  let index'  = pred (model ^. index)
      c       = (flattenDocument $ model ^. localDocument) ! index'
      newEdit = (model ^. localDocument ^. pendingEdit) ++ [Delete index' c]
  in
     updateLayout $
       model {  _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
             , _index       = pred (model ^. index)
             , _caret       = pred (model ^. caret)
             }
 | otherwise = model


moveCaret :: Int -> Model -> Model
moveCaret i model =
         updateLayout $ model { _index = i
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
  else pure model

handleAction :: (WebSocketReq -> IO ()) -> EditAction -> Model -> IO (Maybe Model)
handleAction sendWS editAction model =
  do now <- getPOSIXTime
     let newEditState = toEditState editAction
         b = isNewPatchCondition (model ^. localDocument ^. inflightPatch) (model ^. editState) newEditState
         model''' = model & editState .~ newEditState
                          & lastActivity .~ now
     model'  <- if b then sendPatch sendWS model''' else pure model'''
     model'' <- applyEdit model' editAction
     pure (Just model'')
       where
         toEditState :: EditAction -> EditState
         toEditState (InsertAtom {}) = Inserting
         toEditState (DeleteAtom {}) = Deleting
         toEditState (MoveCaret  {}) = MovingCaret

         isNewPatchCondition :: Maybe (Patch Atom) -> EditState -> EditState -> Bool
--         isNewPatchCondition (Just _) _            _            = False -- a patch is already inflight, so we can just continue our pendingEdit
         isNewPatchCondition _  oldEditState newEditState = not (oldEditState == newEditState)


                    {- case model ^. localDocument ^. inflightPatch of
                             Nothing ->
                               model { _localDocument = (model ^. localDocument) & inflightPatch .~ Just newPatch
                                                                                 & pendingEdit   .~ []
                                     , _index = model ^. caret
                                     }
                             (Just {}) ->
                               model { _localDocument = (model ^. localDocument) & inflightPatch .~ Just newPatch
                                                                                 & pendingEdit   .~ []
                                     , _index = model ^. caret
                                     }

              sendWS (WebSocketReq (ReqAddPatch (model' ^. localDocument ^. forkedAt) newPatch))
-}
--              pure model'

         applyEdit :: Model -> EditAction -> IO Model
         applyEdit model editAction =
           case editAction of
             (InsertAtom c) -> pure $ insertChar c model
             DeleteAtom     -> pure $ backspaceChar model
             (MoveCaret i)  -> pure $ moveCaret i model
{-
handleAction :: (WebSocketReq -> IO ()) -> EditAction -> Model -> IO (Model, Maybe [WebSocketReq])
handleAction sendWS ea model
  | model ^. editState == Inserting =
      case ea of
       InsertAtom c -> pure $ insertChar c model
       DeleteAtom ->
         let newPatch   = Patch.fromList (model ^. localDocument ^. pendingEdit)
             model'     = model { _localDocument    = (model ^. localDocument) & inflightPatch .~ Just newPatch
                                                                     & pendingEdit   .~ []
                                , _editState   = Deleting
                                , _index       = model ^. caret
                                }
         in do (model'', mwsq) <- handleAction sendWS DeleteAtom model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. localDocument ^. forkedAt) newPatch))
               pure (model'', Nothing)
       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. localDocument ^. pendingEdit)
             model'     =  model { _localDocument    = (model ^. localDocument) & inflightPatch .~ Just newPatch
                                                                      & pendingEdit   .~ []
                                  , _editState   = MovingCaret
                                  }
         in do (model'', mwsq) <- handleAction sendWS ea model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. localDocument ^. forkedAt) newPatch))
               pure (model'', Nothing)

  | model ^. editState == Deleting =
      case ea of
       DeleteAtom -> pure $ backspaceChar model
       InsertAtom _ ->
         let newPatch   = Patch.fromList (model ^. localDocument ^. pendingEdit)
             model' = model & (localDocument . inflightPatch) .~ Just newPatch
                            & (localDocument . pendingEdit) .~ []
                            & editState .~ Inserting
         in do (model'', mwsq) <- handleAction sendWS ea model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. localDocument ^. forkedAt) newPatch))
               pure (model'', Nothing)

       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. localDocument ^. pendingEdit)
             model'     =  model { _localDocument    = (model ^. localDocument) & inflightPatch .~ Just newPatch
                                                                      & pendingEdit   .~ []
                                  , _editState   = MovingCaret
                                  }
         in do (model'', mwsq) <- handleAction sendWS ea model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. localDocument ^. forkedAt) newPatch))
               pure (model'', Nothing)

  | model ^. editState == MovingCaret =
      case ea of
       MoveCaret i -> pure $
         (updateLayout $ model { _index = i
                               , _caret = i
                               }, Nothing)
       InsertAtom {} ->
         handleAction sendWS ea (model { _editState = Inserting })
       DeleteAtom {} ->
         handleAction sendWS ea (model { _editState = Deleting })
-}
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
    go [] _ _ = Nothing
    go (hbox:hboxes) y n =
      if y < hbox ^. boxHeight
      then Just n
      else go hboxes (y - hbox ^. boxHeight) (succ n)

-- If we put Characters in boxes then we could perhaps generalize this
--
-- (index relative to start, index relative to hbox it is in)
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
    go [] x i = (i, 0)
    go (box:boxes) x i =
      case box ^. boxContent of
        (RT txt)
         | x < (box ^. boxWidth) ->
            indexAtX' (richTextToRichChars txt) x (i, 0)
         | otherwise -> go boxes (x - box ^. boxWidth) (i + richTextLength txt)
        (Img img)
         | x < (box ^. boxWidth) -> (i, i)
         | otherwise ->  go boxes (x - box ^. boxWidth) (i + 1)
        LineBreak
         -> go boxes x (i + 1)
        Item
         | x < (box ^. boxWidth) -> (i, i)
         | otherwise -> go boxes (x - box ^. boxWidth) (i + 1)


getAtomNode :: Int -> [AtomBox] -> JSNode -> Word -> IO (Maybe JSNode)
getAtomNode 0 [] parent childNum = error $ "getAtomNode 0 []" -- pure Nothing
getAtomNode 0 _ {- (atom:atoms) -} parent childNum =
  do putStrLn $ "getAtomNode childNum=" ++ show childNum
     children <- childNodes parent
     item children childNum
getAtomNode n a@(atom:atoms) parent childNum =
  case atomLength (atom ^. boxContent) of
    n' | n > n' -> getAtomNode (n - n') atoms parent (childNum + (atomNumNodes atom))
{-
       | n == n' ->
           do putStrLn "getAtomNode n == n'"
              pure Nothing
-}
       | otherwise ->
           case atom ^. boxContent of
             (RT (RichText (txt:txts))) ->
               if Text.length (snd txt) >= n
               then getAtomNode 0 a parent childNum
--               else getAtomNode 0 a parent childNum
               else do putStrLn $ "getAtomNode length txt = " ++ show (Text.length (snd txt)) ++ " , n = " ++ show n ++ " txt = " ++ show txt
                       getAtomNode (n - (Text.length (snd txt))) ((atom & boxContent .~ (RT (RichText txts))):[]) parent (childNum + 1)

{-
           do putStrLn $ "getAtomNode childNum=" ++ show childNum
              children <- childNodes parent
              item children childNum
-}
{-
           case atom ^. boxContent of
             (RT (RichText (txt:txts))) ->
               if length txt > n
               then getAtomNode 0 a parent childNum
               else getAtomNode (n - (length txt)) ((atom & boxContent .~ (RT (RichText txts))):[]) parent (childNum + 1)
-}
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


-}
getHBoxNode :: Int -> [HBox [AtomBox]] -> JSNode -> Word -> IO (Maybe JSNode)
getHBoxNode n [] _ _ = error $ "getHBoxNode: looking for n="++show n ++ " but the document is now []"
{-
Here we want the 0th element. So we just get the childNum of the parent.
-}
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
    n' | n > n' ->
           do putStrLn $ "getHBoxNode n = " ++ show n ++ " n' = " ++ show n'
              getHBoxNode (n - n') hboxes parent (childNum + 1)
       | n == n' ->
           do putStrLn "getHBoxNode n==n'"
              children <- childNodes parent
              mchild <- item children childNum
              case mchild of
                Nothing      -> error $ "getHBoxNode: mchild is Nothing. Was looking for childNum =" ++ show childNum -- pure Nothing
                (Just child) -> getAtomNode n (hbox ^. boxContent) child 0
       | otherwise ->
           do children <- childNodes parent
              mchild <- item children childNum
              case mchild of
                Nothing      -> error $ "getHBoxNode: mchild is Nothing. Was looking for childNum =" ++ show childNum -- pure Nothing
                (Just child) -> getAtomNode n (hbox ^. boxContent) child 0
{-
getVBoxNode :: Int -> [VBox [HBox [AtomBox]]] -> JSNode -> Word -> IO (Maybe JSNode)
getVBoxNode n [] _ _ = pure Nothing
getVBoxNode 0 (vbox:vboxes) parent childNum =
  do children <- childNodes parent
     mchild <- item children childNum
     case mchild of
       Nothing -> pure Nothing
       (Just child) -> getHBoxNode 0 (vbox ^. boxContent) child 0
getVBoxNode n (vbox:vboxes) parent childNum =
  case vboxLength vbox of
    n' | n >= n' -> getVBoxNode (n - n') vboxes parent (childNum + 1)
       | otherwise ->
           do children <- childNodes parent
              mchild <- item children childNum
              case mchild of
                Nothing -> pure Nothing
                (Just child) -> getHBoxNode n (vbox ^. boxContent) child 0
-}
{-
nodeAtIndex :: Int -> VBox [HBox [AtomBox]] -> JSNode -> Maybe JSNode
nodeAtIndex 0 layout parentNode =
  case layout of
    ((Box _ _ _ hbox):_) ->
      case hbox of
        (Box _ _ _ atoms) ->
          case atoms of
            (atom:_) ->
              case atom of
                (RC RichChar) -> 
-}

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
indexAtPos :: FontMetrics           -- ^ font metrics for characters in this document
           -> VBox [HBox [AtomBox]] -- ^ current layout of the document
           -> (Double, Double)      -- ^ (x,y) coordinates relative to the top-left of the editor div
           -> Maybe (Int, Int)             -- ^ index of atom if a match was found
indexAtPos fm vbox (x,y) =
  case lineAtY vbox y of
   Nothing -> Nothing
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
     (Just measureElem) <- getElementById doc "measureElement"
     metrics <- mapM (calcMetric measureElem) (nub rcs)
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
    (RT (RichText txts)) -> map renderText txts
    (Img img)            -> [[hsx|<img src=(img ^. imageUrl) />|]]
    LineBreak            -> [[hsx|<span style="display:inline-block;"></span>|]]
    Item                 -> let (RichText txts) = bullet in map renderText txts
  where
    renderText :: (Font, Text) -> Html Model
    renderText (font, txt) = [hsx| <span style=(fontToStyle font)><% nbsp txt %></span>   |]
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
    </div>
  |]
{-
  where
    rlines :: String -> [String]
    rlines l =
      let (b, a) = break (\c -> c == '\n' || c == '\r') l
      in case a of
       [] -> [b]
       [c] -> b : [""]
       (_:cs) -> b : rlines cs
-}
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
      | richTextLength rt < index -> Nothing
      | otherwise ->
        Just $ foldr sumWidth 0 (take index (richTextToRichChars rt))
    (Img img) -> Just (img ^. imageWidth) -- box ^. boxWidth
    LineBreak -> Just 0 -- FIXME: do we have to account for the fact that we should be at a newline now?
    Item      -> Just (box ^. boxWidth)
  where
    sumWidth c acc =
      case Map.lookup c fm of
        Just (w, _) -> acc + w
        Nothing -> acc

-- | given an index, calculate its (left, top, height) coordinates in the editor
--
indexToPos :: Int
           -> Model
           -> Maybe (Double, Double, Double)
indexToPos i model = go (model ^. layout ^. boxContent) i (0,0,16) -- FIMXE: maybe shouldn't hardcode to 16
  where
    -- go over the lines
    go [] _ _  = Nothing
    go (hbox:hboxes) i curPos =
      -- walk over the current line
      case go' (hbox ^. boxContent) (hbox ^. boxHeight) i curPos of
       -- if the position is in current line, we are done
       (Right curPos') -> curPos'
       -- otherwise add the height of that line and start
       -- looking in the next line
       (Left (i', (x,y,height))) ->
         go hboxes i' (0, y + hbox ^. boxHeight, height)

    -- go over the atoms in a line
--     go' :: [AtomBox] -> Int -> Double -> Either (Int, Double) (Maybe Double)
    go' [] _ i curPos = Left (i, curPos)
    go' _ _ 0 curPos = Right (Just curPos)
    go' (box:boxes) lineHeight i (x,y,height) =
      -- if the index is greater than the length of the next atom
      if i > atomLength (box ^. boxContent)
         -- subtract length of next atom, add width, update height, check next atom
         then go' boxes lineHeight (i - atomLength (box ^. boxContent)) (x + box ^. boxWidth, y, box ^. boxHeight)
         -- if we found the atom we are looking for, look for x position within the atom
         else case indexToPosAtom i (model ^. fontMetrics) box of
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

clickEditor :: (WebSocketReq -> IO ()) -> MouseEventObject -> WithModel Model -> IO ()
clickEditor sendWS e withModel = withModel $ \model'' ->
  do model <- updateEditorPos model''
     let elem = target e
     targetRect <- getBoundingClientRect elem
     let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
         mIndex = fst <$> indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
         model' = model & mousePos  ?~ (clientX e, clientY e)
                        & caret     .~ (fromMaybe (model ^. caret) mIndex)
                        & targetPos .~ (Just targetRect)
                        & debugMsg  .~ Just (Text.pack (show (model ^. layout)))
     case mIndex of
       (Just i) -> handleAction sendWS (MoveCaret i) model'
       Nothing  -> pure $ Just $ model' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click.")

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
                        & debugMsg  .~ Just (Text.pack (show (model ^. layout)))
     case mIndex of
       (Just i) -> handleAction sendWS (MoveCaret i) model'
       Nothing  -> pure $ Just $ model' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click.")

updateSelection' :: (WebSocketReq -> IO ()) -> MouseEventObject -> Model -> IO (Maybe Model)
updateSelection' sendWS e model'' =
  do model <- updateEditorPos model''
     let elem = target e
     targetRect <- getBoundingClientRect elem
     let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
         mIndex = indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
         model' = model & mousePos  ?~ (clientX e, clientY e)
                        & caret     .~ (fromMaybe (model ^. caret) (fst <$> mIndex))
                        & targetPos .~ (Just targetRect)
                        & debugMsg  .~ Just (Text.pack (show (model ^. layout)))

     case mIndex of
       (Just (i, si)) ->
         do case model ^. currentRange of
              Nothing -> pure Nothing
              (Just r) ->
                do putStrLn $ "update selection to index="++ show i ++ ", subindex=" ++ show si

                   (Just doc) <- currentDocument
                   (Just editorNode) <- getElementById  doc "editor-layout"
                   print "editor-layout"
                   nt <- nodeType editorNode
                   putStrLn $ nodeTypeString nt
                   nn <- nodeName editorNode
                   putStrLn $ JS.unpack nn
                   (Just lines) <- getFirstChild editorNode
                   mSelNode <- getHBoxNode i (model ^. layout ^. boxContent) (toJSNode lines) 0
                   model'' <- case mSelNode of
                     Nothing -> do print "error: updateSelection': could not find selected node"
                                   pure model'
                     (Just selNode) ->
                       do nt <- nodeType selNode
                          putStrLn $ nodeTypeString nt
                          nn <- nodeName selNode
                          putStrLn $ JS.unpack nn
                          (Just textNode) <- getFirstChild selNode
                          nt <- nodeType textNode
                          putStrLn $ nodeTypeString nt
                          nn <- nodeName textNode
                          putStrLn $ JS.unpack nn
                          jstr <- nodeValue textNode
                          putStrLn $ "nodeValue = " ++ JS.unpack jstr
                          case model ^. documentRange of
                            Nothing -> pure ()
                            (Just (b,e))
                              | i > b ->
                                setEnd r (toJSNode textNode) si
                              | i <= b ->
                                setStart r (toJSNode textNode) si
                          pure model'

{-
                   nt <- nodeType elem
                   putStrLn $ nodeTypeString nt
                   nn <- nodeName elem
                   putStrLn $ JS.unpack nn
                   (Just textNode) <- getFirstChild elem
                   nt <- nodeType textNode
                   putStrLn $ nodeTypeString nt
                   nn <- nodeName textNode
                   putStrLn $ JS.unpack nn
                   setEnd r (toJSNode elem) 0
-}
                   handleAction sendWS (MoveCaret i) (model'' & documentRange %~ (\dr -> fmap (\(b,e) -> (min b i, max e i)) dr))
       Nothing  -> pure $ Just $ model'' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click.")

{-
The object that emits the MouseEvent is typically something like a
span. But we want to be able to select subtext, so we need to know the
offset into that span.
-}

startSelection :: (WebSocketReq -> IO ()) -> MouseEventObject -> Model -> IO (Maybe Model)
startSelection sendWS e model'' =
  do model <- updateEditorPos model''
     let elem = target e
     targetRect <- getBoundingClientRect elem
     let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
         mIndex = indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
         model' = model & mousePos  ?~ (clientX e, clientY e)
                        & caret     .~ (fromMaybe (model ^. caret) (fst <$> mIndex))
                        & targetPos .~ (Just targetRect)
                        & debugMsg  .~ Just (Text.pack (show (model ^. layout)))
     case mIndex of
       (Just (i, si)) ->
         do putStrLn $ "Selection started at index="++ show i ++ ", subindex=" ++ show si
            w     <- window
            sel   <- getSelection w
            range <- newRange
            removeAllRanges sel
{-
            nt <- nodeType elem
            putStrLn $ nodeTypeString nt
            nn <- nodeName elem
            putStrLn $ JS.unpack nn
-}
            (Just doc) <- currentDocument
            (Just editorNode) <- getElementById  doc "editor-layout"
            print "editor-layout"
            nt <- nodeType editorNode
            putStrLn $ nodeTypeString nt
            nn <- nodeName editorNode
            putStrLn $ JS.unpack nn
            (Just lines) <- getFirstChild editorNode
            mSelNode <- getHBoxNode i (model ^. layout ^. boxContent) (toJSNode lines) 0
            case mSelNode of
              Nothing -> print "error: startSelection: could not find selected node"
              (Just selNode) ->
                do nt <- nodeType selNode
                   putStrLn $ nodeTypeString nt
                   nn <- nodeName selNode
                   putStrLn $ JS.unpack nn
                   selNodeTxt <- getInnerHTML (JSElement (unJSNode selNode))
                   putStrLn $ JS.unpack selNodeTxt
                   mTextNode <- getFirstChild selNode
                   case mTextNode of
                     Nothing -> pure ()
                     (Just textNode) ->
                       do jstr <- nodeValue textNode
                          putStrLn $ "nodeValue = " ++ JS.unpack jstr
                          setStart range (toJSNode textNode) si
                          setEnd range (toJSNode textNode) si
            addRange sel range
            handleAction sendWS (MoveCaret i) (model' & currentRange .~ Just range
                                                      & documentRange .~ Just (i, i)
                                              )

       Nothing  -> pure $ Just $ model' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click.")



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
               do (Just m') <- startSelection sendWS mouseEventObject m
                  pure $ Just $ m' & mouseActivity .~ (MouseDowned mouseEventObject)
             _ -> pure Nothing
         MouseDowned oldMEO ->
           case mouseEV of
             MouseMove ->
               do (Just m') <- updateSelection' sendWS mouseEventObject m
                  pure (Just m')

             MouseUp ->
               do (Just m') <- buttonUp sendWS mouseEventObject m
                  pure $ Just $ m' & mouseActivity .~ MouseNoop

-- what if already bold? or a mix of bold and not bold?
boldRange :: (WebSocketReq -> IO ()) -> (Int, Int) -> Model -> IO (Maybe Model)
boldRange sendWS (b, e) model' =
  do model <- sendPatch sendWS model'
     let flattened = flattenDocument (model ^. localDocument)
         sub = Vector.slice b (e - b) flattened
         atoms = zip (Vector.toList sub) [b..e]
         newFontWeight = if allBold sub then FW400 else FW700
         newEdit = catMaybes (map (\(a, i) ->
                             case a of
                               (RC (RichChar f char)) -> Just $ Replace i a (RC (RichChar (f & fontWeight .~  newFontWeight) char))
                               _ -> Nothing
                           ) atoms)
     print flattened
     print sub
     print atoms
     print newEdit
     pure $ Just $ updateLayout $ model { _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
                                        , _caret       = succ (model ^. caret)
                                        }
       where
         isBold :: Atom -> Bool
         isBold atom =
           case atom of
             (RC (RichChar (Common.Font fw _ _) _)) -> fw == FW700
             _ -> True
         allBold :: Vector Atom -> Bool
         allBold atoms = F.foldr (\atom b -> isBold atom && b) True atoms


extractRange :: (Int, Int) -> LocalDocument -> Vector Atom
extractRange (b, e) localDocument =
  let flattened = flattenDocument localDocument
  in Vector.slice b (e - b) flattened


{-
      atoms = zip (Vector.toList sub) [b..e]
      newEdit = catMaybes (map (\(a, i) ->
                             case a of
                               (RC (RichChar f char)) -> Just $ Replace i a (RC (RichChar (f & fontWeight .~  FW700) char))
                               _ -> Nothing
                           ) atoms)
     print flattened
     print sub
     print atoms
     print newEdit
     pure $ Just $ updateLayout $ model { _localDocument = (model ^. localDocument) & pendingEdit .~ newEdit
                                        , _caret       = succ (model ^. caret)
                                        }
-}

boldChange :: (WebSocketReq -> IO ()) -> MouseEventObject -> WithModel Model -> IO ()
boldChange sendWS ev withModel = withModel $ \model->
  do preventDefault ev
     refocus model
     case model ^. documentRange of
       Nothing -> pure $ Just $ (model & (currentFont . fontWeight) %~ (\w -> if w == FW400 then FW700 else FW400)
                                       & bolding .~ (not (model ^. bolding))
                                       & debugMsg .~ Just "BoldChange")
       (Just (b,e)) ->
         do stopPropagation ev
            boldRange sendWS (b,e) model

italicChange :: MouseEventObject -> WithModel Model -> IO ()
italicChange e withModel =  withModel $ \model->
  do preventDefault e
     pure $ Just $ (model & (currentFont . fontStyle) %~ (\fs -> if fs == Normal then Italic else Normal) & debugMsg .~ Just "ItalicChange")

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

updateSelection :: SelectionEventObject -> WithModel Model -> IO ()
updateSelection e withModel =
  withModel $ \model -> do
--    js_alert "updateSelection"
    sel <- getSelectionData model
    pure $ Just $ model & selectionData .~ sel

app :: (WebSocketReq -> IO ()) -> Model -> Html Model
app sendWS model =
  let setFontSize s = EL Click (\e withModel -> withModel $ \m -> pure $ Just $ m & (currentFont . fontSize) .~ s)
  in
         ([hsx|
           <div>
             <% if False
                 then <div style="position: absolute; left: 800px; width: 800px;">
                             <h1>Debug</h1>
                             <p>userId: <% show (model ^. userId) %></p>
                             <p>debugMsg: <% show (model ^. debugMsg) %></p>
                             <p>LocalDocument: <% show (model ^. localDocument) %></p>
--                             <p>Patches:  <% show (model  ^. patches) %></p>
                             <p>Index: <% show (model ^. index) %></p>
                             <p>Caret: <% show (model ^. caret) %></p>
                             <p>mousePos: <% show (model ^. mousePos) %></p>
                             <p>editorPos: <% let mpos = model ^. editorPos in
                                              case mpos of
                                                 Nothing -> "(,)"
                                                 (Just pos) -> show (rectLeft pos, rectTop pos) %></p>
                             <p><% let mepos = model ^. editorPos in
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
                                <% case model ^. selectionData of
                                     Nothing -> <p>No Selection</p>
                                     (Just selectionData) ->
                                       <div>
                                        <p>Selection: count=<% show $ selectionData ^. rangeCount %></p>
                                        <p>Selection: string len=<% show $ length $ selectionData ^. selectionString %></p>
                                        <p>Selection: toString()=<% selectionData ^. selectionString %></p>
                                       </div>
                                %>
                             <p>Selection: documentRange=<% show $ model ^. documentRange %></p>

                             <p>Current Font <% show (model ^. currentFont) %></p>
                             <p>Font Metrics <% show (model ^. fontMetrics) %></p>


                      </div>
                 else <span></span> %>
            <h1>Super Awesome Editor</h1>
            <div class="form-line editor-toolbar row">
            <div class="col-md-6">
              <div class="btn-group" data-toggle="buttons">
               <label class=(if (model ^. bolding) then ("btn btn-default active" :: Text) else ("btn btn-default" :: Text))  [ EL Click (boldChange sendWS) ] >
                <input type="checkbox" autocomplete="off" style="font-weight: 800;"/>B</label>
               <label class="btn btn-default" [ EL Click italicChange ] >
                <input type="checkbox" autocomplete="off" style="font-style: italic;" />I</label>
               <label class="btn btn-default" [ EL Click (itemize sendWS) ] >
                <input type="checkbox" autocomplete="off" />•</label>
              </div>
             </div>
            </div>
            <div id="editor" tabindex="1" style="outline: 0; line-height: 1.0; height: 600px; width: 500px; border: 1px solid black; box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.2);" [ EL KeyPress (keyPressed sendWS)
                             , EL KeyDown  (keyDowned sendWS)
                             , EL MouseDown (selectEditor sendWS MouseDown)
                             , EL MouseUp   (selectEditor sendWS MouseUp)
                             , EL MouseMove (selectEditor sendWS MouseMove)
--                             , EL Click (selectEditor sendWS Click)
--                             , EL Click    (clickEditor sendWS)
                             , EL Copy     editorCopy
                             , OnCreate (\el _ -> focus el)
                             ] >
            <div id="caret" class="editor-caret" (caretPos model (indexToPos (model ^. caret) model))></div>
               <% renderLayout (model ^. layout) %>
            </div>

           </div>
          |])

chili app initAction model url handleWS =
  do (Just doc)   <- currentDocument
     (Just murv) <- getElementById doc "murv"
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
  , _bolding       = False
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
  , _maxWidth      = 300
  , _selectionData = Nothing
  , _currentRange  = Nothing
  , _documentRange = Nothing
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
         toRCs Item          = richTextToRichChars bullet
         toRCs (RC rc) = [rc]
         toRCs (RT rt) = richTextToRichChars rt
         toRCs atom = error $ "resInit: unhandled Atom " ++ show atom
         toRCs' :: Edit Atom -> [RichChar]
         toRCs' (Insert _ a) = toRCs a
         toRCs' (Delete _ _) = []
         toRCs' (Replace _ _ a) = toRCs a
     newFontMetrics <- calcMetrics (model ^. fontMetrics) (concatMap toRCs' (concatMap Patch.toList (initDoc ^. patches)))
     pure (Just $ updateLayout $ model & connectionId .~ Just conn
                                       & fontMetrics %~ (\old -> old `mappend` newFontMetrics)
                                       & (localDocument . document) .~ initDoc
                                       & (localDocument . inflightPatch) .~ Nothing
                                       & (localDocument . forkedAt) .~ (Seq.length (initDoc ^. patches) - 1)
                                       & (localDocument . pendingEdit) .~ [])

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

{-
  | model ^. connectionId == (Just connId) = pure Map.empty
  | otherwise              = let toRCs :: Atom -> [RichChar]
                                 toRCs (Img {})      = []
                                 toRCs (LineBreak)   = []
                                 toRCs (RC rc) = [rc]
                                 toRCs (RT rt) = richTextToRichChars rt
                                 toRCs' :: Edit Atom -> [RichChar]
                                 toRCs' (Insert _ a) = toRCs a
                                 toRCs' (Delete _ _) = []
                                 toRCs' (Replace _ _ a) = toRCs a
                             in calcMetrics (model ^. fontMetrics) (concatMap toRCs' (Patch.toList patch))
-}
{-
incomingWS :: (MessageEvent -> Maybe action) -> TQueue action -> MessageEvent -> IO ()
incomingWS decodeAction queue messageEvent =
  do logMessage messageEvent
     case decodeAction messageEvent of
       Nothing -> consoleLog "Failed to decode messageEvent"
       (Just action) -> atomically $ writeTQueue queue action
-}

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
{-
        Nothing      -> Nothing
        (Just wsres) -> Ju
          case wsres of
            (ResAppendPatch connId (i, path)) ->
              resAppendPatch connId (i, path)
            _ -> pure ()
-}
{-
  = ResAppendPatch ConnectionId (Int, Patch Atom)
--  | ResUpdateCurrent ConnectionId [Edit Atom]
  | ResInit ConnectionId (Seq (Patch Atom))
-}

main :: IO ()
main =
  chili app initAction initModel "ws://localhost:8000/editor/websockets" handleMessage
--  muvWS editorMUV id "ws://localhost:8000/editor/websockets" decodeRes (Just Init)

{- JAS
update' :: Action
        -> Model
        -> IO (Model, Maybe [WebSocketReq])
update' action model'' = do
  now <- getPOSIXTime
  (Just doc)        <- currentDocument
  (Just editorElem) <- getElementById doc "editor"
  focus editorElem
  rect <- getBoundingClientRect editorElem
  let activity =
        case action of
          AddImage {}   -> True
          KeyPressed {} -> True
          PasteA {}     -> True
          KeyDowned {}  -> True
          MouseClick {} -> True
          _             -> False
      forceActivity =
        if ((not activity) && now >= (model ^. lastActivity) + 2)
        then True
        else False

      model = model'' { _editorPos     = Just rect
--                      , _selectionData = ioData ^. bSelectionData
                      , _lastActivity = if activity then now else (model'' ^. lastActivity)
                      }
  newFontMetrics <- case action of
                      (KeyPressed c) -> do
                        let rc = RichChar (model ^. currentFont) c
                        case Map.lookup rc (model ^. fontMetrics) of
                          Nothing ->
                            do (Just document) <- currentDocument
                               (Just measureElem) <- getElementById document "measureElement"
                               (_, metric) <- getFontMetric measureElem rc
                               pure (Map.singleton rc metric)
                          Just {} -> pure Map.empty
                      (KeyDowned c)
                        | c == 32 -> do
                          let rc = RichChar (model ^. currentFont) ' '
                          case Map.lookup rc (model ^. fontMetrics) of
                                Nothing ->
                                  do (Just document) <- currentDocument
                                     (Just measureElem) <- getElementById document "measureElement"
                                     (_, metric) <- getFontMetric measureElem rc
                                     pure (Map.singleton rc metric)
                                Just {} -> pure Map.empty
                        | otherwise -> pure Map.empty
                      (WSRes wsres) ->
                        case wsres of
                          (ResAppendPatch connId (_, patch))
                            | model ^. connectionId == (Just connId) -> pure Map.empty
                            | otherwise              -> let toRCs :: Atom -> [RichChar]
                                                            toRCs (Img {})      = []
                                                            toRCs (LineBreak)   = []
                                                            toRCs Item          = richTextToRichChars bullet
                                                            toRCs (RC rc) = [rc]
                                                            toRCs (RT rt) = richTextToRichChars rt
                                                            toRCs' :: Edit Atom -> [RichChar]
                                                            toRCs' (Insert _ a) = toRCs a
                                                            toRCs' (Delete _ _) = []
                                                            toRCs' (Replace _ _ a) = toRCs a
                                                        in calcMetrics (model ^. fontMetrics) (concatMap toRCs' (Patch.toList patch))
                          (ResInit connectionId patches) ->
                            let toRCs :: Atom -> [RichChar]
                                toRCs (Img {})      = []
                                toRCs (LineBreak)   = []
                                toRCs Item          = richTextToRichChars bullet
                                toRCs (RC rc) = [rc]
                                toRCs (RT rt) = richTextToRichChars rt
                                toRCs' :: Edit Atom -> [RichChar]
                                toRCs' (Insert _ a) = toRCs a
                                toRCs' (Delete _ _) = []
                                toRCs' (Replace _ _ a) = toRCs a
                            in calcMetrics (model ^. fontMetrics) (concatMap toRCs' (concatMap Patch.toList patches))

                      _ -> pure Map.empty
  case action of
      Init             -> do let rcs = nub $ richTextToRichChars bullet
                             newMetrics <- calcMetrics (model ^. fontMetrics) rcs
                             pure (model & fontMetrics  %~ (\old -> old `mappend` newMetrics), Just [WebSocketReq ReqInit])

      ActivityTimeout  -> if forceActivity
                             then do let (model', req) = handleAction (MoveCaret (model ^. caret)) model
                                     pure (model' & debugMsg .~ (Just $ Text.pack "Forced update."), req)
                             else pure (model & debugMsg .~ (Just $ Text.pack $ show $ now), Nothing)

      AddImage         ->  pure $ handleAction (InsertAtom (Img (Image "http://i.imgur.com/YFtU4OV.png" 174 168))) model
      Itemize e        -> case model ^. itemizing of
                           False -> pure $ handleAction (InsertAtom Item) (model & itemizing .~ True)
                           True  -> pure $ (model & itemizing .~ False, Nothing)
      IncreaseFontSize ->  pure (model & (currentFont . fontSize) %~ succ, Nothing)
      DecreaseFontSize ->  pure (model & (currentFont . fontSize) %~ (\fs -> if fs > 1.0 then pred fs else fs), Nothing)
      (SetFontSize s)  -> pure (model & (currentFont . fontSize) .~ s, Nothing)
--      ToggleBold       ->  (model & (currentFont . fontWeight) %~ (\w -> if w == FW400 then FW700 else FW400), Nothing)
      BoldChange e     -> pure (model & (currentFont . fontWeight) %~ (\w -> if w == FW400 then FW700 else FW400) & debugMsg .~ Just "BoldChange", Nothing)
      ItalicChange e   -> pure (model & (currentFont . fontStyle) %~ (\fs -> if fs == Normal then Italic else Normal) & debugMsg .~ Just "ItalicChange", Nothing)
      IncUserId        -> pure (model & userId %~ succ, Nothing)

{-
      CopyA ceo      ->  -- cd <- clipboardData ceo
                           -- setDataTransferData cd "text/plain" "Boo-yeah!"
                         (model & debugMsg .~ Just "copy", Nothing)

      PasteA txt ->  -- txt <- getDataTransferData dt "text/plain"
        --                       txt2 <- getClipboardData ceo
--                       js_alert txt2
                     (model & debugMsg .~ Just (textFromJSString txt), Nothing)
-}
      KeyPressed c  ->
        do let rc = RichChar (model ^. currentFont) c
               model' = case Map.lookup rc newFontMetrics of
                 Nothing -> model
                 (Just metric) -> set (fontMetrics . at rc) (Just metric) model
           pure $ handleAction (InsertAtom (RC rc)) model'

      -- used for handling special cases like backspace, space
      KeyDowned c -- handle \r and \n ?
        | c == 8    -> pure $ handleAction DeleteAtom model
        | c == 13   -> case model ^. itemizing of
                         False -> pure $ handleAction (InsertAtom LineBreak) model
                         True  -> pure $ handleAction (InsertAtom Item) model
        | c == 32   -> let rc = RichChar (model ^. currentFont) ' '
                           model' = case Map.lookup rc newFontMetrics of
                             Nothing -> model
                             (Just metric) -> set (fontMetrics . at rc) (Just metric) model
                       in pure $ handleAction (InsertAtom (RC rc)) model'
        | c == 37   -> pure $ handleAction (MoveCaret ((model ^. caret) - 1)) model -- left
        | c == 38   -> pure $ (model, Nothing)                                      -- up
        | c == 39   -> pure $ handleAction (MoveCaret ((model ^. caret) + 1)) model -- right
        | c == 40   -> pure $ (model, Nothing)                                      -- down
        | otherwise -> pure $ (model, Nothing)

      MouseClick e -> do
           let elem = target e
           targetRect <- getBoundingClientRect elem
           let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
               mIndex = indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
               model' = model & mousePos  ?~ (clientX e, clientY e)
                              & caret     .~ (fromMaybe (model ^. caret) mIndex)
                              & targetPos .~ (Just targetRect)
                              & debugMsg  .~ Just (Text.pack (show (model ^. layout)))
           case mIndex of
             (Just i) -> pure $ handleAction (MoveCaret i) model'
             Nothing  -> pure $ (model' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click."), Nothing)

      UpdateMetrics -> pure (model, Nothing)

      WSRes wsres ->
        case wsres of
          (ResAppendPatch connId (patchId, newPatch))
            | model ^. connectionId == (Just connId) ->
              let model' = updateLayout $
                             model & (document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                                   & (document . inflightPatch) .~ (Patch.fromList [])
                                   & (document . forkedAt) .~ patchId
              in pure (model', Nothing) -- got our own patch back

            | otherwise              -> -- FIXME: probably need to call transform on the patch in progress
                let model' = updateLayout (model & fontMetrics %~ (\old -> old `mappend` newFontMetrics)
                                                 & (document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                                                 & (document . forkedAt) .~ patchId
                                                 )
                    newCaret = if maxEditPos newPatch < (model ^. caret)
                               then (model ^. caret) + patchDelta (newPatch)
                               else (model ^. caret)
                in pure (model' & caret .~ newCaret, Nothing)
          (ResInit conn initPatches) -> pure $
            (updateLayout $ model & connectionId .~ Just conn
                                  & fontMetrics %~ (\old -> old `mappend` newFontMetrics)
                                  & (document . patches) .~ initPatches
                                  & (document . inflightPatch) .~ (Patch.fromList [])
                                  & (document . forkedAt) .~ (Seq.length initPatches - 1)
                                  & (document . pendingEdit) .~ [], Nothing)
-}

{-
view' :: Model -> (Html Action, [Canvas])
view' model =
  let keyDownEvent     = Event KeyDown  (\e -> when (keyCode e == 8 || keyCode e == 32 || keyCode e == 13 || (keyCode e >= 37 && keyCode e <= 40)) (preventDefault e) >> pure (KeyDowned (keyCode e)))
      keyPressEvent    = Event KeyPress (\e -> pure (KeyPressed (chr (charCode e))))
      clickEvent       = Event Click    (\e -> pure (MouseClick e))
      boldChange       = Event Click    (\e -> do preventDefault e ; pure (BoldChange e))
      italicChange     = Event Click    (\e -> do preventDefault e ; pure (ItalicChange e))
      copyEvent        = Event Copy     (\e -> do preventDefault e ; dt <- clipboardData e ; setDataTransferData dt "text/plain" "boo-yeah" ; pure (CopyA e))
      pasteEvent       = Event Paste    (\e -> do preventDefault e ; dt <- clipboardData e ; txt <- getDataTransferData dt "text/plain" ; pure (PasteA txt))
      addImage         = Event Click    (\e -> pure AddImage)
      increaseFontSize = Event Click    (\e -> pure IncreaseFontSize)
      decreaseFontSize = Event Click    (\e -> pure DecreaseFontSize)
      setFontSize n    = Event Click    (\e -> pure (SetFontSize n))
      incUserId        = Event Click    (\e -> pure IncUserId)
      itemize          = Event Click    (\e -> do preventDefault e ; pure (Itemize e))
  in
         ([hsx|
           <div>
--            <p><% show ( (model ^. layout)) %></p>

--            <p><% Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
--            <p><% show $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
--            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>
--            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>
             <% if True
                 then <div style="position: absolute; left: 800px; width: 800px;">
                             <h1>Debug</h1>
                             <p>userId: <% show (model ^. userId) %></p>
                             <p>debugMsg: <% show (model ^. debugMsg) %></p>
                             <p>Document: <% show (model ^. document) %></p>
--                             <p>Patches:  <% show (model  ^. patches) %></p>
                             <p>Index: <% show (model ^. index) %></p>
                             <p>Caret: <% show (model ^. caret) %></p>
                             <p>mousePos: <% show (model ^. mousePos) %></p>
                             <p>editorPos: <% let mpos = model ^. editorPos in
                                              case mpos of
                                                 Nothing -> "(,)"
                                                 (Just pos) -> show (rectLeft pos, rectTop pos) %></p>
                             <p><% let mepos = model ^. editorPos in
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
                                <% case model ^. selectionData of
                                     Nothing -> <p>No Selection</p>
                                     (Just selectionData) ->
                                       <div>
                                        <p>Selection: count=<% show $ selectionData ^. rangeCount %></p>
                                        <p>Selection: len=<% show $ length $ selectionData ^. selectionString %></p>
                                        <p>Selection: toString()=<% selectionData ^. selectionString %></p>
                                       </div>
                                %>
                             <p>Current Font <% show (model ^. currentFont) %></p>
                             <p>Font Metrics <% show (model ^. fontMetrics) %></p>


                      </div>
                 else <span></span> %>

            <h1>Editor</h1>
            <div class="form-line editor-toolbar row">
            {-
             <div class="col-md-2">
--              <button type="button" class="btn btn-default" [addImage]>Add Image</button>
--              <button type="button" class="btn btn-default" [increaseFontSize]>+</button>
--              <button type="button" class="btn btn-default" [decreaseFontSize]>-</button>
             </div>
-}
{-
             <div class="col-md-2">
              <div class="input-group">
               <div class="input-group-btn">
                <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"><span class="caret"></span></button>
                <ul class="dropdown-menu">
                 <li><a href="#" [setFontSize 10]>10</a></li>
                 <li><a href="#" [setFontSize 12]>12</a></li>
                 <li><a href="#" [setFontSize 14]>14</a></li>
                 <li><a href="#" [setFontSize 16]>16</a></li>
                 <li><a href="#" [setFontSize 18]>18</a></li>
                </ul>
               </div>
               <input type="text" class="form-control col-xs-1" aria-label="..." value=(Text.pack $ show $ model ^. currentFont ^. fontSize) />
              </div>
            </div>
              -}
            <div class="col-md-6">
--            <button [toggleBold]>Toggle Bold</button>
              <div class="btn-group" data-toggle="buttons">
               <label class="btn btn-default" [boldChange]>
                <input type="checkbox" autocomplete="off" style="font-weight: 800;" />B</label>
               <label class="btn btn-default" [italicChange]>
                <input type="checkbox" autocomplete="off" style="font-style: italic;" />I</label>
               <label class="btn btn-default" [itemize]>
                <input type="checkbox" autocomplete="off" />•</label>

--               <button type="button" class="btn btn-default" [itemize]>•</button>
              </div>
--              <button [incUserId]>UserId+</button>
             </div>
            </div>
            <div id="editor" tabindex="1" style="outline: 0; line-height: 1.0; height: 600px; width: 300px; border: 1px solid black; box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.2);" autofocus="autofocus" [keyDownEvent, keyPressEvent, clickEvent, copyEvent] >
               <div id="caret" class="editor-caret" (caretPos model (indexToPos (model ^. caret) model))></div>
               <% renderLayout (model ^. layout) %>
            </div>
           </div>
          |], [])
{-
editorMUV :: MUV IO Model Action [WebSocketReq]
editorMUV =
  MUV { model  = Model { _document      = Document { _patches = mempty
                                                   , _inflightPatch = Patch.fromList []
                                                   , _forkedAt = 0
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
                       , _maxWidth      = 300
                       , _selectionData = Nothing
                       , _userId        = UserId 0
                       , _lastActivity  = 0
                       }
--      , browserIO = browserIO'
      , update = update'
      , view   = view'
      }

decodeRes :: MessageEvent -> Maybe Action
decodeRes messageEvent =
  case MessageEvent.getData messageEvent of
    (StringData str) ->
      case decode (C.pack (JS.unpack str)) of
        Nothing      -> Nothing
        (Just wsres) -> Just (WSRes wsres)
-}
-}
