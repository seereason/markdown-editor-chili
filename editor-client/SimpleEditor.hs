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
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.UserId (UserId(..))
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
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

data Document = Document
  { _patches     :: Seq (Patch Atom) -- ^ expected to always be free of conflicts.
--  , _currentEdit :: [Edit Atom]
  , _inflightPatch :: Patch Atom
  , _forkedAt      :: Int
  , _pendingEdit   :: [Edit Atom]
  }
  deriving Show
makeLenses ''Document

flattenDocument :: Document -> Vector Atom
flattenDocument doc =
  let committedDoc = foldl' (\doc patch -> apply patch doc) mempty (doc ^. patches) -- conflict-free patches
      pendingPatch = Patch.fromList (doc ^. pendingEdit)
  in if applicable (doc ^. inflightPatch) committedDoc
     then let doc' = apply (doc ^. inflightPatch) committedDoc
          in if applicable pendingPatch doc'
             then apply pendingPatch doc'
             else doc' -- FIXME
     else committedDoc -- FIXME

-- could have a cache version of the Document with all the edits applied, then new edits would just mod that document
data Model = Model
  { _document      :: Document -- Vector Atom -- ^ all of the patches applied but not the _currentEdit
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
  , _userId        :: UserId
  , _lastActivity  :: POSIXTime
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
  atomsToLines (model ^. fontMetrics) (model ^. maxWidth) (flattenDocument $ model ^. document)

updateLayout :: Model -> Model
updateLayout m = m { _layout = calcLayout m }

insertChar :: Atom
           -> Model
           -> (Model, Maybe [WebSocketReq])
insertChar atom model =
  let newEdit =  (model ^. document ^. pendingEdit) ++ [Insert (model ^. index) atom] -- (if c == '\r' then RichChar '\n' else RichChar c)]
  in
     (updateLayout $ model { _document = (model ^. document) & pendingEdit .~ newEdit
         , _caret       = succ (model ^. caret)
         }, Nothing)

backspaceChar :: Model -> (Model, Maybe [WebSocketReq])
backspaceChar model
 | (model ^. index) > 0 = -- FIXME: how do we prevent over deletion?
  let index'  = pred (model ^. index)
      c       = (flattenDocument $ model ^. document) ! index'
      newEdit = (model ^. document ^. pendingEdit) ++ [Delete index' c]
  in
     (updateLayout $
       model {  _document = (model ^. document) & pendingEdit .~ newEdit
             , _index       = pred (model ^. index)
             , _caret       = pred (model ^. caret)
             }, Nothing)
 | otherwise = (model, Nothing)

handleAction :: (WebSocketReq -> IO ()) -> EditAction -> Model -> IO (Model, Maybe [WebSocketReq])
handleAction sendWS ea model
  | model ^. editState == Inserting =
      case ea of
       InsertAtom c -> pure $ insertChar c model
       DeleteAtom ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model'     = model { _document    = (model ^. document) & inflightPatch .~ newPatch
                                                                     & pendingEdit   .~ []
                                , _editState   = Deleting
                                , _index       = model ^. caret
                                }
         in do (model'', mwsq) <- handleAction sendWS DeleteAtom model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch))
               pure (model'', Nothing)
       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model'     =  model { _document    = (model ^. document) & inflightPatch .~ newPatch
                                                                      & pendingEdit   .~ []
                                  , _editState   = MovingCaret
                                  }
         in do (model'', mwsq) <- handleAction sendWS ea model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch))
               pure (model'', Nothing)

  | model ^. editState == Deleting =
      case ea of
       DeleteAtom -> pure $ backspaceChar model
       InsertAtom _ ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model' = model & (document . inflightPatch) .~ newPatch
                            & (document . pendingEdit) .~ []
                            & editState .~ Inserting
         in do (model'', mwsq) <- handleAction sendWS ea model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch))
               pure (model'', Nothing)

       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model'     =  model { _document    = (model ^. document) & inflightPatch .~ newPatch
                                                                      & pendingEdit   .~ []
                                  , _editState   = MovingCaret
                                  }
         in do (model'', mwsq) <- handleAction sendWS ea model'
               sendWS (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch))
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
indexAtX :: FontMetrics -> HBox [AtomBox] -> Double -> Int
indexAtX fm hbox x = go (hbox ^. boxContent) x 0
  where
    indexAtX' :: [RichChar] -> Double -> Int -> Int
    indexAtX' [] x i = i
    indexAtX' (c:cs) x i =
      let cWidth =  fst $ fromJust $ Map.lookup c fm
      in if x < cWidth
         then i
         else indexAtX' cs (x - cWidth) (succ i)
    go :: [AtomBox] -> Double -> Int -> Int
    go [] x i = i
    go (box:boxes) x i =
      case box ^. boxContent of
        (RT txt)
         | x < (box ^. boxWidth) ->
            indexAtX' (richTextToRichChars txt) x i
         | otherwise -> go boxes (x - box ^. boxWidth) (i + richTextLength txt)
        (Img img)
         | x < (box ^. boxWidth) -> i
         | otherwise ->  go boxes (x - box ^. boxWidth) (i + 1)
        LineBreak
         -> go boxes x (i + 1)
        Item
         | x < (box ^. boxWidth) -> i
         | otherwise -> go boxes (x - box ^. boxWidth) (i + 1)

-- | calculate the index to Atom in the document which is the hit
-- target of the provided (x,y) coordinates
indexAtPos :: FontMetrics           -- ^ font metrics for characters in this document
           -> VBox [HBox [AtomBox]] -- ^ current layout of the document
           -> (Double, Double)      -- ^ (x,y) coordinates relative to the top-left of the editor div
           -> Maybe Int             -- ^ index of atom if a match was found
indexAtPos fm vbox (x,y) =
  case lineAtY vbox y of
   Nothing -> Nothing
   (Just i) -> Just $ (sumPrevious $ take i (vbox ^. boxContent)) + indexAtX fm ((vbox ^. boxContent)!!i) x
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

getSelectionData :: IO (Maybe SelectionData)
getSelectionData =
  do w <- window
     sel <- getSelection w
--     js_alert =<< (selectionToString sel)
     c <- getRangeCount sel
     txt <- selectionToString sel
     pure $ Just $ SelectionData { _selection = sel
                                 , _selectionString = JS.unpack txt
                                 , _rangeCount = c
                                 }


{-
foreign import javascript unsafe "$1[\"clipboardData\"][\"getData\"](\"text/plain\")" getClipboardData ::
        ClipboardEventObject -> IO JS.JSString
-}

foreign import javascript unsafe "$1[\"focus\"]()" js_focus ::
        JSElement -> IO ()

foreign import javascript unsafe "window[\"setTimeout\"]($1, $2)" js_setTimeout ::
  Callback (IO ()) -> Int -> IO ()

-- | return a Map of any new metrics
calcMetrics :: FontMetrics -> [RichChar] -> IO (Map RichChar (Double, Double))
calcMetrics fm rcs =
  do (Just document)    <- currentDocument
     (Just measureElem) <- getElementById document "measureElement"
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
renderAtomBox :: AtomBox
              -> [Html Model]
-- renderTextBox box = CDATA True (box ^. boxContent)
renderAtomBox box =
  case box ^. boxContent of
    (RT (RichText txts)) -> map renderText txts
    (Img img)            -> [[hsx| <img src=(img ^. imageUrl) /> |]]
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
renderLayout :: VBox [HBox [AtomBox]]
          -> Html Model
renderLayout lines =
  [hsx|
    <div data-path="root">
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


keyPressed :: (WebSocketReq -> IO ()) -> KeyboardEventObject -> Model -> IO Model
keyPressed sendWS e model'' =
  do model <- updateEditorPos model''
     let c = chr (charCode e)
         rc = RichChar (model ^. currentFont) c
     putStrLn $ "KeyPressed "++[c]
     newFontMetrics <-
       case Map.lookup rc (model ^. fontMetrics) of
         Nothing ->
           do (Just document) <- currentDocument
              (Just measureElem) <- getElementById document "measureElement"
              (_, metric) <- getFontMetric measureElem rc
              pure (Map.singleton rc metric)
         Just {} -> pure Map.empty
     let model' = case Map.lookup rc newFontMetrics of
                      Nothing -> model
                      (Just metric) -> set (fontMetrics . at rc) (Just metric) model
     fst <$> handleAction sendWS (InsertAtom (RC rc)) model'

keyDowned :: (WebSocketReq -> IO ())
          -> KeyboardEventObject
          -> Model
          -> IO Model
keyDowned sendWS e model =
 do putStrLn $ "KeyDowned "++ (show $ keyCode e)
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
    fmap fst $ case () of
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
        | c == 38   -> pure $ (model, Nothing)                                      -- up
        | c == 39   -> handleAction sendWS (MoveCaret ((model ^. caret) + 1)) model -- right
        | c == 40   -> pure $ (model, Nothing)                                      -- down
        | otherwise -> pure $ (model, Nothing)

clickEditor :: (WebSocketReq -> IO ()) -> MouseEventObject -> Model -> IO Model
clickEditor sendWS e model'' =
  do model <- updateEditorPos model''
     let elem = target e
     targetRect <- getBoundingClientRect elem
     let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
         mIndex = indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
         model' = model & mousePos  ?~ (clientX e, clientY e)
                        & caret     .~ (fromMaybe (model ^. caret) mIndex)
                        & targetPos .~ (Just targetRect)
                        & debugMsg  .~ Just (Text.pack (show (model ^. layout)))
     case mIndex of
       (Just i) -> fst <$> handleAction sendWS (MoveCaret i) model'
       Nothing  -> pure $ fst $ (model' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click."), Nothing)

boldChange :: MouseEventObject -> Model -> IO Model
boldChange e model =
  do preventDefault e
     pure (model & (currentFont . fontWeight) %~ (\w -> if w == FW400 then FW700 else FW400) & debugMsg .~ Just "BoldChange")

italicChange :: MouseEventObject -> Model -> IO Model
italicChange e model =
  do preventDefault e
     pure $ fst $ (model & (currentFont . fontStyle) %~ (\fs -> if fs == Normal then Italic else Normal) & debugMsg .~ Just "ItalicChange", Nothing)

itemize :: (WebSocketReq -> IO ()) -> MouseEventObject -> Model -> IO Model
itemize sendWS e model =
  do preventDefault e
     case model ^. itemizing of
       False -> fst <$> handleAction sendWS (InsertAtom Item) (model & itemizing .~ True)
       True  -> pure $ fst $ (model & itemizing .~ False, Nothing)

editorCopy :: ClipboardEventObject -> Model -> IO Model
editorCopy e model =
  do preventDefault e
     dt <- clipboardData e
     setDataTransferData dt "text/plain" "boo-yeah"
     pure $ model & debugMsg .~ Just "copy"

app :: (WebSocketReq -> IO ()) -> Model -> Html Model
app sendWS model =
  let setFontSize s = EL Click (\e m -> pure $ m & (currentFont . fontSize) .~ s)
  in
         ([hsx|
           <div>
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
            <div class="col-md-6">
              <div class="btn-group" data-toggle="buttons">
               <label class="btn btn-default" [ EL Click boldChange ] >
                <input type="checkbox" autocomplete="off" style="font-weight: 800;" />B</label>
               <label class="btn btn-default" [ EL Click italicChange ] >
                <input type="checkbox" autocomplete="off" style="font-style: italic;" />I</label>
               <label class="btn btn-default" [ EL Click (itemize sendWS) ] >
                <input type="checkbox" autocomplete="off" />•</label>
              </div>
             </div>
            </div>
            <div id="editor" tabindex="1" style="outline: 0; line-height: 1.0; height: 600px; width: 300px; border: 1px solid black; box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.2);" autofocus="autofocus" [ EL KeyPress (keyPressed sendWS)
          , EL KeyDown  (keyDowned sendWS)
          , EL Click    (clickEditor sendWS)
          , EL Copy     editorCopy
          ] >
            <div id="caret" class="editor-caret" (caretPos model (indexToPos (model ^. caret) model))></div>
               <% renderLayout (model ^. layout) %>
            </div>

           </div>
          |])
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

chili app initAction model url handleWS =
  do (Just doc)   <- currentDocument
     (Just murv) <- getElementById doc "murv"
--     (Just body)  <- item nodes 0
     loop doc (toJSNode murv) model initAction url handleWS app
--     initRemoteWS url (handleWS updateModel)
--     updateModel initAction


initModel = Model
  { _document      = Document { _patches = mempty
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

initAction :: (WebSocketReq -> IO ()) -> Model -> IO Model
initAction sendWS model =
  do let rcs = nub $ richTextToRichChars bullet
     newMetrics <- calcMetrics (model ^. fontMetrics) rcs
     sendWS (WebSocketReq ReqInit)
     pure (model & fontMetrics  %~ (\old -> old `mappend` newMetrics))

     -- FIXME: perhaps this should be attached to the <body> tag?
--     cb <- asyncCallback activityTimeout
--     js_setTimeout cb 1000

resInit conn initPatches model =
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
     newFontMetrics <- calcMetrics (model ^. fontMetrics) (concatMap toRCs' (concatMap Patch.toList initPatches))
     pure (updateLayout $ model & connectionId .~ Just conn
                                & fontMetrics %~ (\old -> old `mappend` newFontMetrics)
                                & (document . patches) .~ initPatches
                                & (document . inflightPatch) .~ (Patch.fromList [])
                                & (document . forkedAt) .~ (Seq.length initPatches - 1)
                                & (document . pendingEdit) .~ [])

resAppendPatch connId (patchId, newPatch) model
  | model ^. connectionId == (Just connId) =
      do putStrLn "resAppendPatch: Got own patch back"
         let model' = updateLayout $
               model & (document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                     & (document . inflightPatch) .~ (Patch.fromList [])
                     & (document . forkedAt) .~ patchId
         pure model' -- got our own patch back

  | otherwise              = -- FIXME: probably need to call transform on the patch in progress
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
                                          & (document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                                          & (document . forkedAt) .~ patchId
                                    )
             newCaret = if maxEditPos newPatch < (model ^. caret)
                               then (model ^. caret) + patchDelta (newPatch)
                               else (model ^. caret)
         pure (model' & caret .~ newCaret)

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

handleMessage :: MessageEvent -> Model -> IO Model
handleMessage messageEvent model =
  do logMessage messageEvent
     case decodeRes messageEvent of
       Nothing ->
         do putStrLn "handleMessage: unable to decode response"
            pure model
       (Just res) ->
         case res of
           (ResAppendPatch connId (i, path)) -> resAppendPatch connId (i, path) model
           (ResInit connId patches) -> resInit connId patches model

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
