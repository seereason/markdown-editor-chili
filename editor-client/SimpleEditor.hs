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
import Data.Aeson (decode)
import Data.Char (chr)
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
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback)
import JavaScript.Web.MessageEvent (MessageEvent(..), MessageEventData(..))
import qualified JavaScript.Web.MessageEvent as MessageEvent
import JavaScript.Web.WebSocket (WebSocketRequest(..), connect, send)
import Servant.API ()
import Servant.Isomaniac (HasIsomaniac, MUV(..), ReqAction(..), isomaniac, muv, muvWS, runIdent)
import Servant.Common.BaseUrl
import Servant.Common.Req (Req(..))
import Language.Haskell.HSX.QQ (hsx)
import Web.Editor.API
import Web.ISO.HSX
import Web.ISO.Types hiding (Context2D(Font))
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

data BrowserIO = BrowserIO
  { _bSelectionData :: Maybe SelectionData
  , _bEditorPos     :: Maybe DOMClientRect
  , _mTargetRect    :: Maybe DOMClientRect
  , _newFontMetrics :: Map RichChar (Double, Double)
  , _bNow           :: POSIXTime
  }
makeLenses ''BrowserIO

data Action
    = KeyPressed Char
    | KeyDowned Int
    | UpdateMetrics
    | GetMeasureElement
    | MouseClick MouseEventObject
    | CopyA ClipboardEventObject
    | PasteA JS.JSString
    | AddImage
    | IncreaseFontSize
    | DecreaseFontSize
    | ToggleBold
    | IncUserId
    | WSRes WebSocketRes
    | Init
    | ActivityTimeout
    deriving Show

data EditAction
  = InsertAtom Atom
  | DeleteAtom
  | MoveCaret Index
    deriving Show

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
        (Box 0 18 True LineBreak : boxify fm (Vector.tail v)) -- FIXME: height should probably be the height of a space char?
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

handleAction :: EditAction -> Model -> (Model, Maybe [WebSocketReq])
handleAction ea model
  | model ^. editState == Inserting =
      case ea of
       InsertAtom c -> insertChar c model
       DeleteAtom ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model'     = model { _document    = (model ^. document) & inflightPatch .~ newPatch
                                                                     & pendingEdit   .~ []
                                , _editState   = Deleting
                                , _index       = model ^. caret
                                }
             (model'', mwsq) = handleAction DeleteAtom model'
         in (model'', Just $ (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch)) : (fromMaybe [] mwsq))
       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model'     =  model { _document    = (model ^. document) & inflightPatch .~ newPatch
                                                                      & pendingEdit   .~ []
                                  , _editState   = MovingCaret
                                  }
             (model'', mwsq) = handleAction ea model'
         in (model'', Just $ (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch)) : (fromMaybe [] mwsq))

  | model ^. editState == Deleting =
      case ea of
       DeleteAtom -> backspaceChar model
       InsertAtom _ ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model' = model & (document . inflightPatch) .~ newPatch
                            & (document . pendingEdit) .~ []
                            & editState .~ Inserting
             (model'', mwsq) = handleAction ea model'
         in (model'', Just $ (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch)) : (fromMaybe [] mwsq))

       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. document ^. pendingEdit)
             model'     =  model { _document    = (model ^. document) & inflightPatch .~ newPatch
                                                                      & pendingEdit   .~ []
                                  , _editState   = MovingCaret
                                  }
             (model'', mwsq) = handleAction ea model'
         in (model'', Just $ (WebSocketReq (ReqAddPatch (model ^. document ^. forkedAt) newPatch)) : (fromMaybe [] mwsq))

  | model ^. editState == MovingCaret =
      case ea of
       MoveCaret i ->
         (updateLayout $ model { _index = i
                               , _caret = i
                               }, Nothing)
       InsertAtom {} ->
         handleAction ea (model { _editState = Inserting })
       DeleteAtom {} ->
         handleAction ea (model { _editState = Deleting })

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
     pure (rc, (w, h))

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

browserIO' :: TQueue Action
           -> Action
           -> Model
           -> IO BrowserIO
browserIO' queue action model =
  do (Just doc)        <- currentDocument
     (Just editorElem) <- getElementById doc "editor"
     rect <- getBoundingClientRect editorElem
     mSelectionData <- getSelectionData
     js_focus editorElem
     now' <- getPOSIXTime
     nfm <- case action of
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
                                toRCs (RC rc) = [rc]
                                toRCs (RT rt) = richTextToRichChars rt
                                toRCs' :: Edit Atom -> [RichChar]
                                toRCs' (Insert _ a) = toRCs a
                                toRCs' (Delete _ _) = []
                                toRCs' (Replace _ _ a) = toRCs a
                            in calcMetrics (model ^. fontMetrics) (concatMap toRCs' (concatMap Patch.toList patches))

                      _ -> pure Map.empty
     mTargetRect <- case action of
                      (MouseClick e) ->
                        do elem <- target e
                           targetRect <- getBoundingClientRect elem
                           pure (Just $ targetRect)
                      _ -> pure Nothing

     case action of
       ActivityTimeout ->do cb <- asyncCallback $ atomically $ writeTQueue queue ActivityTimeout
                            js_setTimeout cb 2000
       Init -> do cb <- asyncCallback $ atomically $ writeTQueue queue ActivityTimeout
                  js_setTimeout cb 1000
       _ -> pure ()
     pure $ BrowserIO { _bSelectionData = mSelectionData
                      , _bEditorPos     = Just rect
                      , _mTargetRect    = mTargetRect
                      , _newFontMetrics = nfm
                      , _bNow           = now'
                      }

update' :: Action
        -> BrowserIO
        -> Model
        -> (Model, Maybe [WebSocketReq])
update' action ioData model'' =
  let activity =
        case action of
          AddImage {}   -> True
          KeyPressed {} -> True
          PasteA {}     -> True
          KeyDowned {}  -> True
          MouseClick {} -> True
          _             -> False
      forceActivity =
        if ((not activity) && ((ioData ^. bNow) >= (model ^. lastActivity) + 2))
        then True
        else False
      model = model'' { _editorPos     = ioData ^. bEditorPos
                      , _selectionData = ioData ^. bSelectionData
                      , _lastActivity = if activity then (ioData ^. bNow) else (model'' ^. lastActivity)
                      }
  in
     case action of
      Init             -> (model, Just [WebSocketReq ReqInit])
      ActivityTimeout  -> if forceActivity
                             then let (model', req) = handleAction (MoveCaret (model ^. caret)) model
                                  in (model' & debugMsg .~ (Just $ Text.pack "Forced update."), req)
                             else (model & debugMsg .~ (Just $ Text.pack $ show $ ioData ^. bNow), Nothing)

      AddImage         ->  handleAction (InsertAtom (Img (Image "http://i.imgur.com/YFtU4OV.png" 174 168))) model
      IncreaseFontSize ->  (model & (currentFont . fontSize) %~ succ, Nothing)
      DecreaseFontSize ->  (model & (currentFont . fontSize) %~ (\fs -> if fs > 1.0 then pred fs else fs), Nothing)
      ToggleBold       ->  (model & (currentFont . fontWeight) %~ (\w -> if w == FW400 then FW700 else FW400), Nothing)
      IncUserId        ->  (model & userId %~ succ, Nothing)
      CopyA ceo      ->  -- cd <- clipboardData ceo
                           -- setDataTransferData cd "text/plain" "Boo-yeah!"
                         (model & debugMsg .~ Just "copy", Nothing)
      PasteA txt ->  -- txt <- getDataTransferData dt "text/plain"
--                       txt2 <- getClipboardData ceo
--                       js_alert txt2
                     (model & debugMsg .~ Just (textFromJSString txt), Nothing)

      KeyPressed c  ->
        let rc = RichChar (model ^. currentFont) c
            model' = case Map.lookup rc (ioData ^. newFontMetrics) of
              Nothing -> model
              (Just metric) -> set (fontMetrics . at rc) (Just metric) model
        in handleAction (InsertAtom (RC rc)) model'

      -- used for handling special cases like backspace, space
      KeyDowned c -- handle \r and \n ?
        | c == 8    -> handleAction DeleteAtom model
        | c == 13   -> handleAction (InsertAtom LineBreak) model
        | c == 32   -> let rc = RichChar (model ^. currentFont) ' '
                           model' = case Map.lookup rc (ioData ^. newFontMetrics) of
                             Nothing -> model
                             (Just metric) -> set (fontMetrics . at rc) (Just metric) model
                       in handleAction (InsertAtom (RC rc)) model'
        | otherwise -> (model, Nothing)

      MouseClick e ->
           let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
               mIndex = indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
               model' = model & mousePos  ?~ (clientX e, clientY e)
                              & caret     .~ (fromMaybe (model ^. caret) mIndex)
                              & targetPos .~ (ioData ^. mTargetRect)
                              & debugMsg  .~ Just (Text.pack (show (model ^. layout)))
           in case mIndex of
               (Just i) -> handleAction (MoveCaret i) model'
               Nothing  -> (model' & debugMsg .~ (Just $ Text.pack "Could not find the index of the mouse click."), Nothing)

      UpdateMetrics -> (model, Nothing)

      WSRes wsres ->
        case wsres of
          (ResAppendPatch connId (patchId, newPatch))
            | model ^. connectionId == (Just connId) ->
              let model' = updateLayout $
                             model & (document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                                   & (document . inflightPatch) .~ (Patch.fromList [])
                                   & (document . forkedAt) .~ patchId
              in (model', Nothing) -- got our own patch back

            | otherwise              -> -- FIXME: probably need to call transform on the patch in progress
                let model' = updateLayout (model & fontMetrics %~ (\old -> old `mappend` (ioData ^. newFontMetrics))
                                                 & (document . patches) %~ (\oldPatches -> oldPatches |> newPatch)
                                                 & (document . forkedAt) .~ patchId
                                                 )
                    newCaret = if maxEditPos newPatch < (model ^. caret)
                               then (model ^. caret) + patchDelta (newPatch)
                               else (model ^. caret)
                in (model' & caret .~ newCaret, Nothing)
          (ResInit conn initPatches) ->
            (updateLayout $ model & connectionId .~ Just conn
                                  & fontMetrics %~ (\old -> old `mappend` (ioData ^. newFontMetrics))
                                  & (document . patches) .~ initPatches
                                  & (document . inflightPatch) .~ (Patch.fromList [])
                                  & (document . forkedAt) .~ (Seq.length initPatches - 1)
                                  & (document . pendingEdit) .~ [], Nothing)

-- | calculate tho CSS value for a 'Font'
fontToStyle :: Font -> Text
fontToStyle font =
  "font-size: " <> Text.pack (show $ font ^. fontSize) <>
  "px; font-weight: " <> (fontWeightText $ font ^. fontWeight) <>
  "; font-style: " <> (case (font ^. fontStyle) of
                          Normal  -> "normal;"
                          Italic  -> "italic;"
                          Oblique -> "oblique;")

-- | convert an 'AtomBox' to HTML
renderAtomBox :: AtomBox
              -> [HTML Action]
-- renderTextBox box = CDATA True (box ^. boxContent)
renderAtomBox box =
  case box ^. boxContent of
    (RT (RichText txts)) -> map renderText txts
    (Img img)            -> [[hsx| <img src=(img ^. imageUrl) /> |]]
    LineBreak            -> [[hsx|<span style="display:inline-block;"></span>|]]
  where
    renderText :: (Font, Text) -> HTML Action
    renderText (font, txt) = [hsx| <span style=(fontToStyle font)><% nbsp txt %></span>   |]
    nbsp = Text.replace " " (Text.singleton '\160')

-- | convert a horizontal list of 'AtomBom' to HTML
renderAtomBoxes' :: HBox [AtomBox] -> HTML Action
renderAtomBoxes' box =
    [hsx| <div class=("line"::Text)><% concatMap renderAtomBox (box ^. boxContent)  %></div> |]
--  [hsx| <div class=(if line ^. lineHighlight then ("line highlight" :: Text) else "line")><% map renderTextBox (line ^. lineBoxes)  %></div> |]

-- | convert a vertical list of horizontal lists of 'AtomBom' to HTML
renderAtomBoxes :: VBox [HBox [AtomBox]] -> HTML Action
renderAtomBoxes lines =
  [hsx| <div class="lines"><% map renderAtomBoxes' (lines ^. boxContent) %></div> |]

-- render a layout (vertical list of horizontal lists of AtomBoxes) to HTML
renderLayout :: VBox [HBox [AtomBox]]
          -> HTML Action
renderLayout lines =
  [hsx|
    <div data-path="root">
     <% renderAtomBoxes lines %>
--        <% textToHTML fm maxWidth 2 $ Text.pack $ Vector.toList (apply (Patch.fromList edits) mempty) %>
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
indexToPos i model = go (model ^. layout ^. boxContent) i (0,0,18) -- FIMXE: maybe shouldn't hardcode to 18
  where
    -- go over the lines
    go [] _ _  = Nothing
    go (hbox:hboxes) i curPos =
      -- walk over thecurrent line
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

view' :: Model -> (HTML Action, [Canvas])
view' model =
  let keyDownEvent     = Event KeyDown  (\e -> when (keyCode e == 8 || keyCode e == 32 || keyCode e == 13) (preventDefault e) >> pure (KeyDowned (keyCode e)))
      keyPressEvent    = Event KeyPress (\e -> pure (KeyPressed (chr (charCode e))))
      clickEvent       = Event Click    (\e -> pure (MouseClick e))
      copyEvent        = Event Copy     (\e -> do preventDefault e ; dt <- clipboardData e ; setDataTransferData dt "text/plain" "boo-yeah" ; pure (CopyA e))
      pasteEvent       = Event Paste    (\e -> do preventDefault e ; dt <- clipboardData e ; txt <- getDataTransferData dt "text/plain" ; pure (PasteA txt))
      addImage         = Event Click    (\e -> pure AddImage)
      increaseFontSize = Event Click    (\e -> pure IncreaseFontSize)
      decreaseFontSize = Event Click    (\e -> pure DecreaseFontSize)
      toggleBold       = Event Click    (\e -> pure ToggleBold)
      incUserId        = Event Click    (\e -> pure IncUserId)
  in
         ([hsx|
           <div>
--            <p><% show ( (model ^. layout)) %></p>

--            <p><% Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
--            <p><% show $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
--            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>
--            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>
             <% if True
                 then <div style="float: right; width: 800px;">
                             <h1>Debug</h1>
                             <p>userId: <% show (model ^. userId) %></p>
                             <p>debugMsg: <% show (model ^. debugMsg) %></p>
--                             <p>Document: <% show (model ^. document) %></p>
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
                             <p>Font Metrics <% show (model ^. fontMetrics) %></p>

                      </div>
                 else <span></span> %>

            <h1>Editor</h1>
            <button [addImage]>Add Image</button>
            <button [increaseFontSize]>+</button>
            <button [decreaseFontSize]>-</button>
            <button [toggleBold]>Toggle Bold</button>
            <button [incUserId]>UserId+</button>
            <div id="editor" tabindex="1" style="outline: 0; height: 600px; width: 300px; border: 1px solid black; box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.2);" autofocus="autofocus" [keyDownEvent, keyPressEvent, clickEvent, copyEvent] >
              <div id="caret" class="caret" (caretPos model (indexToPos (model ^. caret) model))></div>
              <% renderLayout (model ^. layout) %>
            </div>

           </div>
          |], [])

editorMUV :: MUV () Model BrowserIO Action [WebSocketReq]
editorMUV =
  MUV { model  = Model { _document      = Document { _patches = mempty
                                                   , _inflightPatch = Patch.fromList []
                                                   , _forkedAt = 0
                                                   , _pendingEdit = []
                                                   }
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
      , browserIO = browserIO'
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

main :: IO ()
main =
  muvWS editorMUV "ws://localhost:8000/editor/websockets" decodeRes (Just Init)

