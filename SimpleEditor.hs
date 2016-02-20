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
module Main where

import Control.Lens ((^.), (.~), (?~), (&), (%~), (^?), _Just)
import Control.Lens.At (ix)
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Data.Char (chr)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Patch (Patch, Edit(..), toList, fromList, apply, diff)
import qualified Data.Patch as Patch
import Data.Text (Text, findIndex)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Servant.API ()
import Servant.Isomaniac (HasIsomaniac, MUV(..), ReqAction(..), isomaniac, muv, runIdent)
import Servant.Common.BaseUrl
import Servant.Common.Req (Req(..))
import Language.Haskell.HSX.QQ (hsx)
import Web.ISO.HSX
import Web.ISO.Types

type Document = Vector Char -- [Edit Char]
type Index = Int
type FontMetrics = Map Char (Double, Double)


data Box a = Box
  { _boxWidth     :: Double
  , _boxHeight    :: Double
  , _boxContent   :: a
  , _boxHighlight :: Bool
  }
  deriving Show
makeLenses ''Box

data Line a = Line
  { _lineHeight :: Double
  , _lineBoxes  :: [Box a]
  , _lineHighlight :: Bool
  }
makeLenses ''Line

{-

A document is formed by apply a list of Patch to mempty.

Patches have some restrictions. Edits in a patch can not depend on each other.

  "Patch resolution happens in zero time. Each edit in a particular
   patch is independent of any other edit. There is no telescopic
   dependency. E.g a patch P.fromList [a,b,c], if I remove the edit a,
   the edits b and c will work correctly unchanged, because they don't
   depend on a."

This means that the same patch can not add and then remove a character.

We wish to preserve the history of edits. We can do this by having
Patches that reflect what actually happened rather than consolidating
them.

As a basic rule we close the current patch and start a new one anytime we:

 1. insert after a delete
 2. delete after an insert
 3. change the caret position via the mouse/keyboard arrows (aka, with out inserting a character)

A deletion is always performed from the beginning of the text. Which in this case is the begining of the document.

-}

data EditState
  = Inserting
  | Deleting
  | MovingCaret
    deriving (Show, Eq)

data SelectionData = SelectionData
  { _selection       :: Selection
  , _selectionString :: String
  , _rangeCount      :: Int
  }
makeLenses ''SelectionData

-- could have a cache version of the Document with all the edits applied, then new edits would just mod that document
data Model = Model
  { _document    :: Vector Char -- ^ all of the patches applied but not the _currentEdit
  , _patches     :: [Patch Char]
  , _currentEdit :: [Edit Char]
  , _editState   :: EditState
  , _index       :: Index -- ^ current index for patch edit operations
  , _caret       :: Index -- ^ position of caret
  , _fontMetrics :: Map Char (Double, Double)
  , _measureElem :: Maybe JSElement
  , _debugMsg    :: Maybe Text
  , _mousePos    :: Maybe (Double, Double)
  , _editorPos   :: Maybe DOMClientRect
  , _targetPos   :: Maybe DOMClientRect
  , _layout      :: [Line Text]
  , _maxWidth    :: Double
  , _selectionData :: Maybe SelectionData
  }
makeLenses ''Model

data Action
    = KeyPressed Char
    | KeyDowned Int
    | UpdateMetrics
    | GetMeasureElement
    | MouseClick MouseEventObject
    | CopyA ClipboardEventObject
    | PasteA JS.JSString
    deriving Show

data EditAction
  = InsertChar Char
  | DeleteChar
  | MoveCaret Index
    deriving Show

handleAction :: EditAction -> Model -> Model
handleAction ea model
  | model ^. editState == Inserting =
      case ea of
       InsertChar c -> insertChar c model
       DeleteChar ->
         let newPatch   = Patch.fromList (model ^. currentEdit)
             newPatches = (model ^. patches) ++ [newPatch]
             newDoc     = apply newPatch (model ^. document)
             model'     = model { _document    = newDoc
                                , _patches     = newPatches
                                , _currentEdit = []
                                , _editState   = Deleting
                                , _index       = model ^. caret
                                }
         in handleAction DeleteChar model'
       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. currentEdit)
             newPatches = (model ^. patches) ++ [newPatch]
             newDoc     = apply newPatch (model ^. document)
             model'     = model { _document    = newDoc
                                , _patches     = newPatches
                                , _currentEdit = []
                                , _editState   = MovingCaret
                                }
         in handleAction ea model'

  | model ^. editState == Deleting =
      case ea of
       DeleteChar -> backspaceChar model
       InsertChar _ ->
         let newPatch   = Patch.fromList (model ^. currentEdit)
             newPatches = (model ^. patches) ++ [newPatch]
             newDoc     = apply newPatch (model ^. document)
             model'     = model { _document = newDoc
                                , _patches  = newPatches
                                , _currentEdit = []
                                , _editState   = Inserting
                                }
         in handleAction ea model'
       MoveCaret {} ->
         let newPatch   = Patch.fromList (model ^. currentEdit)
             newPatches = (model ^. patches) ++ [newPatch]
             newDoc     = apply newPatch (model ^. document)
             model'     = model { _document    = newDoc
                                , _patches     = newPatches
                                , _currentEdit = []
                                , _editState   = MovingCaret
                                }
         in handleAction ea model'

  | model ^. editState == MovingCaret =
      case ea of
       MoveCaret i ->
         model { _index = i
               , _caret = i
               }
       InsertChar {} ->
         handleAction ea (model { _editState = Inserting })
       DeleteChar {} ->
         handleAction ea (model { _editState = Deleting })

insertChar :: Char -> Model -> Model
insertChar c model =
  let newEdit =  (model ^. currentEdit) ++ [Insert (model ^. index) (if c == '\r' then '\n' else c)]
  in
   model { _currentEdit = newEdit
--        , _index    = model ^. index
         , _caret       = succ (model ^. caret)
         , _layout      = textToLines (model ^. fontMetrics) (model ^. maxWidth) 2 $ Text.pack $ Vector.toList (apply (Patch.fromList newEdit) (model ^. document))
         }

-- in --  newDoc =   (model ^. document) ++ [Delete (model ^. index) ' '] -- FIXME: won't work correctly if converted to a replace
backspaceChar :: Model -> Model
backspaceChar model
 | (model ^. index) > 0 = -- FIXME: how do we prevent over deletion?
  let index'  = pred (model ^. index)
      c       = (model ^. document) ! index'
      newEdit = (model ^. currentEdit) ++ [Delete index' c]
  in
   model { _currentEdit = newEdit
         , _index       = pred (model ^. index)
         , _caret       = pred (model ^. caret)
         , _layout      = textToLines (model ^. fontMetrics) (model ^. maxWidth) 2 $ Text.pack $ Vector.toList (apply (Patch.fromList newEdit) (model ^. document))
         }
 | otherwise = model
{--}
{-
relativeClickPos model =
  let mepos = model ^. editorPos in
   case mepos of
    Nothing -> Nothing
    (Just epos) ->
      case model ^. mousePos of
       Nothing -> Nothing
      (Just (mx, my)) -> Just (mx - (rectLeft epos), my - (rectTop epos))
-}

relativeClickPos model mx my =
   case model ^. editorPos of
    Nothing     -> Nothing
    (Just epos) -> Just (mx - (rectLeft epos), my - (rectTop epos))

lineAtY :: [Line a] -> Double -> Maybe Int
lineAtY lines y = go lines y 0
  where
    go [] _ _ = Nothing
    go (line:lines) y n =
      if y < line ^. lineHeight
      then Just n
      else go lines (y - line ^. lineHeight) (succ n)

indexAtPos :: FontMetrics -> [Line Text] -> (Double, Double) -> Maybe Int
indexAtPos fm lines (x,y) =
  case lineAtY lines y of
   Nothing -> Nothing
   (Just i) -> Just $ (sumPrevious $ take i lines) + indexAtX fm (lines!!i) x
   where
     sumPrevious lines = sum (map sumLine lines)
     sumLine line = sum (map (\box -> Text.length (box ^. boxContent)) (line ^. lineBoxes))

-- If we put Characters in boxes then we could perhaps generalize this
indexAtX :: FontMetrics -> Line Text -> Double -> Int
indexAtX fm line x = go (line ^. lineBoxes) x 0
  where
    indexAtX' :: [Char] -> Double -> Int -> Int
    indexAtX' [] x i = i
    indexAtX' (c:cs) x i =
      let cWidth =  fst $ fromJust $ Map.lookup c fm
      in if x < cWidth
         then i
         else indexAtX' cs (x - cWidth) (succ i)
    go :: [Box Text] -> Double -> Int -> Int
    go [] x i = i
    go (box:boxes) x i
      | x < (box ^. boxWidth) =
          indexAtX' (Text.unpack (box ^. boxContent)) x i
      | otherwise = go boxes (x - box ^. boxWidth) (i + Text.length (box ^. boxContent))

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

foreign import javascript unsafe "$1[\"clipboardData\"][\"getData\"](\"text/plain\")" getClipboardData ::
        ClipboardEventObject -> IO JS.JSString

update' :: Action -> Model -> IO (Model, Maybe (ReqAction Action))
update' action model'' =
  do (Just doc)        <- currentDocument
     (Just editorElem) <- getElementById doc "editor"
     rect <- getBoundingClientRect editorElem
     mSelectionData <- getSelectionData
     let model = model'' { _editorPos = Just rect
                         , _selectionData = mSelectionData
                         }
     case action of
      CopyA ceo      -> do cd <- clipboardData ceo
                           setDataTransferData cd "text/plain" "Boo-yeah!"
                           pure (model & debugMsg .~ Just "copy", Nothing)
      PasteA txt -> do -- txt <- getDataTransferData dt "text/plain"
--                       txt2 <- getClipboardData ceo
--                       js_alert txt2
                       pure (model & debugMsg .~ Just (textFromJSString txt), Nothing)
      KeyPressed c  -> pure (handleAction (InsertChar c) model, Nothing)
      KeyDowned c -- handle \r and \n ?
        | c == 8    -> pure (handleAction DeleteChar model, Nothing)
        | c == 32   -> pure (handleAction (InsertChar ' ') model, Nothing)
        | otherwise -> pure (model, Nothing)
      MouseClick e ->
        do elem <- target e
{-
         (Just doc) <- currentDocument
         (Just editorElem) <- getElementById doc "editor"
         rect <- getBoundingClientRect editorElem
-}

           targetRect <- getBoundingClientRect elem
           {-
           let highlightLine (Just n) lines = lines & ix n %~ lineHighlight .~ True
               highlightLine Nothing lines = lines
               (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
             -}
           let (Just (x,y)) = relativeClickPos model (clientX e) (clientY e)
               mIndex = indexAtPos (model ^. fontMetrics) (model ^. layout) (x,y)
               model' = model & mousePos ?~ (clientX e, clientY e)
                              & caret .~ (fromMaybe (model ^. caret) mIndex)
                              & targetPos ?~ targetRect
           case mIndex of
            (Just i) -> pure (handleAction (MoveCaret i) model', Nothing)
            Nothing  -> pure (model', Nothing)

--              & layout .~ highlightLine (lineAtY (model ^. layout) y) (map (\l -> l & lineHighlight .~ False) (model ^. layout))
--                       & layout .~ highlightLine (lineAtY (model ^. layout) y) (map (\l -> l & lineHighlight .~ False) (model ^. layout))
--                , Nothing)

      UpdateMetrics ->
        do case model ^. measureElem of
            Nothing -> do
              (Just document) <- currentDocument
              mme <- getElementById document "measureElement"
              case mme of
                Nothing -> pure (model { _debugMsg = Just "Could not find measureElement" }, Nothing)
                (Just me) ->
                  do let model' = model & measureElem .~ (Just me)
                     doMetrics model' me
            (Just me) -> doMetrics model me
        where
          doMetrics model me =
            do metrics <- mapM (getFontMetric me) [' '..'~']
               pure $ (model & fontMetrics .~ (Map.fromList metrics) & debugMsg .~ (Just $ Text.pack $ show metrics) , Nothing)

getFontMetric :: JSElement -> Char -> IO (Char, (Double, Double))
getFontMetric measureElm c =
  do setInnerHTML measureElm (JS.pack $ replicate 100 (if c == ' ' then '\160' else c))
     domRect <- getBoundingClientRect measureElm
     -- FIXME: width and height are not official properties of DOMClientRect
     let w = width domRect / 100
         h = height domRect
     pure (c, (w, h))

-- | similar to words except whitespace is preserved at the end of a word
textToWords :: Text -> [Text]
textToWords txt
  | Text.null txt = []
  | otherwise =
      let whiteIndex   = fromMaybe 0 $ findIndex (\c -> c ==' ') txt
          charIndex    = fromMaybe 0 $ findIndex (\c -> c /= ' ') (Text.drop whiteIndex txt)
      in case whiteIndex + charIndex of
          0 -> [txt]
          _ -> let (word, rest) = Text.splitAt (whiteIndex + charIndex) txt
               in (word : textToWords rest)

textToBox :: FontMetrics -> Text -> Box Text
textToBox fm input
  | Text.null input = Box { _boxWidth     = 0
                          , _boxHeight    = 0
                          , _boxContent   = input
                          , _boxHighlight = False
                          }
  | otherwise =
      Box { _boxWidth     = Text.foldr (\c w -> w + getWidth fm c) 0 input
          , _boxHeight    = getHeight fm (Text.head input)
          , _boxContent   = input
          , _boxHighlight = False
          }
  where
    getWidth, getHeight :: FontMetrics -> Char -> Double
    getWidth  fm c = maybe 0 fst (Map.lookup c fm)
    getHeight fm c = maybe 0 snd (Map.lookup c fm)

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- | FIXME: possibly not tail recursive -- should probably use foldr or foldl'
layoutBoxes :: Double -> (Double, [Box a]) -> [[Box a]]
layoutBoxes maxWidth (currWidth, []) = []
layoutBoxes maxWidth (currWidth, (box:boxes))
  | currWidth + (box ^. boxWidth) <= maxWidth =
      case layoutBoxes maxWidth ((currWidth + (box ^. boxWidth)), boxes) of
       [] -> [[box]] -- this is the last box
       (line:lines) -> (box:line):lines
  -- if a box is longer than the maxWidth we will place it at the start of a line and let it overflow
  | (currWidth == 0) && (box ^. boxWidth > maxWidth) =
      [box] : layoutBoxes maxWidth (0, boxes)
  | otherwise =
      ([]:layoutBoxes maxWidth (0, box:boxes))

calcSizes :: [[Box a]] -> [Line a]
calcSizes [] = []
calcSizes (line:lines) =
  mkLine line : calcSizes lines
  where
    mkLine [] = Line 0 [] False
    mkLine boxes = Line { _lineHeight = maximum (map _boxHeight boxes)
                        , _lineBoxes = boxes
                        , _lineHighlight = False
                        }

renderTextBoxes :: [Line Text] -> HTML Action
renderTextBoxes lines =
  [hsx| <div class="lines"><% map renderTextBoxes' lines %></div> |]

renderTextBoxes' :: Line Text -> HTML Action
renderTextBoxes' line =
  [hsx| <div class=(if line ^. lineHighlight then ("line highlight" :: Text) else "line")><% map renderTextBox (line ^. lineBoxes)  %></div> |]

renderTextBox :: Box Text -> HTML Action
-- renderTextBox box = CDATA True (box ^. boxContent)
renderTextBox box = [hsx| <span (if box ^. boxHighlight then [(("class" :: Text) := ("highlight" :: Text))] else [])><% nbsp $ box ^. boxContent %></span> |]
  where
    nbsp = Text.replace " " (Text.singleton '\160')

textToLines :: FontMetrics -> Double -> Int -> Text -> [Line Text]
textToLines fm maxWidth caret txt =
  let boxes = map (textToBox fm) (textToWords txt)
      boxes' = boxes & ix caret %~ boxHighlight .~ True
  in calcSizes $ layoutBoxes maxWidth (0, boxes')

linesToHTML :: [Line Text] -> HTML Action
linesToHTML lines = renderTextBoxes lines
{-
textToHTML :: FontMetrics -> Double -> Int -> Text -> HTML Action
textToHTML fm maxWidth caret txt =
  let boxes = map (textToBox fm) (textToWords txt)
      boxes' = boxes & ix caret %~ boxHighlight .~ True
  in
   renderTextBoxes (layoutBoxes maxWidth (0, boxes'))
-}
renderDoc :: [Line Text] -> HTML Action
renderDoc lines =
  [hsx|
    <div data-path="root">
     <% linesToHTML lines %>
--        <% textToHTML fm maxWidth 2 $ Text.pack $ Vector.toList (apply (Patch.fromList edits) mempty) %>
--      <% addP $ rlines $ Vector.toList (apply (Patch.fromList edits) mempty) %>
--      <% show $ map p $ rlines $ Vector.toList (apply (Patch.fromList edits) mempty) %>
    </div>
  |]
  where
    rlines :: String -> [String]
    rlines l =
      let (b, a) = break (\c -> c == '\n' || c == '\r') l
      in case a of
       [] -> [b]
       [c] -> b : [""]
       (_:cs) -> b : rlines cs
{-
    caret :: HTML Action
    caret =  [hsx| <span class="caret">♦</span> |]
    addP [] = [caret]
    addP [l] = [[hsx| <p><% l %><% caret %></p> |]]
    addP (l:ls) = (p l) : addP ls
    p c = [hsx| <p><% c %></p> |]
-}
-- | based on the font metrics, maximum width, and input text, calculate the line breaks
{-
layout :: FontMetrics
       -> Double
       -> Text
       -> [Text]
layout fm maxWidth input =
  -}

-- | given a character index, calculate its (x,y) coordinates in the editor
--
indexToPos :: Int
           -> Model
           -> Maybe (Double, Double)
indexToPos i model = go (model ^. layout) i (0,0)
  where
    go [] _ _ = Nothing
    go (line:lines) i curPos =
      case go' (line ^. lineBoxes) i curPos of
       (Right curPos) -> curPos
       (Left (i', (x,y))) ->
         go lines i' (0, y + line ^. lineHeight)

    go' [] i curPos = Left (i, curPos)
    go' _ 0 curPos = Right (Just curPos)
    go' (box:boxes) i (x,y) =
      if i > Text.length (box ^. boxContent)
         then go' boxes (i - Text.length (box ^. boxContent)) (x + box ^. boxWidth, y)
         else case indexToPosTxt i (model ^. fontMetrics) box of
               Nothing   -> Right Nothing
               (Just x') -> Right (Just (x + x', y))

indexToPosTxt :: Int -> FontMetrics -> Box Text -> Maybe Double
indexToPosTxt index fm box
  | Text.length (box ^. boxContent) < index = Nothing
  | otherwise =
      Just $ Text.foldr sumWidth 0 (Text.take index (box ^. boxContent))
  where
    sumWidth c acc =
      case Map.lookup c fm of
        Just (w, _) -> acc + w
        Nothing -> acc

caretPos :: Model -> Maybe (Double, Double) -> [KV Text Text]
caretPos model Nothing = []
caretPos model (Just (x, y)) =
  case model ^. editorPos of
   Nothing -> []
   (Just ep) -> ["style" := ("top: " <> (Text.pack $ show (y{- + (rectTop ep) -})) <> "px; left: " <> (Text.pack $ show (x {- + (rectLeft ep) -}) <> "px;"))]

view' :: Model -> (HTML Action, [Canvas])
view' model =
  let keyDownEvent  = Event KeyDown  (\e -> when (keyCode e == 8 || keyCode e == 32) (preventDefault e) >> pure (KeyDowned (keyCode e)))
      keyPressEvent = Event KeyPress (\e -> pure (KeyPressed (chr (charCode e))))
      clickEvent    = Event Click    (\e -> pure (MouseClick e))
      copyEvent     = Event Copy     (\e -> do preventDefault e ; dt <- clipboardData e ; setDataTransferData dt "text/plain" "boo-yeah" ; pure (CopyA e))
      pasteEvent    = Event Paste    (\e -> do preventDefault e ; dt <- clipboardData e ; txt <- getDataTransferData dt "text/plain" ; pure (PasteA txt))
  in
         ([hsx|
           <div>
--            <p><% show (Patch.fromList (model ^. document)) %></p>

--            <p><% Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
--            <p><% show $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
--            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>
--            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>

            <h1>Editor</h1>
            <input type="text" value="" [copyEvent, pasteEvent, copyEvent] />
            <div id="editor" tabindex="1" style="outline: 0; height: 200px; width: 300px; border: 1px solid black; box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.2);" autofocus="" [keyDownEvent, keyPressEvent, clickEvent, copyEvent] >
              <div id="caret" class="caret" (caretPos model (indexToPos (model ^. caret) model))></div>
              <% renderDoc (model ^. layout) %>
            </div>
            <h1>Debug</h1>
            <p>debugMsg: <% show (model ^. debugMsg) %></p>
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
            <p>line heights: <% show (map _lineHeight (model ^. layout)) %></p>

            <p>Document: <% show (model ^. document) %></p>
            <p>Patches: <% show (model  ^. patches) %></p>
            <p>Current Patch: <% show (model  ^. currentEdit) %></p>
            <p>Index: <% show (model ^. index) %></p>
            <p>Caret: <% show (model ^. caret) %></p>
            <% case model ^. selectionData of
                 Nothing -> <p>No Selection</p>
                 (Just selectionData) ->
                   <div>
                    <p>Selection: count=<% show $ selectionData ^. rangeCount %></p>
                    <p>Selection: len=<% show $ length $ selectionData ^. selectionString %></p>
                    <p>Selection: toString()=<% selectionData ^. selectionString %></p>
                   </div>
            %>
           </div>
          |], [])

editorMUV :: MUV IO Model Action (ReqAction Action)
editorMUV =
  MUV { model  = Model { _document    = mempty
                       , _patches     = []
                       , _currentEdit = []
                       , _editState   = Inserting
                       , _index       = 0
                       , _caret       = 0
                       , _fontMetrics = Map.empty
                       , _measureElem = Nothing
                       , _debugMsg    = Nothing
                       , _mousePos    = Nothing
                       , _editorPos   = Nothing
                       , _targetPos   = Nothing
                       , _layout      = []
                       , _maxWidth    = 300
                       , _selectionData = Nothing
                       }
      , update = update'
      , view   = view'
      }

main :: IO ()
main =
  muv editorMUV id (Just UpdateMetrics)
