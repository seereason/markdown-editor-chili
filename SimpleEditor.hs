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

import Control.Lens ((^.), (.~), (?~), (&))
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Data.Char (chr)
import qualified Data.JSString as JS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Patch (Patch, Edit(..), toList, fromList, apply, diff)
import qualified Data.Patch as Patch
import Data.Text (Text, findIndex)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Servant.API ()
import Servant.Isomaniac (HasIsomaniac, MUV(..), ReqAction(..), isomaniac, muv, runIdent)
import Servant.Common.BaseUrl
import Servant.Common.Req (Req(..))
import Language.Haskell.HSX.QQ (hsx)
import Web.ISO.HSX
import Web.ISO.Types

type Document = [Edit Char]
type Index = Int
type FontMetrics = Map Char (Double, Double)

-- could have a cache version of the Document with all the edits applied, then new edits would just mod that document
data Model = Model
  { _document    :: Document
  , _index       :: Index
  , _cursor      :: Index
  , _fontMetrics :: Map Char (Double, Double)
  , _measureElem :: Maybe JSElement
  , _debugMsg    :: Maybe Text
  , _mousePos    :: Maybe (Double, Double)
  , _editorPos   :: Maybe DOMClientRect
  , _targetPos   :: Maybe DOMClientRect
  }
makeLenses ''Model

data Box a = Box
  { _boxWidth  :: Double
  , _boxHeight :: Double
  , _boxContent :: a
  }
  deriving Show
makeLenses ''Box

data Action
    = KeyPressed Char
    | KeyDowned Int
    | UpdateMetrics
    | GetMeasureElement
    | MouseClick MouseEventObject
    deriving Show

insertChar :: Char -> Model -> Model
insertChar c model =
  model { _document = (model ^. document) ++ [Insert (model ^. index) (if c == '\r' then '\n' else c)]
        , _index    = model ^. index
        , _cursor   = succ (model ^. cursor)
        }

deleteChar :: Model -> Model
deleteChar model
  | (model ^. cursor) > 0 =
      model { _document = (model ^. document) ++ [Delete (model ^. index) ' '] -- FIXME: won't work correctly if converted to a replace
            , _index    = model ^. index
            , _cursor   = pred (model ^. cursor)
            }
  | otherwise = model

update' :: Action -> Model -> IO (Model, Maybe (ReqAction Action))
update' action model =
  case action of
    KeyPressed c  -> pure (insertChar c model, Nothing)
    KeyDowned c
      | c == 8    -> pure (deleteChar model, Nothing)
      | c == 32   -> pure (insertChar ' ' model, Nothing)
      | otherwise -> pure (model, Nothing)
    MouseClick e ->
      do elem <- target e
         (Just doc) <- currentDocument
         (Just editorElem) <- getElementById doc "editor"
         rect <- getBoundingClientRect editorElem
         targetRect <- getBoundingClientRect elem
         pure (model & mousePos ?~ (clientX e, clientY e)
                     & editorPos ?~ rect
                     & targetPos ?~ targetRect
              , Nothing)
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
  | Text.null input = Box { _boxWidth = 0
                          , _boxHeight = 0
                          , _boxContent = input
                          }
  | otherwise =
      Box { _boxWidth = Text.foldr (\c w -> w + getWidth fm c) 0 input
          , _boxHeight = getHeight fm (Text.head input)
          , _boxContent = input
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

renderTextBoxes :: [[Box Text]] -> HTML Action
renderTextBoxes lines =
  [hsx| <div class="lines"><% map renderTextBoxes' lines %></div> |]

renderTextBoxes' :: [Box Text] -> HTML Action
renderTextBoxes' line =
  [hsx| <div class="line"><% map renderTextBox line  %></div> |]

renderTextBox :: Box Text -> HTML Action
-- renderTextBox box = CDATA True (box ^. boxContent)
renderTextBox box = [hsx| <span><% nbsp $ box ^. boxContent %></span> |]
  where
    nbsp = Text.replace " " (Text.singleton '\160')


textToHTML :: FontMetrics -> Double -> Text -> HTML Action
textToHTML fm maxWidth txt =
   renderTextBoxes (layoutBoxes maxWidth (0, map (textToBox fm) (textToWords txt)))

renderDoc :: FontMetrics -> Double -> Document -> Index -> HTML Action
renderDoc fm maxWidth edits cursor =
  [hsx|
    <div data-path="root">
        <% textToHTML fm maxWidth $ Text.pack $ Vector.toList (apply (Patch.fromList edits) mempty) %>
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

    cursor :: HTML Action
    cursor =  [hsx| <span class="cursor">♦</span> |]
    addP [] = [cursor]
    addP [l] = [[hsx| <p><% l %><% cursor %></p> |]]
    addP (l:ls) = (p l) : addP ls
    p c = [hsx| <p><% c %></p> |]

-- | based on the font metrics, maximum width, and input text, calculate the line breaks
{-
layout :: FontMetrics
       -> Double
       -> Text
       -> [Text]
layout fm maxWidth input =
  -}

view' :: Model -> (HTML Action, [Canvas])
view' model =
  let keyDownEvent  = Event KeyDown (\e -> when (keyCode e == 8 || keyCode e == 32) (preventDefault e) >> pure (KeyDowned (keyCode e)))
      keyPressEvent = Event KeyPress (\e -> pure (KeyPressed (chr (charCode e))))
      clickEvent    = Event Click (\e -> pure (MouseClick e))
  in
         ([hsx|
           <div>
            <h1>Debug</h1>
            <p><% show (model ^. mousePos) %></p>
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
            {-
            <p><% show (model ^. document) %></p>
            <p><% show (Patch.fromList (model ^. document)) %></p>
            <p><% show (model ^. debugMsg) %></p>
            <p><% Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
            <p><% show $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty) %></p>
            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>
            <p><% show $ layoutBoxes 300 (0, map (textToBox (model ^. fontMetrics)) $ textToWords $ Text.pack $ Vector.toList (apply (Patch.fromList (model ^. document)) mempty)) %></p>
-}
            <h1>Editor</h1>
            <div id="editor" tabindex="1" style="outline: 0; height: 500px; width: 300px; border: 1px solid black; box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.2);" autofocus="" [keyDownEvent, keyPressEvent, clickEvent] >
              <div id="cursor" class="cursor"></div>
              <% case model ^. document of
                  d          -> renderDoc (model ^. fontMetrics) 300 d (model ^. cursor)
               %>
            </div>
           </div>
          |], [])

editorMUV :: MUV IO Model Action (ReqAction Action)
editorMUV =
  MUV { model  = Model { _document    = mempty
                       , _index       = 0
                       , _cursor      = 0
                       , _fontMetrics = Map.empty
                       , _measureElem = Nothing
                       , _debugMsg    = Nothing
                       , _mousePos    = Nothing
                       , _editorPos   = Nothing
                       , _targetPos   = Nothing
                       }
      , update = update'
      , view   = view'
      }

main :: IO ()
main =
  muv editorMUV id (Just UpdateMetrics)
