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
{-# LANGUAGE StandaloneDeriving #-}
module Common where

import Control.Lens ((^.), (.~), (?~), (&), (%~), (^?), _Just, set)
import Control.Lens.At (at, ix)
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Char (chr)
import Data.Data (Data, Typeable)
-- import qualified Data.JSString as JS
-- import Data.JSString.Text (textToJSString, textFromJSString)
import Data.List (findIndex, groupBy, null, splitAt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Patch (Patch, Edit(..), toList, fromList, apply, diff)
import qualified Data.Patch as Patch
import Data.Sequence          (Seq, (|>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Servant.API ()
-- import Servant.Isomaniac (HasIsomaniac, MUV(..), ReqAction(..), isomaniac, muv, runIdent)
-- import Servant.Common.BaseUrl
-- import Servant.Common.Req (Req(..))
import Language.Haskell.HSX.QQ (hsx)
-- import Web.ISO.HSX
-- import Web.ISO.Types hiding (Context2D(Font))

type Index = Int

deriveJSON defaultOptions ''Edit
deriveJSON defaultOptions ''Patch

data FontStyle
  = Normal
  | Italic
  | Oblique
  deriving (Eq, Ord, Show, Read, Data, Typeable)
deriveJSON defaultOptions ''FontStyle

data FontWeight
  = FW100
  | FW200
  | FW300
  | FW400
  | FW500
  | FW600
  | FW700
  | FW800
  | FW900
  deriving (Eq, Ord, Show, Read, Data, Typeable)
deriveJSON defaultOptions ''FontWeight

fontWeightText :: FontWeight -> Text
fontWeightText weight =
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

data Font = Font
 { _fontWeight :: FontWeight
 , _fontSize   :: Double
 , _fontStyle  :: FontStyle
 }
 deriving (Eq, Ord, Show, Read, Data, Typeable)
makeLenses ''Font
deriveJSON defaultOptions ''Font

defaultFont :: Font
defaultFont =
  Font { _fontWeight = FW400
       , _fontSize   = 16
       , _fontStyle  = Normal
       }

data RichChar = RichChar
  { _font :: Font
  , _char :: Char
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)
makeLenses ''RichChar
deriveJSON defaultOptions ''RichChar

type FontMetrics = Map RichChar (Double, Double)

-- | a block of non-breaking Text
--
-- We have a list of [(Font,Text)]. This allows us to do things like
-- have the font style change in the middle of a word with out
-- introducing the possibility that the layout engine will insert a
-- line-break in the middle of the world.
data RichText = RichText
  { _text :: [(Font, Text)]
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)
makeLenses ''RichText
deriveJSON defaultOptions ''RichText

richTextLength :: RichText -> Int
richTextLength (RichText txts) = sum (map (Text.length . snd) txts)

richCharsToRichText :: [RichChar] -> RichText
richCharsToRichText []    = RichText []
richCharsToRichText chars =
  RichText $ map flatten $ groupBy (\(RichChar f _) (RichChar f' _) -> f == f') chars
  where
    flatten :: [RichChar] -> (Font, Text)
    flatten chars = ((head chars) ^. font, Text.pack (map _char chars))

richTextToRichChars :: RichText -> [RichChar]
richTextToRichChars (RichText texts) =
  concatMap (\(font, txt) -> [ RichChar font c | c <- Text.unpack txt ]) texts

data Box c a = Box
  { _boxWidth     :: Double
  , _boxHeight    :: Double
  , _boxLineBreak :: Bool
  , _boxContent   :: a
  }
  deriving (Eq, Ord, Show, Read, Typeable)
makeLenses ''Box

-- deriving instance (Typeable c) => Typeable (Box c a)
deriving instance (Data a, Typeable c, Typeable (Box c a)) => Data (Box c a)

data Image = Image
  { _imageUrl    :: Text
  , _imageWidth  :: Double
  , _imageHeight :: Double
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)
makeLenses ''Image
deriveJSON defaultOptions ''Image

data Atom
  = RC RichChar
  | RT RichText
  | Img Image
  | Conflict Atom Atom
  | LineBreak
  | Item
    deriving (Eq, Ord, Show, Read, Data, Typeable)
deriveJSON defaultOptions ''Atom

merge :: Atom -> Atom -> Atom
merge a1 a2 = Conflict a1 a2

isRichChar :: Atom -> Bool
isRichChar (RC {}) = True
isRichChar _             = False

unRichChar :: Atom -> RichChar
unRichChar (RC rc) = rc

-- | length here is a measure of ??
atomLength :: Atom -> Int
atomLength (RC {})  = 1
atomLength (RT (RichText txts)) = sum (map (Text.length . snd) txts)
atomLength (Img {})       = 1
atomLength LineBreak = 1 -- FIXME: or is it zero?
atomLength Item = 1 -- or is it zero?
atomLength (Conflict atom1 atom2) = atomLength atom1 + atomLength atom2

type AtomBox  = Box Singleton Atom

data Direction
  = Horizontal
  | Vertical
  | Singleton
    deriving (Eq, Show, Read, Data, Typeable)

type HBox a = Box Horizontal a
type VBox a = Box Vertical a

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
    deriving (Eq, Show, Read, Data, Typeable)

type ConnectionId = Int

patchDelta :: Patch a -> Int
patchDelta patch =
  let edits = Patch.toList patch
      offset (Insert {})  = 1
      offset (Delete {})  = (-1)
      offset (Replace {}) = 0
  in foldr (\edit delta -> delta + offset edit) 0 edits

maxEditPos :: Patch a -> Int
maxEditPos patch =
  let edits = Patch.toList patch
  in
    case edits of
      [] -> 0
      _ -> case last edits of
        Insert i _    -> i
        Delete i _    -> i
        Replace i _ _ -> i

data Document = Document
 { _patches      :: Seq (Patch Atom)
 } deriving (Eq, Read, Show)
makeLenses ''Document
deriveJSON defaultOptions ''Document

emptyDocument :: Document
emptyDocument = Document
 { _patches     = mempty
 }
