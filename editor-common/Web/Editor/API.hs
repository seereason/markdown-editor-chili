{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
module Web.Editor.API where

import           Common (Atom, ConnectionId)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Patch (Patch, Edit(..), toList, fromList, apply, diff)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Data.UserId(UserId(..))
import           Servant.API ((:<|>), (:>), ReqBody, JSON, Post, Raw)
import           Web.HttpApiData (FromHttpApiData, ToHttpApiData)

data WSRequest
  = ReqAddPatch Int (Patch Atom)
--  | ReqUpdateCurrent [Edit Atom]
  | ReqInit
    deriving Show
deriveJSON defaultOptions ''WSRequest

data WebSocketReq = WebSocketReq
     { reqRequest :: WSRequest
     }
    deriving Show
deriveJSON defaultOptions ''WebSocketReq

data WebSocketRes
  = ResAppendPatch ConnectionId (Int, Patch Atom)
--  | ResUpdateCurrent ConnectionId [Edit Atom]
  | ResInit ConnectionId (Seq (Patch Atom))
    deriving Show
deriveJSON defaultOptions ''WebSocketRes

type EditorAPI =
  "editor" :>
    ( "edit" :> ReqBody '[JSON] (Patch Atom) :> Post '[JSON] ()
           :<|> Raw
    )

