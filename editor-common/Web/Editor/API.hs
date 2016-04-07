{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
module Web.Editor.API where

import           Common (Atom)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Patch (Patch, Edit(..), toList, fromList, apply, diff)
import           Data.UserId(UserId(..))
import           Servant.API ((:<|>), (:>), ReqBody, JSON, Post, Raw)
import           Web.HttpApiData (FromHttpApiData, ToHttpApiData)

data WSRequest
  = ReqAddPatch (Patch Atom)
  | ReqUpdateCurrent [Edit Atom]
    deriving Show
deriveJSON defaultOptions ''WSRequest

data WebSocketReq = WebSocketReq
     { reqUser    :: UserId
     , reqRequest :: WSRequest
     }
    deriving Show
deriveJSON defaultOptions ''WebSocketReq

data WebSocketRes
  = ResUpdateCurrent UserId [Edit Atom]
  | ResAppendPatch UserId (Patch Atom)
    deriving Show
deriveJSON defaultOptions ''WebSocketRes

type EditorAPI =
  "editor" :>
    ( "edit" :> ReqBody '[JSON] (Patch Atom) :> Post '[JSON] ()
           :<|> Raw
    )

