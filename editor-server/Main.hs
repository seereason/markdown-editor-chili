module Main where

import Common               (Atom)
import Control.Monad        (MonadPlus, msum, forever)
import Control.Monad.Trans  (MonadIO(liftIO))
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Concurrent.STM    (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar', readTVar, writeTVar)
import Control.Exception    (finally)
import Data.Aeson.Encode    (encodeToTextBuilder)
import Data.Aeson           (decode, encode, toJSON)
import Data.Patch           (Patch, Edit(..), toList, fromList, apply, diff)
import Data.Text            (Text)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.IO as T
import Data.Proxy (Proxy(..))
import Data.Monoid (Monoid)
import Happstack.Server (Response, ServerPartT, Browsing(EnableBrowsing), asContentType, dir, simpleHTTP, nullConf, nullDir, serveDirectory, serveFile, seeOther, toResponse)
import Happstack.Server.WebSockets (runWebSocketsHappstack)
import Servant.API hiding (Patch)
import Servant.Happstack
import Web.Editor.API (EditorAPI, WebSocketReq(..), WebSocketRes(..), WSRequest(..))
import Network.WebSockets (Connection, ServerApp, acceptRequest, receiveData, sendBinaryData, sendTextData)

instance Monoid ServantErr

data Document = Document
 { patches      :: [Patch Atom]
 , currentEdit  :: [Edit Atom]
 }

emptyDocument :: Document
emptyDocument = Document
 { patches     = []
 , currentEdit = []
 }

data ServerState = ServerState
 { nextConnNum :: Int
 , connections :: [(Int, Connection)]
 , document    :: Document
 }

initialServerState :: ServerState
initialServerState =
 ServerState { nextConnNum = 0
             , connections = []
             , document    = emptyDocument
             }

editorAPI :: Proxy EditorAPI
editorAPI = Proxy

patchPost thePatch = pure ()

editorApp :: TVar ServerState -> ServerApp
editorApp tvServerState pendingConnection =
  do liftIO $ putStrLn "accepting connection..."
     conn <- acceptRequest pendingConnection
     liftIO $ putStrLn "accepted."
     i <- atomically $ do ss <- readTVar tvServerState
                          let ss' = ss { nextConnNum = succ (nextConnNum ss)
                                       , connections = (nextConnNum ss, conn) : (connections ss)
                                       }
                          writeTVar tvServerState ss'
                          pure (nextConnNum ss)

-- modifyTVar' tvServerState (\ss -> ss { nextConnNum = succ (nextConnNum ss)
--                                                             , connections = (nextConnNum ss, conn) : (connections ss)
--                                                             })
     (forever $ do bs' <- receiveData conn
                   let bs = C.takeWhile (/= '\0') bs'
--                  liftIO $ print bs
                   let mReqs = (decode bs) :: Maybe [WebSocketReq]
--                  liftIO $ print mReqs
                   case mReqs of
                     Nothing -> pure ()
                     (Just reqs) ->
                          mapM_ handleReq reqs) `finally` (do putStrLn $ "Removing connection: " ++ show i
                                                              atomically $ modifyTVar' tvServerState (\ss -> ss { connections = filter (\(n,_) -> n /= i) (connections ss) }))
     --             sendTextData conn t
  where
    handleReq req =
      case req of
        (WebSocketReq userid (ReqUpdateCurrent edits)) ->
          do conns <- atomically (connections <$> readTVar tvServerState)
             let msg = Builder.toLazyText (encodeToTextBuilder (toJSON (ResUpdateCurrent userid edits)))
             mapM_ (\(_, conn) -> sendTextData conn msg) conns
        (WebSocketReq userid (ReqAddPatch patch)) ->
          do conns <- atomically (connections <$> readTVar tvServerState)
             let msg = Builder.toLazyText (encodeToTextBuilder (toJSON (ResAppendPatch userid patch)))
             mapM_ (\(_, conn) -> sendTextData conn msg) conns

websockets :: (MonadIO m) => TVar ServerState -> ServerPartT m Response
websockets tvServerState = runWebSocketsHappstack (editorApp tvServerState)

editorServer :: TVar ServerState -> Server EditorAPI
editorServer tvServerState = patchPost :<|> serveClient
  where
    serveClient :: (MonadPlus m, MonadIO m) => ServerPartT m Response
    serveClient =
      let basePath = "../editor-client/.cabal-sandbox/bin/editor-client.jsexe/"
      in  msum [ nullDir >> seeOther ("/editor/index.html" :: String) (toResponse ())
               , dir "data"       $ serveDirectory EnableBrowsing [] "data"
               , dir "style.css"  $ serveFile (asContentType "text/css") ("../editor-client/style.css")
               , dir "index.html" $ serveFile (asContentType "text/html") "data/index.html"
               , dir "rts.js"     $ serveFile (asContentType "application/javascript") (basePath ++ "rts.js")
               , dir "out.js"     $ serveFile (asContentType "application/javascript") (basePath ++ "out.js")
               , dir "lib.js"     $ serveFile (asContentType "application/javascript") (basePath ++ "lib.js")
               , dir "runmain.js" $ serveFile (asContentType "application/javascript") (basePath ++ "runmain.js")
               , dir "websockets" $ websockets tvServerState
               ]
main :: IO ()
main =
  do tvServerState <- atomically (newTVar initialServerState)
     simpleHTTP nullConf $ serve editorAPI (editorServer tvServerState)
