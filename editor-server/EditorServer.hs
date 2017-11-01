module EditorServer where

import Common               (Atom, ConnectionId, Document(..), emptyDocument, merge, patches)
import Control.Monad        (MonadPlus, msum, forever)
import Control.Monad.Trans  (MonadIO(liftIO))
import Control.Lens         ((^.))
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Concurrent.STM    (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar', readTVar, writeTVar)
import Control.Exception    (finally)
import Data.Aeson.Encode    (encodeToTextBuilder)
import Data.Aeson           (decode, encode, toJSON)
import Data.Monoid          (mempty)
import Data.Patch           (Patch, Edit(..), toList, fromList, apply, diff, transformWith)
import Data.Sequence          (Seq, (|>))
import Data.Foldable        as Foldable
import qualified Data.Sequence as Seq
import Data.Text            (Text)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.IO as T
import Data.Proxy (Proxy(..))
import Data.Monoid (Monoid)
import Happstack.Server (Response, ServerPartT, Browsing(EnableBrowsing), asContentType, dir, simpleHTTP, nullConf, nullDir, serveDirectory, serveFile, seeOther, toResponse)
import Happstack.Server.WebSockets (runWebSocketsHappstack)
-- import Servant.API hiding (Patch)
-- import Servant.Happstack
import Web.Editor.API (WebSocketReq(..), WebSocketRes(..), WSRequest(..))
import Network.WebSockets (Connection, ServerApp, acceptRequest, receiveData, sendBinaryData, sendTextData)

-- instance Monoid ServantErr

data ServerState = ServerState
 { nextConnNum :: ConnectionId
 , connections :: [(ConnectionId, Connection)]
 , document    :: Document
 }

initialServerState :: ServerState
initialServerState =
 ServerState { nextConnNum = 0
             , connections = []
             , document    = emptyDocument
             }
{-
editorAPI :: Proxy EditorAPI
editorAPI = Proxy
-}
-- patchPost thePatch = pure ()

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
                   liftIO $ putStrLn (C.unpack bs)
                   let mReq = (decode bs) :: Maybe (WebSocketReq)
                   liftIO $ print mReq
                   case mReq of
                     Nothing -> pure ()
                     (Just req) ->
                       handleReq tvServerState conn i req)
--                          mapM_ (handleReq tvServerState conn i) reqs)
       `finally` (do putStrLn $ "Removing connection: " ++ show i
                     atomically $ modifyTVar' tvServerState (\ss -> ss { connections = filter (\(n,_) -> n /= i) (connections ss) }))
     --             sendTextData conn t

-- FIXME: patches need to have their order number attached to the response
-- FIXME: we probably have a race condition -- calculating the
-- newPatch and the patch number needs to be done inside the
-- atomically

handleReq :: TVar ServerState
          -> Connection
          -> ConnectionId
          -> WebSocketReq
          -> IO ()
handleReq tvServerState connection connectionId (WebSocketReq req) =
  case req of
    (ReqAddPatch forkedAt patchCandidate) ->
      do (conns, r) <- atomically $
           do ss <- readTVar tvServerState
              let c = connections ss
                  d = document ss
              let serverPatch = mconcat (Foldable.toList $ Seq.drop (forkedAt + 1) (d ^. patches))
                  (_, newPatch) = transformWith merge serverPatch patchCandidate
                  doc' = d { _patches = (d ^. patches) |> newPatch }
                  i    = Seq.length (doc' ^. patches) - 1
              writeTVar tvServerState  (ss { document = doc'})
              pure (c, (i, newPatch))
         let msg = Builder.toLazyText (encodeToTextBuilder (toJSON (ResAppendPatch connectionId r)))
         mapM_ (\(_, conn) -> sendTextData conn msg) conns
    ReqInit ->
      do document <- atomically $ do document <$> readTVar tvServerState
         let msg = Builder.toLazyText (encodeToTextBuilder (toJSON (ResInit connectionId document)))
         sendTextData connection msg
{-
    (WebSocketReq (ReqUpdateCurrent edits)) ->
      do conns <- atomically (connections <$> readTVar tvServerState)
         let msg = Builder.toLazyText (encodeToTextBuilder (toJSON (ResUpdateCurrent connectionId edits)))
         mapM_ (\(_, conn) -> sendTextData conn msg) conns
    (WebSocketReq (ReqAddPatch patch)) ->
      do conns <- atomically (connections <$> readTVar tvServerState)
         let msg = Builder.toLazyText (encodeToTextBuilder (toJSON (ResAppendPatch connectionId patch)))
         mapM_ (\(_, conn) -> sendTextData conn msg) conns
-}
websockets :: (MonadIO m) =>
              TVar ServerState
           -> ServerPartT m Response
websockets tvServerState = runWebSocketsHappstack (editorApp tvServerState)

editorServer :: (MonadIO m, MonadPlus m) => TVar ServerState -> ServerPartT m Response
editorServer tvServerState = dir "editor" $ serveClient
  where
    serveClient :: (MonadIO m, MonadPlus m) => ServerPartT m Response
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

mkEditor :: (MonadIO m, MonadPlus m) => IO (ServerPartT m Response)
mkEditor =
  do tvServerState <- atomically (newTVar initialServerState)
     pure $ editorServer tvServerState
