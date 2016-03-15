module Main where

import Control.Monad        (MonadPlus, msum)
import Control.Monad.Trans  (MonadIO)
import Data.Text            (Text)
import Data.Proxy (Proxy(..))
import Data.Monoid (Monoid)
import Happstack.Server (Response, ServerPartT, Browsing(EnableBrowsing), asContentType, dir, simpleHTTP, nullConf, nullDir, serveDirectory, serveFile, seeOther, toResponse)
import Servant.API
import Servant.Happstack
import Web.Editor.API (EditorAPI)

instance Monoid ServantErr

editorAPI :: Proxy EditorAPI
editorAPI = Proxy

editorServer :: Server EditorAPI
editorServer = serveClient
  where
    serveClient :: (MonadPlus m, MonadIO m) => ServerPartT m Response
    serveClient =
      let basePath = "../editor-client/.cabal-sandbox/bin/editor-client.jsexe/"
      in  msum [ nullDir >> seeOther ("/editor/index.html" :: String) (toResponse ())
               , dir "data" $ serveDirectory EnableBrowsing [] "data"
               , dir "style.css" $ serveFile (asContentType "text/css") ("../editor-client/style.css")
               , dir "index.html" $ serveFile (asContentType "text/html") "data/index.html"
               , dir "rts.js" $ serveFile (asContentType "application/javascript") (basePath ++ "rts.js")
               , dir "out.js" $ serveFile (asContentType "application/javascript") (basePath ++ "out.js")
               , dir "lib.js" $ serveFile (asContentType "application/javascript")  (basePath ++ "lib.js")
               , dir "runmain.js" $ serveFile (asContentType "application/javascript") (basePath ++ "runmain.js")
               ]
main :: IO ()
main = simpleHTTP nullConf $ serve editorAPI editorServer
