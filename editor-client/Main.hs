{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)
import SimpleEditor

main :: IO ()
main =
  do forkIO $ chili app initAction (initModel "murv") "ws://localhost:8000/editor/websockets" handleMessage "murv"
     forkIO $ chili app initAction (initModel "murv2") "ws://localhost:8000/editor/websockets" handleMessage "murv2"
     pure ()

