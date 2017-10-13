{-# LANGUAGE OverloadedStrings #-}
module Server
(
  runServer,
)
where

import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets

import Logger (Logger, postLog)

runServer :: Logger -> ServerApp -> Application -> IO ()
runServer logger wsApp httpApp =
  let
    wsConnectionOpts = WebSockets.defaultConnectionOptions
  in do
    postLog logger "Listening on port 3000."
    Warp.run 3000 $ websocketsOr wsConnectionOpts wsApp httpApp
