{-# LANGUAGE OverloadedStrings #-}
module Server
(
  runServer,
)
where

import Data.Semigroup ((<>))
import Data.Text (pack)

import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets

import Logger (Logger, LogLevel(..), postLog)

runServer :: Logger -> ServerApp -> Application -> Int -> IO ()
runServer logger wsApp httpApp port =
  let
    wsConnectionOpts = WebSockets.defaultConnectionOptions
  in do
    postLog logger LogInfo $ pack $ "Listening on port " <> show port <> "."
    Warp.run port $ websocketsOr wsConnectionOpts wsApp httpApp
