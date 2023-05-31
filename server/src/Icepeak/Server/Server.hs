{-# LANGUAGE OverloadedStrings #-}
module Icepeak.Server.Server
(
  runServer,
)
where

import Data.Text (pack)

import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (isWebSocketsReq, websocketsOr)

import qualified Network.Wai.Handler.Warp as Warp

import Icepeak.Server.Logger (Logger, LogLevel(..), postLog)
import Icepeak.Server.WebsocketServer (WSServerApp, wsConnectionOpts, mkWSServerOptions)

runServer :: Logger -> WSServerApp -> Application -> Int -> IO ()
runServer logger wsApp httpApp port = do
  postLog logger LogInfo $ pack $ "Listening on port " <> show port <> "."
  Warp.run port $ \req response -> if isWebSocketsReq req
    then do
      wsOptions <- mkWSServerOptions
      -- This should never fall back to 'httpApp' since we already confirmed
      -- that this is a websocket connection, but the direct 'websocketsApp'
      -- returns a @Maybe Application@ so this is more robust
      websocketsOr (wsConnectionOpts wsOptions) (wsApp wsOptions) httpApp req response
    else httpApp req response
