module Main where

import WebsocketServer (newServerState, onConnect)
import Control.Concurrent (newMVar)

import qualified Network.WebSockets as WS


main :: IO ()
main = do
    state <- newMVar newServerState
    let req = "asdf"
    -- TODO: Get the actual request from WAI somehow
    WS.runServer "127.0.0.1" 9160 $ onConnect req state
