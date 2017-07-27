module Main where

import WebsocketServer
import Control.Concurrent (newMVar)

import qualified Network.WebSockets as WS

-- main :: IO ()
-- main = putStrLn "I'm a websocket"

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
