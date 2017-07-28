{-# LANGUAGE OverloadedStrings #-}

module WebsocketServer (
  ServerState,
  acceptConnection,
  processUpdates
) where

import Control.Concurrent (modifyMVar_, yield, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson (Value)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID
import System.Random (randomIO)

import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as Text
import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types.URI as Uri

import Core (Core (..), ServerState, Updated (..), getCurrentValue)
import Store (Path)

import qualified Subscription

newUUID :: IO UUID
newUUID = randomIO

-- send the updated data to all subscribers to the path
broadcast :: [Text] -> Value -> ServerState -> IO ()
broadcast =
  let
    send :: Value -> WS.Connection -> IO ()
    send value conn = do
      putStrLn $ "Sending binary data ..."
      WS.sendBinaryData conn (Aeson.encode value)
  in
    Subscription.broadcast send

-- Called for each new client that connects.
acceptConnection :: Core -> WS.PendingConnection -> IO ()
acceptConnection core pending = do
  printRequest pending
  -- TODO: Validate the path and headers of the pending request
  let path = fst $ Uri.decodePath $ WS.requestPath $ WS.pendingRequest pending
  conn <- WS.acceptRequest pending
  -- fork a pinging thread, because browsers...
  WS.forkPingThread conn 30
  handleClient conn path core

handleClient :: WS.Connection -> Path -> Core -> IO ()
handleClient conn path core = do
  uuid <- newUUID
  let
    state = coreClients core
    onConnect = do
      modifyMVar_ state (pure . Subscription.subscribe path uuid conn)
      putStrLn $ "Client " ++ (show uuid) ++ " connected, subscribed to " ++ (show path) ++ "."
    onDisconnect = do
      modifyMVar_ state (pure . Subscription.unsubscribe path uuid)
      putStrLn $ "Client " ++ (show uuid) ++ " disconnected."
    sendInitialValue = do
      currentValue <- getCurrentValue core path
      putStrLn $ "Sending initial value to client: " ++ show currentValue
      WS.sendBinaryData conn (Aeson.encode currentValue)
    -- We don't send any messages here; sending is done by the update
    -- loop; it finds the client in the set of subscriptions. But we do
    -- need to keep the thread running, otherwise the connection will be
    -- closed. So we go into an infinite loop here.
    keepAlive = forever yield
  -- Put the client in the subscription tree and keep the connection open.
  -- Remove it when the connection is closed.
  finally (onConnect >> sendInitialValue >> keepAlive) onDisconnect

-- Print the path and headers of the pending request
printRequest :: WS.PendingConnection -> IO ()
printRequest pending = do
   putStrLn $ show ("\nPath: " <> (WS.requestPath $ WS.pendingRequest pending))
   let headers = WS.requestHeaders $ WS.pendingRequest pending
   Text.putStrLn "Headers:"
   forM_ headers print

-- loop that is called for every update and that broadcasts the values to all
-- subscribers of the updated path
processUpdates :: Core -> IO ()
processUpdates core = go
  where
    go = do
      maybeUpdate <- atomically $ readTBQueue (coreUpdates core)
      case maybeUpdate of
        Just (Updated path value) -> do
          clients <- readMVar (coreClients core)
          broadcast path value clients
          putStrLn $ "Update at " ++ (show path) ++ ", new value: " ++ (show value)
          go
        -- Stop the loop when we receive a Nothing.
        Nothing -> pure ()
