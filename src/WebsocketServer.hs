{-# LANGUAGE OverloadedStrings #-}

module WebsocketServer (
  ServerState,
  acceptConnection,
  broadcast,
  newServerState,
) where

import Control.Concurrent (MVar, modifyMVar_, yield)
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

import Store (Path)
import Subscription (SubscriptionTree)

import qualified Subscription

type ServerState = SubscriptionTree UUID WS.Connection

newUUID :: IO UUID
newUUID = randomIO

newServerState :: ServerState
newServerState = Subscription.empty

broadcast :: [Text] -> Value -> ServerState -> IO ()
broadcast =
  let
    send value conn = do
      putStrLn $ "Sending binary data ..."
      WS.sendBinaryData conn (Aeson.encode value)
  in
    Subscription.broadcast send

-- Called for each new client that connects.
acceptConnection :: MVar ServerState -> WS.PendingConnection -> IO ()
acceptConnection state pending = do
  printRequest pending
  -- TODO: Validate the path and headers of the pending request
  let path = fst $ Uri.decodePath $ WS.requestPath $ WS.pendingRequest pending
  conn <- WS.acceptRequest pending
  -- fork a pinging thread, because browsers...
  WS.forkPingThread conn 30
  handleClient conn path state

handleClient :: WS.Connection -> Path -> MVar ServerState -> IO ()
handleClient conn path state = do
  uuid <- newUUID
  let
    onConnect = do
      modifyMVar_ state (pure . Subscription.subscribe path uuid conn)
      putStrLn $ "Client " ++ (show uuid) ++ " connected, subscribed to " ++ (show path) ++ "."
    onDisconnect = do
      modifyMVar_ state (pure . Subscription.unsubscribe path uuid)
      putStrLn $ "Client " ++ (show uuid) ++ " disconnected."
    -- We don't send any messages here; sending is done by the update
    -- loop; it finds the client in the set of subscriptions. But we do
    -- need to keep the thread running, otherwise the connection will be
    -- closed. So we go into an infinite loop here.
    keepAlive = forever yield
  -- Put the client in the subscription tree and keep the connection open.
  -- Remove it when the connection is closed.
  finally (onConnect >> keepAlive) onDisconnect

-- Print the path and headers of the pending request
printRequest :: WS.PendingConnection -> IO ()
printRequest pending = do
   putStrLn $ show ("\nPath: " <> (WS.requestPath $ WS.pendingRequest pending))
   let headers = WS.requestHeaders $ WS.pendingRequest pending
   Text.putStrLn "Headers:"
   forM_ headers print
