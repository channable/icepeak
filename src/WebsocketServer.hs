{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID
import System.Random (randomIO)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.WebSockets as WS

type ServerState = SubscriptionTree

-- Keeps subscriptions in a tree data structure, so we can efficiently determine
-- which clients need to be notified for a given update.
data SubscriptionTree =
  SubscriptionTree (HashMap UUID WS.Connection) (HashMap Text SubscriptionTree)

emptySubsTree :: SubscriptionTree
emptySubsTree = SubscriptionTree HashMap.empty HashMap.empty

subscribe :: [Text] -> UUID -> WS.Connection -> SubscriptionTree -> SubscriptionTree
subscribe path uuid conn (SubscriptionTree here inner) =
  case path of
    [] -> SubscriptionTree (HashMap.insert uuid conn here) inner
    key : pathTail ->
      let
        subscribeInner = subscribe pathTail uuid conn
        newInner = HashMap.alter (Just . subscribeInner . fromMaybe emptySubsTree) key inner
      in
        SubscriptionTree here newInner

unsubscribe :: [Text] -> UUID -> SubscriptionTree -> SubscriptionTree
unsubscribe path uuid (SubscriptionTree here inner) =
  case path of
    [] -> SubscriptionTree (HashMap.delete uuid here) inner
    key : pathTail ->
      let
        unsubscribeInner = unsubscribe pathTail uuid
        newInner = HashMap.adjust unsubscribeInner key inner
        -- TODO: Prune empty branches in the tree.
      in
        SubscriptionTree here newInner

newUUID :: IO UUID
newUUID = randomIO

newServerState :: ServerState
newServerState = emptySubsTree

broadcast :: [Text] -> Value -> ServerState -> IO ()
broadcast path value (SubscriptionTree here inner) = do
  case path of
    -- When the path is empty, all subscribers that are "here" or at a deeper
    -- level should receive a notification.
    [] -> do
      forM_ here $ \ conn -> do
        putStrLn $ "Sending binary data ..."
        WS.sendBinaryData conn (Aeson.encode value)
      -- TODO: Trim the value to the key.
      forM_ inner $ broadcast [] value
    key : pathTail -> case HashMap.lookup key inner of
      Nothing -> pure ()
      -- TODO: Extract the inner thing from the value as well; the client is not
      -- subscribed to the top-level thing after all.
      Just subs -> broadcast pathTail value subs

-- Called for each new client that connects.
acceptConnection :: MVar ServerState -> WS.PendingConnection -> IO ()
acceptConnection state pending = do
  printRequest pending
  -- accept the connection
  -- TODO: Validate the path and headers of the pending request
  conn <- WS.acceptRequest pending
  -- fork a pinging thread, because browsers...
  WS.forkPingThread conn 30
  handleFirstMessage conn state

handleFirstMessage :: WS.Connection -> MVar ServerState -> IO ()
handleFirstMessage conn state = do
  msg <- WS.receiveData conn
  uuid <- newUUID
  let
    -- TODO: Use a proper url reader. Url decode the parts, allow empty strings,
    -- to strip the root leading slash, etc. Also, read until a newline, rather
    -- than an arbitrary end of buffer?
    path = filter (not . Text.null) $ Text.split (== '/') msg
    onConnect = do
      modifyMVar_ state (pure . subscribe path uuid conn)
      putStrLn $ "Client " ++ (show uuid) ++ " connected, subscribed to " ++ (show path) ++ "."
    onDisconnect = do
      modifyMVar_ state (pure . unsubscribe path uuid)
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
