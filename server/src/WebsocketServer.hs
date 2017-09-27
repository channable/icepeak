{-# LANGUAGE OverloadedStrings #-}

module WebsocketServer (
  ServerState,
  acceptConnection,
  processUpdates
) where

import Control.Concurrent (modifyMVar_, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson (Value)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID
import Prelude hiding (log)
import System.Random (randomIO)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.Clock.POSIX as Clock
import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types.Header as HttpHeader
import qualified Network.HTTP.Types.URI as Uri

import Config (Config (..))
import Core (Core (..), ServerState, Updated (..), getCurrentValue)
import Logger (log)
import Store (Path)
import AccessControl (AccessMode(..))
import JwtMiddleware (AuthResult (..), isRequestAuthorized, errorResponseBody)

import qualified Subscription

newUUID :: IO UUID
newUUID = randomIO

-- send the updated data to all subscribers to the path
broadcast :: [Text] -> Value -> ServerState -> IO ()
broadcast =
  let
    send :: WS.Connection -> Value -> IO ()
    send conn value = do
      WS.sendTextData conn (Aeson.encode value)
  in
    Subscription.broadcast send

-- Called for each new client that connects.
acceptConnection :: Core -> WS.PendingConnection -> IO ()
acceptConnection core pending = do
  -- printRequest pending
  -- TODO: Validate the path and headers of the pending request
  authResult <- authorizePendingConnection core pending
  case authResult of
    AuthRejected err ->
      WS.rejectRequestWith pending $ WS.RejectRequest
        { WS.rejectCode = 401
        , WS.rejectMessage = "Unauthorized"
        , WS.rejectHeaders = [(HttpHeader.hContentType, "application/json")]
        , WS.rejectBody = LBS.toStrict $ errorResponseBody err
        }
    AuthAccepted -> do
      let path = fst $ Uri.decodePath $ WS.requestPath $ WS.pendingRequest pending
      conn <- WS.acceptRequest pending
      -- Fork a pinging thread to keep idle connections open and to detect closed connections.
      -- Note: The thread dies silently if the connection crashes or is closed.
      WS.forkPingThread conn 30
      handleClient conn path core

-- * Authorization

authorizePendingConnection :: Core -> WS.PendingConnection -> IO AuthResult
authorizePendingConnection core conn
  | configEnableJwtAuth (coreConfig core) = do
      now <- Clock.getPOSIXTime
      let req = WS.pendingRequest conn
          (path, query) = Uri.decodePath $ WS.requestPath req
          headers = WS.requestHeaders req
      return $ isRequestAuthorized headers query now (configJwtSecret (coreConfig core)) path ModeRead
  | otherwise = pure AuthAccepted

-- * Client handling

handleClient :: WS.Connection -> Path -> Core -> IO ()
handleClient conn path core = do
  uuid <- newUUID
  let
    state = coreClients core
    logRecords = coreLogRecords core
    onConnect = do
      modifyMVar_ state (pure . Subscription.subscribe path uuid conn)
      log (T.pack $ "Client " ++ (show uuid) ++ " connected, subscribed to " ++ (show path) ++ ".") logRecords
    onDisconnect = do
      modifyMVar_ state (pure . Subscription.unsubscribe path uuid)
      log (T.pack $ "Client " ++ (show uuid) ++ " disconnected.") logRecords
    sendInitialValue = do
      currentValue <- getCurrentValue core path
      log (T.pack $ "Sending initial value to client: " ++ show currentValue) logRecords
      WS.sendTextData conn (Aeson.encode currentValue)
  -- Put the client in the subscription tree and keep the connection open.
  -- Remove it when the connection is closed.
  finally (onConnect >> sendInitialValue >> keepTalking conn) onDisconnect

-- We don't send any messages here; sending is done by the update
-- loop; it finds the client in the set of subscriptions. But we do
-- need to keep the thread running, otherwise the connection will be
-- closed. So we go into an infinite loop here.
keepTalking :: WS.Connection -> IO ()
keepTalking conn = forever $ do
    -- Note: WS.receiveDataMessage will handle control messages automatically and e.g.
    -- do the closing handshake of the websocket protocol correctly
    WS.receiveDataMessage conn

-- Print the path and headers of the pending request
printRequest :: WS.PendingConnection -> IO ()
printRequest pending = do
  putStrLn $ show ("\nPath: " <> (WS.requestPath $ WS.pendingRequest pending))
  let headers = WS.requestHeaders $ WS.pendingRequest pending
  T.putStrLn "Headers:"
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
          go
        -- Stop the loop when we receive a Nothing.
        Nothing -> pure ()
