module Icepeak.Server.WebsocketServer.SingleSubscription (handleClient) where

import Control.Concurrent (modifyMVar_, newEmptyMVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, finally, fromException, throwIO)
import Control.Monad (forever)
import Data.Aeson (Value)

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import Icepeak.Server.Store (Path)
import Icepeak.Server.Core (Core, coreClients, withCoreMetrics, getCurrentValue)

import qualified Icepeak.Server.Metrics as Metrics
import qualified Icepeak.Server.Subscription as Subscription
import qualified Icepeak.Server.WebsocketServer.Utils as Utils

-- * Client handling

handleClient :: WS.Connection -> Path -> Core -> IO ()
handleClient conn path core = do
  uuid <- Utils.newUUID
  pathCurentValueMVar <- newEmptyMVar
  let
    state = coreClients core
    onConnect = do
      modifyMVar_ state
        (pure . Subscription.subscribe path uuid
         (\writeToSub -> writeToSub pathCurentValueMVar))
      withCoreMetrics core Metrics.incrementSubscribers
    onDisconnect = do
      modifyMVar_ state (pure . Subscription.unsubscribe path uuid)
      withCoreMetrics core Metrics.decrementSubscribers
    sendInitialValue = do
      currentValue <- getCurrentValue core path
      WS.sendTextData conn (Aeson.encode currentValue)
    -- For each connection, we want to spawn a client thread with an associated
    -- queue, in order to manage subscribers. `withAsync` acts as `forkIO` in this
    -- context, with the assurance the child thread is killed when the parent is.
    manageConnection = withAsync (updateThread conn pathCurentValueMVar)
                                 (const $ keepTalking conn)

    -- Simply ignore connection errors, otherwise, Warp handles the exception
    -- and sends a 500 response in the middle of a WebSocket connection, and
    -- that violates the WebSocket protocol.
    -- Note that subscribers are still properly removed by the finally below.
    handleConnectionError :: WS.ConnectionException -> IO ()
    handleConnectionError _ = pure ()
  -- Put the client in the subscription tree and keep the connection open.
  -- Remove it when the connection is closed.
  finally (onConnect >> sendInitialValue >> manageConnection) onDisconnect
    `catch` handleConnectionError

-- This function handles sending the updates to subscribers.
updateThread :: WS.Connection -> MVar Value -> IO ()
updateThread conn state =
  let
    send :: Value -> IO ()
    send value =
      WS.sendTextData conn (Aeson.encode value)
      `catch`
      sendFailed

    sendFailed :: SomeException -> IO ()
    sendFailed exc
      -- Rethrow async exceptions, they are meant for inter-thread communication
      -- (e.g. ThreadKilled) and we don't expect them at this point.
      | Just asyncExc <- fromException exc = throwIO (asyncExc :: SomeAsyncException)
      -- We want to catch all other errors in order to prevent them from
      -- bubbling up and disrupting the broadcasts to other clients.
      | otherwise = pure ()
  in forever $ do
      value <- takeMVar state
      send value

-- We don't send any messages here; sending is done by the update
-- loop; it finds the client in the set of subscriptions. But we do
-- need to keep the thread running, otherwise the connection will be
-- closed. So we go into an infinite loop here.
keepTalking :: WS.Connection -> IO ()
keepTalking conn = forever $ do
    -- Note: WS.receiveDataMessage will handle control messages automatically and e.g.
    -- do the closing handshake of the websocket protocol correctly
    WS.receiveDataMessage conn

