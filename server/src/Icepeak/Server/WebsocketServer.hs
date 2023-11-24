{-# LANGUAGE OverloadedStrings #-}

module Icepeak.Server.WebsocketServer (
  WSServerApp,
  -- The constructor is intentionally not exposed since the 'TimeSpec' should be
  -- initialized using the monotonic clock
  WSServerOptions(),
  wsConnectionOpts,
  mkWSServerOptions,

  ServerState,
  acceptConnection,
  processUpdates
) where

import Control.Concurrent (readMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (MVar, putMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Exception (AsyncException, fromException, handle, throwIO)
import Control.Monad (unless, void, when)
import Data.Aeson (Value)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicWriteIORef, readIORef, newIORef)
import Data.Text (Text)
import System.Clock (Clock (Monotonic), TimeSpec (..), getTime)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Clock
import qualified Network.HTTP.Types.Header as HttpHeader
import qualified Network.HTTP.Types.URI as Uri
import qualified Network.WebSockets as WS

import Icepeak.Server.Config (Config (..))
import Icepeak.Server.Core (Core (..), ServerState, SubscriberState (..), Updated (..))
import Icepeak.Server.AccessControl (AccessMode(..))
import Icepeak.Server.JwtMiddleware (AuthResult (..), isRequestAuthorized, errorResponseBody)

import qualified Icepeak.Server.WebsocketServer.SingleSubscription as SingleSubscription
import qualified Icepeak.Server.WebsocketServer.MultiSubscription as MultiSubscription

import qualified Icepeak.Server.Metrics as Metrics
import qualified Icepeak.Server.Subscription as Subscription
import Data.Maybe (isJust)
import System.Timeout (timeout)

-- | 'WS.ServerApp' parameterized over the last received pong timestamp. See
-- 'WSServerOptions'.
type WSServerApp = WSServerOptions -> WS.ServerApp

-- | Any additional payload information information used by the websocket
-- application.
--
-- Currently this is only used to keep track of the last received pong. This
-- value is initialized to the current time when starting the application, and
-- it is updated in a connection-specific @connectionOnPong@ handler. When a
-- ping is sent, the IO action in 'withInterruptiblePingThread' checks whether
-- the client answered our last ping with a pong. If it hasn't, then the server
-- will terminate the websocket connection as timeouts would otherwise leave
-- zombie connections.
newtype WSServerOptions = WSServerOptions { wsOptionLastPongTime :: IORef TimeSpec }

-- | Builds the /per connection/-connection settings. This hooks up the
-- connection's pong handler to the last received pong time IORef so timeouts
-- can be detected in the ping thread's handlers.
wsConnectionOpts :: WSServerOptions -> WS.ConnectionOptions
wsConnectionOpts wsOptions =
  WS.defaultConnectionOptions
    { WS.connectionOnPong = pongHandler wsOptions
    }

-- | Initialize a 'mkWSServerOptions' so it can be used for timeout tracking.
-- See 'WSServerOptions' for more information.
mkWSServerOptions :: IO WSServerOptions
mkWSServerOptions = do
  -- See 'WSServerOptions' for more information, but this is used to keep track
  -- of when the last pong was received so the websocket connection can be
  -- terminated if the client stops sending them. It's initialized with the
  -- current time so this also works as expected if the client never sends a
  -- single pong.
  lastPongTime <- getTime Monotonic
  WSServerOptions <$> newIORef lastPongTime

-- send the updated data to all subscribers to the path
broadcast :: Core -> [Text] -> Value -> ServerState -> IO ()
broadcast core =
  let
    writeToSub :: MVar Value -> Value -> IO ()
    writeToSub queue val = do
      -- We are the only producer, so either the subscriber already
      -- read the value or we can discard it to replace it with the
      -- new one. We don't need atomicity for this operation.
      -- `tryTakeMVar` basically empties the MVar, from this perspective.
      mbQueue <- tryTakeMVar queue
      -- If the MVar has not yet been read by the subscriber thread, it means
      -- that the update has been skipped.
      Control.Monad.when (isJust mbQueue) $ for_ (coreMetrics core) Metrics.incrementWsSkippedUpdates
      putMVar queue val

    modifySubscriberState (SubscriberStateOld subscriberState) newValue =
      writeToSub subscriberState newValue
    modifySubscriberState (SubscriberStateNew (subscriberState, isDirtyMVar)) newValue =
      do writeToSub subscriberState newValue
         Control.Monad.void $ tryPutMVar isDirtyMVar ()

  in Subscription.broadcast modifySubscriberState

-- loop that is called for every update and that broadcasts the values to all
-- subscribers of the updated path
processUpdates :: Core -> IO ()
processUpdates core = go
  where
    go = do
      maybeUpdate <- atomically $ readTBQueue (coreUpdates core)
      for_ (coreMetrics core) Metrics.incrementWsQueueRemoved
      case maybeUpdate of
        Just (Updated path value) -> do
          clients <- readMVar (coreClients core)
          broadcast core path value clients
          go
        -- Stop the loop when we receive a Nothing.
        Nothing -> pure ()

-- Called for each new client that connects.
acceptConnection :: Core -> WSServerOptions -> WS.PendingConnection -> IO ()
acceptConnection core wsOptions pending = do
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
          config = coreConfig core
          pingInterval = configWebSocketPingInterval config
          onPing = pingHandler config wsOptions
      connection <- WS.acceptRequest pending
      -- Fork a pinging thread, for each client, to keep idle connections open and to detect
      -- closed connections. Sends a ping message every 30 seconds.
      -- Note: The thread dies silently if the connection crashes or is closed.
      withInterruptiblePingThread connection pingInterval onPing
        $ SingleSubscription.handleClient connection path core

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

-- * Timeout handling
--
-- The websockets library lets you send pings, but it has no built in way to
-- terminate clients that never send a pong back. We implement this ourselves by
-- keeping track of when we last received a pong, and then terminating the
-- connection if the time between the last sent ping and the last received pong
-- exceeds a certain threshold. See 'WSServerOptions' for more information

-- | The connection-specific on-pong handler. This writes the time at which the
-- last pong was received so 'pingHandler' can terminate the connection if the
-- client stops sending pongs back.
pongHandler :: WSServerOptions -> IO ()
pongHandler (WSServerOptions lastPongTime) = getTime Monotonic >>= Control.Monad.void . atomicWriteIORef lastPongTime

-- | An action passed to 'withInterruptiblePingThread' that is used together with
-- 'pongHandler' to terminate a WebSocket connection if the client stops sending
-- timely pongs. This returns @True@ if the connection has timed out and should
-- be terminated.
pingHandler :: Config -> WSServerOptions -> IO Bool
pingHandler config (WSServerOptions lastPongTime) = do
  now <- getTime Monotonic
  let pingInterval     = TimeSpec (fromIntegral $ configWebSocketPingInterval config) 0
      pongTimeout      = TimeSpec (fromIntegral $ configWebSocketPongTimeout config) 0
      lastPongDeadline = now - pingInterval - pongTimeout

  lastPong <- readIORef lastPongTime
  return $! lastPong < lastPongDeadline

-- | Similar to 'WS.withPingThread', except that it uses a combination of
-- 'pingHandler' and the 'pongHandler' set in the websocket connection's pong
-- handler to detect that the thread client stopped sending pongs. If that
-- happens the @app@ action will be canceled immediately.
--
-- The @pingAction@ is exected on every ping, and it should return @True@ if the
-- client has timed out and the connection should be terminated.
withInterruptiblePingThread :: WS.Connection -> Int -> IO Bool -> IO () -> IO ()
withInterruptiblePingThread conn pingInterval pingAction
  | pingInterval <= 0 = id
  | otherwise = race_ (interruptiblePingThread conn pingInterval pingAction)

-- | This is based on 'WS.pingThread', with the following differences:
--
--   * Instead of running an `IO ()` action after a ping is sent, this uses an
--     `IO Bool` action. If that action returns true then the connection is to
--     be considered timed out and this function will return.
--   * The check for `pingInterval` being 0 or less has been moved to
--     `withInterruptiblePingThread` to avoid spawning threads when unnecessary.
--     Still calling this function with a zero or negative ping interval will
--     not break anything, although it will cause it to spam pings.
interruptiblePingThread :: WS.Connection -> Int -> IO Bool -> IO ()
interruptiblePingThread conn pingInterval pingAction = ignore `handle` go 1
  where
    pingIntervalUs :: Int
    pingIntervalUs = pingInterval * 1000 * 1000

    go :: Int -> IO ()
    go i = do
      threadDelay pingIntervalUs
      -- If the send buffer is full (e.g. because we pushed a lot of updates to
      -- a client that's timed out) then this send will block indefinitely.
      -- Adding the timeout here prevents this from happening, and it also
      -- interacts nicely with the @pingAction@.
      _ <- timeout pingIntervalUs $ WS.sendPing conn (T.pack $ show i)
      -- The difference with the original 'pingThread' is that this action now
      -- returns a boolean, and we'll terminate this thread when that action
      -- returns true
      hasTimedOut <- pingAction
      Control.Monad.unless hasTimedOut $ go (i + 1)

    -- The rest of this function is exactly the same as the 'pingThread' in
    -- @websockets-0.12.7.3@
    ignore e = case fromException e of
      Just async -> throwIO (async :: AsyncException)
      Nothing -> return ()
