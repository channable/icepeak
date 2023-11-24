module Icepeak.Server.WebsocketServer.HandleClientMultiSubscription where

import Control.Concurrent (modifyMVar_, newEmptyMVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, takeMVar, tryTakeMVar, newMVar, readMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, finally, fromException, throwIO)
import Control.Monad (forever)
import Data.Aeson (Value)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.UUID (UUID)
import System.Random (randomIO)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS

import Icepeak.Server.Core (Core, coreClients, withCoreMetrics, SubscriberState (SubscriberStateNew))
import qualified Icepeak.Server.Metrics as Metrics
import qualified Icepeak.Server.Subscription as Subscription

-- * Client handling

newUUID :: IO UUID
newUUID = randomIO

data ClientPayload
  = Subscribe   { subscribePath :: [Text], token :: Text }
  | UnSubscribe { unsubscribePath :: [Text] }

instance Aeson.FromJSON ClientPayload where
  parseJSON = undefined

data ClientPayloadParsed
  = ClientPayloadSuccessfulParse ClientPayload
  | ClientPayloadFailedParse

clientPayloadFromByteString :: LBS.ByteString -> ClientPayloadParsed
clientPayloadFromByteString byteString = 
  case Aeson.eitherDecode byteString of
      (Left _jsonDecodeError) -> ClientPayloadFailedParse
      (Right clientPayload)   -> ClientPayloadSuccessfulParse clientPayload

clientPayloadFromDataMessage :: WS.DataMessage -> ClientPayloadParsed
clientPayloadFromDataMessage (WS.Text encodedUtf8ByteString _mbDecodedCache) =
  clientPayloadFromByteString encodedUtf8ByteString
clientPayloadFromDataMessage (WS.Binary _) = ClientPayloadFailedParse
  
authorisePathSubscription :: [Text] -> Text -> IO Bool
authorisePathSubscription = undefined

handleClient :: WS.Connection -> Core -> IO ()
handleClient conn core = do
  uuid <- newUUID

  isDirtyMVar <- newMVar ()
  subscribedPathsMVar <- newMVar (HashMap.empty :: HashMap [Text] (MVar Value))

  let
    serverStateMVar = coreClients core

    onConnect = withCoreMetrics core Metrics.incrementSubscribers

    onDisconnect = do
      paths <-  HashMap.keys <$> takeMVar subscribedPathsMVar
      for_ paths
        (\path -> modifyMVar_ serverStateMVar
          (pure . Subscription.unsubscribe path uuid))
      withCoreMetrics core Metrics.decrementSubscribers

    onPayload (ClientPayloadSuccessfulParse (Subscribe newPath pathToken)) = do
      isAuthorised <- authorisePathSubscription newPath pathToken

      case isAuthorised of
        False -> undefined
        True -> do
          pathValueMVar <- newEmptyMVar
          modifyMVar_ subscribedPathsMVar
            (pure . HashMap.insert newPath pathValueMVar)
          modifyMVar_ serverStateMVar
            (pure . Subscription.subscribe newPath uuid
              (SubscriberStateNew (pathValueMVar, isDirtyMVar)))

    onPayload (ClientPayloadSuccessfulParse (UnSubscribe unsubPath)) = do
      modifyMVar_ serverStateMVar
        (pure . Subscription.unsubscribe unsubPath uuid)
      modifyMVar_ subscribedPathsMVar
        (pure . HashMap.delete unsubPath)

    onPayload ClientPayloadFailedParse = pure ()

    takeMVarNewValues = do
      takeMVar isDirtyMVar
      valueMVars <- HashMap.elems <$> readMVar subscribedPathsMVar
      catMaybes <$> mapM tryTakeMVar valueMVars

    manageConnection = withAsync
      (updateThread conn takeMVarNewValues)
      (const $ forever
        $ WS.receiveDataMessage conn
        >>= (pure . clientPayloadFromDataMessage)
        >>= onPayload)

    -- Simply ignore connection errors, otherwise, Warp handles the exception
    -- and sends a 500 response in the middle of a WebSocket connection, and
    -- that violates the WebSocket protocol.
    -- Note that subscribers are still properly removed by the finally below.
    handleConnectionError :: WS.ConnectionException -> IO ()
    handleConnectionError _ = pure ()
  -- Put the client in the subscription tree and keep the connection open.
  -- Remove it when the connection is closed.
  finally (onConnect >> manageConnection) onDisconnect
    `catch` handleConnectionError


-- This function handles sending the updates to subscribers.
updateThread :: WS.Connection -> IO [Value] -> IO ()
updateThread conn takeMVarNewValues =
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
      value <- takeMVarNewValues
      mapM send value
