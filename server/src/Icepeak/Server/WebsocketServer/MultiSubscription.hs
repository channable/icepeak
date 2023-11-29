{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Icepeak.Server.WebsocketServer.MultiSubscription (handleClient) where

import Control.Concurrent (modifyMVar_, newEmptyMVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, takeMVar, tryTakeMVar, newMVar, readMVar, tryPutMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, finally, fromException, throwIO)
import Control.Monad (forever, void)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.UUID (UUID)
import System.Random (randomIO)

import Data.Bifunctor

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))

import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString as BS

import qualified Network.WebSockets as WS

import Icepeak.Server.Core (Core, coreClients, withCoreMetrics)
import qualified Icepeak.Server.Metrics as Metrics
import qualified Icepeak.Server.Subscription as Subscription

-- * Client handling

newUUID :: IO UUID
newUUID = randomIO

data ClientPayload
  = Subscribe   { subscribePaths :: [Text], token :: Text }
  | UnSubscribe { unsubscribePaths :: [Text] }

-- ** Parsing the Payload

data PayloadType = SubscribeType | UnsubscribeType

instance Aeson.FromJSON PayloadType where
  parseJSON (Aeson.String "subscribe") = pure SubscribeType
  parseJSON (Aeson.String "unsubscribe") = pure UnsubscribeType
  parseJSON _ = fail "Expecting 'type' to be either 'subscribe' or 'unsubscribe'"

data ClientPayloadMalformed
  = PayloadSizeOutOfBounds
  | UnexpectedBinaryPayload

  | JsonDecodeError Text
  | PayloadNotAnObject
  | InvalidType Text

  | SubscribePathsMissingOrMalformed Text
  | SubscribeTokenMissing Text
  | SubscribeTokenNotAString Text

  | UnsubscribePathsMissingOrMalformed Text

data BoundedBS
  = WithinBounds LBS.ByteString
  | OutOfBounds

checkBounds :: Int -> LBS.ByteString -> BoundedBS
checkBounds maxSize lazyBS = isOutOfBoundsAcc lazyBS 0
  where
    isOutOfBoundsAcc LBS.Empty _ = WithinBounds lazyBS
    isOutOfBoundsAcc (LBS.Chunk chunk rest) accSize =
      let accSize' = accSize + BS.length chunk in
        if accSize' > maxSize then OutOfBounds
        else isOutOfBoundsAcc rest accSize'

maxPayloadBytes :: Int
maxPayloadBytes = 1_000_000

clientPayloadFromDataMessage
  :: WS.DataMessage
  -> Either ClientPayloadMalformed ClientPayload
clientPayloadFromDataMessage (WS.Text encodedUtf8ByteString _)
  = case checkBounds maxPayloadBytes encodedUtf8ByteString of
      OutOfBounds -> Left PayloadSizeOutOfBounds
      (WithinBounds boundedBytesString) ->
        parseClientPayload boundedBytesString

clientPayloadFromDataMessage (WS.Binary _)
  = Left UnexpectedBinaryPayload

parseClientPayload
  :: LBS.ByteString
  -> Either ClientPayloadMalformed ClientPayload

parseClientPayload clientPayloadByteString = do
  let
    parseEither :: Aeson.Parser a -> Either String a
    parseEither parser = Aeson.parseEither (const parser) ()

    mapError = flip first

    mapErrorAeson (Aeson.Error str) f = Left $ f str
    mapErrorAeson (Aeson.Success val) _ = Right val

  clientPayloadAeson <- Aeson.eitherDecode @Value clientPayloadByteString
                        `mapError` (JsonDecodeError . Text.pack)
  case clientPayloadAeson of
    (Aeson.Object clientPayloadObject) -> do
      payloadType <- parseEither @PayloadType (clientPayloadObject .: "type")
                     `mapError` (InvalidType . Text.pack)
      case payloadType of
        SubscribeType -> do
          pathsParsed <- parseEither @[Text] (clientPayloadObject .: "paths")
                         `mapError` (SubscribePathsMissingOrMalformed . Text.pack)
          tokenValue <- parseEither @Value (clientPayloadObject .: "token")
                         `mapError` (SubscribeTokenMissing . Text.pack)
          tokenParsed <- Aeson.fromJSON @Text tokenValue
                         `mapErrorAeson` (SubscribeTokenNotAString . Text.pack)
          pure $ Subscribe pathsParsed tokenParsed

        UnsubscribeType -> do
          parsedPaths <- parseEither @[Text] (clientPayloadObject .: "paths")
                         `mapError` (UnsubscribePathsMissingOrMalformed . Text.pack)
          pure $ UnSubscribe parsedPaths
    _ -> Left PayloadNotAnObject



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

    onPayload (Right (Subscribe newPath pathToken)) = do
      isAuthorised <- authorisePathSubscription newPath pathToken

      case isAuthorised of
        False -> undefined
        True -> do
          pathValueMVar <- newEmptyMVar
          modifyMVar_ subscribedPathsMVar
            (pure . HashMap.insert newPath pathValueMVar)
          modifyMVar_ serverStateMVar
            (pure . Subscription.subscribe newPath uuid
              (\writeToSub newValue -> do
                  writeToSub pathValueMVar newValue
                  void $ tryPutMVar isDirtyMVar ()))

    onPayload (Right (UnSubscribe unsubPath)) = do
      modifyMVar_ serverStateMVar
        (pure . Subscription.unsubscribe unsubPath uuid)
      modifyMVar_ subscribedPathsMVar
        (pure . HashMap.delete unsubPath)

    onPayload (Left invalidPayload) = pure ()

    takeMVarNewValues = do
      takeMVar isDirtyMVar
      valueMVars <- HashMap.elems <$> readMVar subscribedPathsMVar
      catMaybes <$> mapM tryTakeMVar valueMVars

    manageConnection = withAsync
      (updateThread conn takeMVarNewValues)
      (const $ forever
        $ WS.receiveDataMessage conn
        >>= pure . clientPayloadFromDataMessage
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
