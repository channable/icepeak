{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Icepeak.Server.WebsocketServer.MultiSubscription (handleClient) where

import Control.Concurrent (modifyMVar_, newEmptyMVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, takeMVar, tryTakeMVar, newMVar, readMVar, tryPutMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, finally, fromException, throwIO)
import Control.Monad (forever, void, forM)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.UUID (UUID)
import System.Random (randomIO)

import Data.Bifunctor

import Data.Functor

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.:?))

import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString as BS

import qualified Network.WebSockets as WS

import qualified Icepeak.Server.Config as Config
import Icepeak.Server.Config (Config)

import qualified Icepeak.Server.Core as Core
import Icepeak.Server.Core (Core)

import qualified Icepeak.Server.Metrics as Metrics
import qualified Icepeak.Server.Subscription as Subscription
import Network.WebSockets (sendDataMessage)
import Icepeak.Server.Store (Path)
import Icepeak.Server.JwtAuth (extractClaim)
import qualified Icepeak.Server.AccessControl as Access
import qualified Data.Time.Clock.POSIX as Clock
import qualified Data.Text.Encoding as Text

-- * Client handling

newUUID :: IO UUID
newUUID = randomIO

data ClientPayload
  = Subscribe   { subscribePaths :: [Text], token :: Maybe Text }
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
  | UnexpectedType Text

  | SubscribePathsMissingOrMalformed Text
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

fromDataMessage
  :: WS.DataMessage
  -> Either ClientPayloadMalformed ClientPayload
fromDataMessage (WS.Text encodedUtf8ByteString _)
  = case checkBounds maxPayloadBytes encodedUtf8ByteString of
      OutOfBounds -> Left PayloadSizeOutOfBounds
      (WithinBounds boundedBytesString) ->
        parseClientPayload boundedBytesString
fromDataMessage (WS.Binary _)
  = Left UnexpectedBinaryPayload
  

parseClientPayload
  :: LBS.ByteString
  -> Either ClientPayloadMalformed ClientPayload

parseClientPayload clientPayloadByteString = do
  let
    parseEither :: Aeson.Parser a -> Either String a
    parseEither parser = Aeson.parseEither (const parser) ()

    mapError = flip first
    
  clientPayloadAeson <- Aeson.eitherDecode @Value clientPayloadByteString
                        `mapError` (JsonDecodeError . Text.pack)
  case clientPayloadAeson of
    (Aeson.Object clientPayloadObject) -> do
      payloadType <- parseEither @PayloadType (clientPayloadObject .: "type")
                     `mapError` (UnexpectedType . Text.pack)
      case payloadType of
        SubscribeType -> do

          pathsParsed <- parseEither @[Text] (clientPayloadObject .: "paths")
                         `mapError` (SubscribePathsMissingOrMalformed . Text.pack)
          mbToken <- parseEither @(Maybe Text) (clientPayloadObject .:? "token")
                         `mapError` (SubscribeTokenNotAString . Text.pack)
          pure $ Subscribe pathsParsed mbToken

        UnsubscribeType -> do
          parsedPaths <- parseEither @[Text] (clientPayloadObject .: "paths")
                         `mapError` (UnsubscribePathsMissingOrMalformed . Text.pack)
          pure $ UnSubscribe parsedPaths
          
    _ -> Left PayloadNotAnObject


-- ** Sending Response Payloads

-- sendDataMessage  :: Connection -> DataMessage   -> IO () -- probably this one

data SubscribeResponse
  = SubscribeResponseSuccess
  | SubscribeResponseFailure

data UnsubscribeResponse
  = UnubscribeResponseSuccess
  | UnubscribeResponseFailure

data Client = Client
  { clientConn :: WS.Connection
  , clientUuid :: UUID
  , clientCore :: Core
  , clientIsDirty :: MVar ()
  , clientSubscriptions :: MVar (HashMap Path (MVar Value))
  }

onPayload
  :: Client
  -> Either ClientPayloadMalformed ClientPayload
  -> IO ()
onPayload client (Right (Subscribe paths mbToken)) = do
  let
    core = clientCore client
    subscriptions = clientSubscriptions client
    conn = clientConn client
    isDirty = clientIsDirty client
    uuid = clientUuid client

    coreClients = Core.coreClients core
           
    coreConfig = Core.coreConfig core
    jwtEnabled = Config.configEnableJwtAuth coreConfig
    jwtSecret = Config.configJwtSecret coreConfig
    
    segmentedPaths = Text.splitOn "/" <$> paths :: [Path]

    doSubscribe :: IO ()
    doSubscribe = do
      
      pathsWithCurrentValue <- forM segmentedPaths $
        \path -> do
          valueAtPath <- Core.getCurrentValue core path
          pure (Text.intercalate "/" path, valueAtPath)
      
      WS.sendTextData conn
        $ Aeson.encode
        $ Aeson.object
        [ "type"  .= ("subscription" :: Text)
        , "paths" .=
          (pathsWithCurrentValue <&>
            \(path, value) ->
              Aeson.object [ "path" .= path, "value" .= value ])
        ]

      for_ segmentedPaths $ \newPath -> do
        pathValueMVar <- newEmptyMVar
        
        modifyMVar_ subscriptions
          (pure . HashMap.insert newPath pathValueMVar)
          
        modifyMVar_ coreClients
          (pure . Subscription.subscribe newPath uuid
            (\writeToSub newValue -> do
                writeToSub pathValueMVar newValue
                void $ tryPutMVar isDirty ()))
  
  case (jwtEnabled, jwtSecret, mbToken) of
    (True, Just secret, Nothing) -> _
    (True, Just secret, Just tokenBS) -> do
      now <- Clock.getPOSIXTime
      case extractClaim now secret (Text.encodeUtf8 tokenBS) of
        Left tokenError -> _
        Right authenticatedIcePeakClaim -> do
          let pathIsAuth = segmentedPaths <&>
                (\path -> Access.isAuthorizedByClaim
                              authenticatedIcePeakClaim
                              path Access.ModeRead)
              allAuth = and pathIsAuth
          if allAuth then doSubscribe
            else _
            
    _ -> doSubscribe

onPayload client (Right (UnSubscribe paths)) = do
  pure ()  

    --   modifyMVar_ serverStateMVar
    --     (pure . Subscription.unsubscribe unsubPath uuid)
    --   modifyMVar_ subscribedPathsMVar
    --     (pure . HashMap.delete unsubPath)

onPayload client (Left malformedPayload) = do
  pure ()


onMessage :: Client -> IO ()
onMessage client = do
  dataMessage <- WS.receiveDataMessage (clientConn client)
  onPayload client (fromDataMessage dataMessage)

onConnect :: Client -> IO ()
onConnect client =
  Core.withCoreMetrics (clientCore client) Metrics.incrementSubscribers

onDisconnect :: Client -> IO ()
onDisconnect client = do
  let
    core = clientCore client
    subscriptions = clientSubscriptions client
    uuid = clientUuid client
    
  paths <-  HashMap.keys <$> takeMVar subscriptions
  for_ paths
    (\path -> modifyMVar_ (Core.coreClients core)
      (pure . Subscription.unsubscribe path uuid))
    
  Core.withCoreMetrics core Metrics.decrementSubscribers

handleClient :: WS.Connection -> Core -> IO ()
handleClient conn core = do
  uuid          <- newUUID
  isDirty       <- newMVar ()
  subscriptions <- newMVar (HashMap.empty :: HashMap [Text] (MVar Value))

  let
    client = Client
      { clientConn = conn
      , clientUuid = uuid
      , clientCore = core
      , clientIsDirty = isDirty
      , clientSubscriptions = subscriptions
      }
    
    manageConnection = withAsync
      (updateThread client)
      (const $ forever $ do
          dataMessage <- WS.receiveDataMessage conn
          onPayload client (fromDataMessage dataMessage))
 
    -- Simply ignore connection errors, otherwise, Warp handles the exception
    -- and sends a 500 response in the middle of a WebSocket connection, and
    -- that violates the WebSocket protocol.
    -- Note that subscribers are still properly removed by the finally below.
    handleConnectionError :: WS.ConnectionException -> IO ()
    handleConnectionError _ = pure ()
  -- Put the client in the subscription tree and keep the connection open.
  -- Remove it when the connection is closed.
  finally (onConnect client >> manageConnection) (onDisconnect client)
    `catch` handleConnectionError


takeMVarUpdatedValues :: Client -> IO [Value]
takeMVarUpdatedValues client = do
  let
    isDirty = clientIsDirty client
    subscriptions = clientSubscriptions client
  takeMVar isDirty
  valueMVars <- HashMap.elems <$> readMVar subscriptions
  catMaybes <$> mapM tryTakeMVar valueMVars

-- This function handles sending the updates to subscribers.
updateThread :: Client -> IO ()
updateThread client =
  let
    conn = clientConn client
    
    send :: Value -> IO ()
    send value = WS.sendTextData conn (Aeson.encode value) `catch` sendFailed

    sendFailed :: SomeException -> IO ()
    sendFailed exc
      -- Rethrow async exceptions, they are meant for inter-thread communication
      -- (e.g. ThreadKilled) and we don't expect them at this point.
      | Just asyncExc <- fromException exc = throwIO (asyncExc :: SomeAsyncException)
      -- We want to catch all other errors in order to prevent them from
      -- bubbling up and disrupting the broadcasts to other clients.
      | otherwise = pure ()
      
  in forever $ do
    value <- takeMVarUpdatedValues client
    mapM send value
