{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Icepeak.Server.WebsocketServer.MultiSubscription (handleClient) where


import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar  as MVar

import qualified Control.Exception as Exception
import qualified Control.Monad     as Monad

import Data.Functor  ((<&>))
import qualified Data.Maybe    as Maybe

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Text (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson

import Data.UUID (UUID)
import qualified Data.Time.Clock.POSIX as Clock
import qualified System.Random         as Random

import qualified Web.JWT               as JWT
import qualified Network.WebSockets    as WS


import Icepeak.Server.WebsocketServer.Payload
import Icepeak.Server.Core (Core)
import Icepeak.Server.Store (Path)
import Icepeak.Server.Config (Config)
import qualified Icepeak.Server.Config                  as Config
import qualified Icepeak.Server.Core                    as Core
import qualified Icepeak.Server.Metrics                 as Metrics
import qualified Icepeak.Server.Subscription            as Subscription
import qualified Icepeak.Server.JwtAuth                 as JwtAuth
import qualified Icepeak.Server.AccessControl           as Access

-- * Client handling

newUUID :: IO UUID
newUUID = Random.randomIO

-- ** Sending Response Payloads

data Client = Client
  { clientConn :: WS.Connection
  , clientUuid :: UUID
  , clientCore :: Core
  , clientIsDirty :: MVar ()
  , clientSubscriptions :: MVar (HashMap Path (MVar Value))
  }


doSubscribe :: Client -> [Path] -> IO ()
doSubscribe client paths = do
  let
    core = clientCore client
    conn = clientConn client
    
    uuid          = clientUuid client
    isDirty       = clientIsDirty client
    subscriptions = clientSubscriptions client

    coreClients = Core.coreClients core

  pathsWithCurrentValue <- Monad.forM paths $
    \path -> do
      valueAtPath <- Core.getCurrentValue core path
      pure (Text.intercalate "/" path, valueAtPath)

  WS.sendTextData conn
    $ Aeson.encode
    $ ResponseSubscribeSuccess
    { subscribeSuccessPathsValues = pathsWithCurrentValue }
    
  -- WARNING: Race condition, potentially miss an update when program is at this point 

  Monad.forM_ paths $ \newPath -> do
    pathValueMVar <- MVar.newEmptyMVar

    MVar.modifyMVar_ subscriptions
      (pure . HashMap.insert newPath pathValueMVar)

    MVar.modifyMVar_ coreClients
      (pure . Subscription.subscribe newPath uuid
        (\writeToSub newValue -> do
            writeToSub pathValueMVar newValue
            Monad.void $ MVar.tryPutMVar isDirty ()))


onPayloadSubscribeWithAuth
  :: Client
  -> JWT.VerifySigner
  -> RequestSubscribe
  -> IO ()
onPayloadSubscribeWithAuth client _ (RequestSubscribe paths Nothing) = do
  let conn = clientConn client
  WS.sendTextData conn -- 401  | No authorization token provided
    $ Aeson.encode
    $ ResponseSubscribeFailure
    { subscribeFailureStatusCode = 401
    , subscribeFailureMessage = "No authorisation token provided"
    , subscribeFailureExtraData = Nothing
    , subscribeFailurePaths = Just paths }
    
onPayloadSubscribeWithAuth client secret (RequestSubscribe paths (Just tokenBS)) = do
  let conn = clientConn client
      segmentedPaths = Text.splitOn "/" <$> paths :: [Path]
  now <- Clock.getPOSIXTime
  case JwtAuth.extractClaim now secret (Text.encodeUtf8 tokenBS) of
    Left tokenError -> do -- 403  | Authorization token was rejected / malformed |
      WS.sendTextData conn
        $ Aeson.encode
        $ ResponseSubscribeFailure
        { subscribeFailureStatusCode = 403
        , subscribeFailureMessage = "Error while extracting claim from JWT: " <> Text.pack (show tokenError)
        , subscribeFailureExtraData = Nothing
        , subscribeFailurePaths = Just paths }

    Right authenticatedIcePeakClaim -> do
      let pathsIsAuth = segmentedPaths <&>
            (\path -> ( path
                      , Access.isAuthorizedByClaim authenticatedIcePeakClaim path Access.ModeRead
                      ))
          allAuth = and $ snd <$> pathsIsAuth
      if allAuth
        then doSubscribe client segmentedPaths
        else WS.sendTextData conn -- 403  | Authorization token was rejected / malformed |
             $ Aeson.encode
             $ ResponseSubscribeFailure
             { subscribeFailureStatusCode = 403
             , subscribeFailureMessage = "Some paths are not authorised by the provided JWT claim"
             , subscribeFailureExtraData = Just
               $ Aeson.object [ "unauthorisedPaths" .= (fst <$> filter (not . snd) pathsIsAuth) ]
             , subscribeFailurePaths = Just paths }

onPayloadSubscribeNoAuth
  :: Client
  -> RequestSubscribe
  -> IO ()
onPayloadSubscribeNoAuth client (RequestSubscribe paths _) = do
  let segmentedPaths = Text.splitOn "/" <$> paths :: [Path]
  doSubscribe client segmentedPaths

onPayloadUnsubscribe
  :: Client
  -> RequestUnsubscribe
  -> IO ()
onPayloadUnsubscribe client (RequestUnsubscribe paths) = do
  let
    conn = clientConn client
    core = clientCore client

    segmentedPaths = Text.splitOn "/" <$> paths :: [Path]

    uuid          = clientUuid client
    subscriptions = clientSubscriptions client

    coreClients = Core.coreClients core

  Monad.forM_ segmentedPaths $ \path -> do
    MVar.modifyMVar_ coreClients
      (pure . Subscription.unsubscribe path uuid)

    MVar.modifyMVar_ subscriptions
      (pure . HashMap.delete path)

  WS.sendTextData conn
    $ Aeson.encode
    $ ResponseUnsubscribeSuccess { unsubscribeSuccessPaths = paths }

  
onPayloadMalformed
  :: Client
  -> RequestMalformedError
  -> IO ()
onPayloadMalformed client requestMalformedError = do
  let
    conn = clientConn client

    closeConnection :: CloseType -> IO ()
    closeConnection closeType = do
      -- NOTE:
      -- This will issue a control message to the peer.
      -- The connection will stay alive, and we will be expecting
      -- the peer to eventually respond with a close control message
      -- of its own, which will cause receiveDataMessage to
      -- throw a CloseRequest exception.
      WS.sendCloseCode conn
        (closeCode closeType)
        (closeMessage closeType)

    respondMalformedSubscribe :: Text -> IO ()
    respondMalformedSubscribe extra = do
      WS.sendTextData conn
        $ Aeson.encode
        $ ResponseSubscribeFailure
        { subscribeFailureStatusCode = 400
        , subscribeFailureMessage = "Subscribe request payload is malformed"
        , subscribeFailureExtraData = Just $ Aeson.toJSON extra
        , subscribeFailurePaths = Nothing }

    respondMalformedUnsubscribe :: Text -> IO ()
    respondMalformedUnsubscribe extra = do
      WS.sendTextData conn
        $ Aeson.encode
        $ ResponseUnsubscribeFailure
        { unsubscribeFailureStatusCode = 400
        , unsubscribeFailureMessage = "Unsubscribe request payload is malformed"
        , unsubscribeFailurePaths = Nothing
        , unsubscribeFailureExtraData = Just $ Aeson.toJSON extra
        }

  case requestMalformedError of
    PayloadSizeOutOfBounds
      -> closeConnection TypeSizeOutOfBounds
    UnexpectedBinaryPayload
      -> closeConnection TypeBinaryPayload
    JsonDecodeError _decodeError
      -> closeConnection TypeJsonDecodeError
    PayloadNotAnObject
      -> closeConnection TypePayloadNotObject
    MissingOrUnexpectedType _unexpectedType
      -> closeConnection TypeMissingOrUnexpectedType

    SubscribePathsMissingOrMalformed pathsParseError
      -> respondMalformedSubscribe
         $ "Subscribe paths are missing or malformed: " <> pathsParseError
    SubscribeTokenNotAString tokenParseError
      -> respondMalformedSubscribe
         $ "Subscribe token is not a string: " <> tokenParseError
    UnsubscribePathsMissingOrMalformed pathsParseError
      -> respondMalformedUnsubscribe
         $ "Unsubscribe paths are missing or malformed: " <> pathsParseError

-- | Explicit enumeration of the procedures that
-- the server will perform given a request.
data PayloadAction
  = ActionSubscribeWithAuth JWT.VerifySigner RequestSubscribe
  | ActionSubscribeNoAuth RequestSubscribe
  | ActionUnsubscribe RequestUnsubscribe
  | ActionError RequestMalformedError

-- | Determine the server action based on the request and config.
-- `Core.Config` is needed to determine if auth is enabled, otherwise the
-- `PayloadAction` can be determined purely from the parsed `RequestPayload`.
determinePayloadAction
  :: Config -> RequestPayload -> PayloadAction
  
determinePayloadAction coreConfig (RequestPayloadSubscribe requestSubscribe) = do
  let
    jwtEnabled = Config.configEnableJwtAuth coreConfig
    jwtSecret = Config.configJwtSecret coreConfig
    
  case (jwtEnabled, jwtSecret) of
    (True, Just secret) -> ActionSubscribeWithAuth secret requestSubscribe
    (False, Just _ )    -> ActionSubscribeNoAuth requestSubscribe
    (True , Nothing)    -> ActionSubscribeNoAuth requestSubscribe
    (False, Nothing)    -> ActionSubscribeNoAuth requestSubscribe

determinePayloadAction _ (RequestPayloadUnsubscribe requestUnsubscribe)
  = ActionUnsubscribe requestUnsubscribe

determinePayloadAction _ (RequestPayloadMalformed malformedPayload)
  = ActionError malformedPayload

-- | Peform the payload action. We pass the `Client` argument, which
-- heavily implies impure things are to happen, namely:
--  - Mutating `Core` MVars
--  - Using `Conn`
--  - Mutating `Client` MVars
performPayloadAction
  :: Client -> PayloadAction -> IO ()
performPayloadAction client payloadAction =
  case payloadAction of
    ActionSubscribeWithAuth secret requestSubscribe
      -> onPayloadSubscribeWithAuth client secret requestSubscribe
    ActionSubscribeNoAuth requestSubscribe
      -> onPayloadSubscribeNoAuth client requestSubscribe
    ActionUnsubscribe requestUnsubscribe
      -> onPayloadUnsubscribe client requestUnsubscribe
    ActionError requestMalformed
      -> onPayloadMalformed client requestMalformed

onMessage :: Client -> IO ()
onMessage client = do
  let coreConfig = Core.coreConfig $ clientCore client
      conn = clientConn client
  dataMessage <- WS.receiveDataMessage conn
  performPayloadAction client
    $ determinePayloadAction coreConfig
    $ parseDataMessage dataMessage

onConnect :: Client -> IO ()
onConnect client =
  Core.withCoreMetrics (clientCore client) Metrics.incrementSubscribers

onDisconnect :: Client -> IO ()
onDisconnect client = do
  let
    core = clientCore client
    subscriptions = clientSubscriptions client
    uuid = clientUuid client

  paths <-  HashMap.keys <$> MVar.takeMVar subscriptions
  Monad.forM_ paths
    (\path -> MVar.modifyMVar_ (Core.coreClients core)
      (pure . Subscription.unsubscribe path uuid))

  Core.withCoreMetrics core Metrics.decrementSubscribers

handleClient :: WS.Connection -> Core -> IO ()
handleClient conn core = do
  uuid          <- newUUID
  isDirty       <- MVar.newMVar ()
  subscriptions <- MVar.newMVar (HashMap.empty :: HashMap [Text] (MVar Value))

  let
    client = Client
      { clientConn = conn
      , clientUuid = uuid
      , clientCore = core
      , clientIsDirty = isDirty
      , clientSubscriptions = subscriptions
      }

    manageConnection = Async.withAsync
      (updateThread client)
      (const $ Monad.forever $ onMessage client)

    -- Simply ignore connection errors, otherwise, Warp handles the exception
    -- and sends a 500 response in the middle of a WebSocket connection, and
    -- that violates the WebSocket protocol.
    -- Note that subscribers are still properly removed by the finally below.
    handleConnectionError :: WS.ConnectionException -> IO ()
    handleConnectionError _ = pure ()
  -- Put the client in the subscription tree and keep the connection open.
  -- Remove it when the connection is closed.
  Exception.finally (onConnect client >> manageConnection) (onDisconnect client)
    `Exception.catch` handleConnectionError


takeMVarUpdatedValues :: Client -> IO [(Text, Value)]
takeMVarUpdatedValues client = do
  let
    isDirty = clientIsDirty client
    subscriptions = clientSubscriptions client
  MVar.takeMVar isDirty
  valueMVars <- HashMap.toList <$> MVar.readMVar subscriptions
  Maybe.catMaybes <$> (Monad.forM valueMVars $
    \(path, valueMVar) ->
      fmap (\v -> (Text.intercalate "/" path, v))
      <$> MVar.tryTakeMVar valueMVar)


-- This function handles sending the updates to subscribers.
updateThread :: Client -> IO ()
updateThread client =
  let
    conn = clientConn client

    send :: Text -> Value -> IO ()
    send path value =
      Exception.catch 
      (WS.sendTextData conn $
        Aeson.encode $ Update path value)
      sendFailed

    sendFailed :: Exception.SomeException -> IO ()
    sendFailed exc
      -- Rethrow async exceptions, they are meant for inter-thread communication
      -- (e.g. ThreadKilled) and we don't expect them at this point.
      | Just asyncExc <- Exception.fromException exc = Exception.throwIO (asyncExc :: Exception.SomeAsyncException)
      -- We want to catch all other errors in order to prevent them from
      -- bubbling up and disrupting the broadcasts to other clients.
      | otherwise = pure ()

  in Monad.forever $ do
    value <- takeMVarUpdatedValues client
    mapM (uncurry send) value
