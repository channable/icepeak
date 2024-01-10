{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Icepeak.Server.MultiSubscriptionSpec (spec) where

import Test.Hspec
import Test.Hspec.Expectations.Json
import Data.Aeson ((.=))
import Data.Word (Word16)
import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified System.Directory as Directory
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets as WS

import qualified Icepeak.Server.Server as Icepeak
import qualified Icepeak.Server.Logger as Icepeak
import qualified Icepeak.Server.Config as Icepeak
import qualified Icepeak.Server.WebsocketServer as Icepeak
import qualified Icepeak.Server.Core as Icepeak
import qualified Icepeak.Server.Store as Icepeak
import qualified Icepeak.Server.HttpServer as IcepeakHttp
import qualified Icepeak.Server.WebsocketServer.Payload as Icepeak


icepeakPort :: Int
icepeakPort = 9898

data Icepeak = Icepeak
  { icepeakCore :: Icepeak.Core
  , icepeakShutdown :: IO ()
  }

defaultConfig
  :: Icepeak.Config
defaultConfig
  = Icepeak.Config
  { Icepeak.configDataFile = Nothing
  , Icepeak.configPort = icepeakPort
  , Icepeak.configEnableJwtAuth = False
  , Icepeak.configJwtSecret = Nothing
  , Icepeak.configMetricsEndpoint = Nothing
  , Icepeak.configQueueCapacity = 256
  , Icepeak.configSyncIntervalMicroSeconds = Nothing
  , Icepeak.configEnableJournaling = False
  , Icepeak.configDisableSentryLogging = True
  , Icepeak.configSentryDSN = Nothing
  , Icepeak.configStorageBackend = Icepeak.File
  , Icepeak.configSyncLogging = False
  , Icepeak.configWebSocketPingInterval = 1
  , Icepeak.configWebSocketPongTimeout = 1
  , Icepeak.configInitialSubscriptionTimeoutMicroSeconds = 100_000
  }

withIcepeak :: IO Icepeak
withIcepeak = do
  let
    storageFile = "/tmp/icepeak.json"
    config = defaultConfig { Icepeak.configDataFile = Just storageFile }
  writeFile storageFile "{}"
  logger <- Icepeak.newLogger config
  core <- Icepeak.newCore config logger Nothing >>=
    (\mbCore -> case mbCore of
        Left err -> do expectationFailure ("Failed to create Core: " <> err)
                       undefined
        Right core -> pure core)

  let wsServer = Icepeak.acceptConnection core
  application <- IcepeakHttp.new core

  -- Thread that accepts websocket connections
  webserverThread <- Async.async $ Icepeak.runServer logger wsServer application icepeakPort

  -- Thread that processes the Icepeak.Command queue
  commandLoopThread <- Async.async $ Icepeak.runCommandLoop core

  -- Thread that posts updates to websocket clients
  webSocketThread <- Async.async $ Icepeak.processUpdates core

  pure $ Icepeak
    { icepeakCore = core
    , icepeakShutdown = do
        mapM_ Async.cancel [webserverThread, commandLoopThread, webSocketThread ]
        Directory.removeFile storageFile
    }

createDataSet :: Icepeak -> IO ()
createDataSet icepeak = do
  mapM_ (makeModification (icepeakCore icepeak))
    [ Icepeak.Put ["A", "A"] "A"
    , Icepeak.Put ["A", "B"] "B"
    , Icepeak.Put ["B"] "B"
    ]

makeModification :: Icepeak.Core -> Icepeak.Modification -> IO ()
makeModification core modification = do
  waitMVar <- MVar.newEmptyMVar
  Icepeak.enqueueCommand (Icepeak.Modify modification (Just waitMVar)) core
  MVar.takeMVar waitMVar

data CloseExpectation
  = Unexpected String
  | CloseCode Word16
  deriving (Show, Eq)

openReusableIcepeakConn :: (WS.Connection -> IO a) -> IO a
openReusableIcepeakConn = WS.runClient "localhost" icepeakPort "/?method=reusable"

sendJson :: WS.Connection -> Aeson.Value -> IO ()
sendJson conn value = WS.sendDataMessage conn (WS.Text (Aeson.encode value) Nothing)

expectDataMessage :: WS.Connection -> IO WS.DataMessage
expectDataMessage conn = do
  Right msg <- Async.race
    (do Concurrent.threadDelay 100_000 -- 0.1 s
        expectationFailure "Client expected a message, but did not receive anything for the duration of the 0.1s timeout")
    (WS.receiveDataMessage conn)
  pure msg

expectNoMessage :: WS.Connection -> IO ()
expectNoMessage conn = do
  Left () <- Async.race
    (Concurrent.threadDelay 10_000) -- 0.01s
    (WS.receiveDataMessage conn >>=
      (\p -> expectationFailure
        $ "Client received an unexpected payload: " <> show p))
  pure ()

withResponseJson :: WS.Connection -> (Aeson.Value -> Expectation) -> Expectation
withResponseJson conn jsonCheck = do
  (WS.Text payload _) <- expectDataMessage conn
  (Just json) <- pure $ Aeson.decode payload
  jsonCheck json

invalidPayloadsSpec :: SpecWith a
invalidPayloadsSpec = describe "Payload Parse" $ do
  context "when client sends invalid payload " $ do
    let openThenSend dataMessage = openReusableIcepeakConn $ \conn ->
          do WS.sendDataMessage conn dataMessage
             Exception.catch
               (do unexpectedDataMessage <- WS.receiveDataMessage conn
                   pure (Unexpected $ "Received data message when socket close was expected: " <> show unexpectedDataMessage))

               (\case (WS.CloseRequest code _) -> pure $ CloseCode code
                      otherException -> pure $ Unexpected ("Unexpected exception: " <> show otherException))

    it "should close and know when provided with non-json input" $ const $ do
      notJSON <- openThenSend (WS.Text "Hello" Nothing)
      notJSON `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeJsonDecodeError)

    it "should close and know when provided with binary payload" $ const $ do
      binaryMessage <- openThenSend (WS.Binary "Hello")
      binaryMessage `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeBinaryPayload)

    it "should close and know when provided with something other than object" $ const $ do
      notAnObject <- openThenSend (WS.Text (Aeson.encode $ Aeson.Number 3) Nothing)
      notAnObject `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypePayloadNotObject)

    it "should close and know when provided with unexpected payload type" $ const $ do
      unexpectedType <- openThenSend (WS.Text (Aeson.encode $ Aeson.object [ "type" .= ("subskribe" :: String) ]) Nothing)
      unexpectedType `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeMissingOrUnexpectedType)

      typeMissing <- openThenSend (WS.Text (Aeson.encode $ Aeson.object [ ]) Nothing)
      typeMissing `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeMissingOrUnexpectedType)

    it "should close and know when provided with a payload size that is out of bounds" $ const $ do
      outOfBounds <- openThenSend (WS.Text (BS.Lazy.replicate (fromInteger (toInteger $ Icepeak.maxPayloadBytes + 1)) 0) Nothing)
      outOfBounds `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeSizeOutOfBounds)

      insideOfBounds <- openThenSend (WS.Text (BS.Lazy.replicate (fromInteger (toInteger Icepeak.maxPayloadBytes)) 0) Nothing)
      insideOfBounds `shouldNotBe` CloseCode (Icepeak.closeCode Icepeak.TypeSizeOutOfBounds)

singleConnectionCommunicationSpec :: SpecWith Icepeak
singleConnectionCommunicationSpec = aroundAllWith
  (\specUsingArgs icepeak -> openReusableIcepeakConn (curry specUsingArgs icepeak))
  $ describe "Communication Over Single Connection" $ do
  successfulSubscribe
  successfulReceiveUpdates
  successfulUnsubscribe
  successfulUnsubscribeNoUpdates

successfulSubscribe :: SpecWith (Icepeak, WS.Connection)
successfulSubscribe = context "when client subscribes" $ do
  it "should succesfully send subscribe and receive values at paths" $
    \(_, clientConn) -> do
    sendJson clientConn $ Aeson.object
      [ "type" .= ("subscribe" :: Text)
      , "paths" .= ([ "A/B", "A/A" ] :: [Text])
      ]
    withResponseJson clientConn
      (\responseJson -> do
          responseJson `shouldMatchJson` Aeson.object
            [ "type" .= ("subscribe" :: Text)
            , "code" .= (200 :: Int)
            , "paths" .=
              [ Aeson.object
                [ "path"  .= ("A/B" :: Text)
                , "value" .= ("B":: Text)
                ]
              , Aeson.object
                [ "path"  .= ("A/A" :: Text)
                , "value" .= ("A":: Text)
                ]
              ]])

  it "should subscribe to non-existent path and get null" $
    \(_, clientConn) -> do
    sendJson clientConn $ Aeson.object
      [ "type" .= ("subscribe" :: Text)
      , "paths" .= ([ "NULL" ] :: [Text]) ]
    withResponseJson clientConn
      (\responseJson -> do
          responseJson `shouldMatchJson` Aeson.object
            [ "type" .= ("subscribe" :: Text)
            , "code" .= (200 :: Int)
            , "paths" .=
              [ Aeson.object
                [ "path"  .= ("NULL" :: Text)
                , "value" .= Aeson.Null
                ]
              ]])

successfulReceiveUpdates :: SpecWith (Icepeak, WS.Connection)
successfulReceiveUpdates = context "when values are updated" $ do
  it "should send client upated value at subscribed path" $
    \(icepeak, clientConn) -> do
      makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "C")
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("update" :: Text)
              , "value" .= ("C" :: Text)
              ])

  it "should send client nulled sub-paths when path sub-paths are overriden with value" $
    \(icepeak, clientConn) -> do
      makeModification (icepeakCore icepeak) (Icepeak.Put ["A"] "C")
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("update" :: Text)
              , "value" .= Aeson.Null
              ])
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("update" :: Text)
              , "value" .= Aeson.Null
              ])

  it "should send client update on previosly overriden path" $
    \(icepeak, clientConn) -> do
      makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "C")
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("update" :: Text)
              , "value" .= ("C" :: Text)
              , "path" .= ("A/A" :: Text)
              ])

  it "should send client null update on deleted path" $
    \(icepeak, clientConn) -> do
      makeModification (icepeakCore icepeak) (Icepeak.Delete ["A", "A"])
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("update" :: Text)
              , "value" .= Aeson.Null
              , "path" .= ("A/A" :: Text)
              ])

  it "should send client update on previosly deleted path" $
    \(icepeak, clientConn) -> do
      makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "D")
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("update" :: Text)
              , "value" .= ("D" :: Text)
              , "path" .= ("A/A" :: Text)
              ])

successfulUnsubscribe :: SpecWith (Icepeak, WS.Connection)
successfulUnsubscribe = context "when client unsubscribes" $ do
  it "should unsubscribe from multiple existing paths" $
    \(_icepeak, clientConn) -> do
      sendJson clientConn $ Aeson.object
        [ "type" .= ("unsubscribe" :: Text)
        , "paths" .= ([ "A/B", "A/A" ] :: [Text]) ]
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("unsubscribe" :: Text)
              , "code" .= (200 :: Int)
              , "paths" .= ([ "A/B", "A/A" ] :: [Text])
              ])

  it "should get no paths from non-subscribed paths" $
    \(_icepeak, clientConn) -> do
        sendJson clientConn $ Aeson.object
          [ "type" .= ("unsubscribe" :: Text)
          , "paths" .= ([ "C/D", "E" ] :: [Text]) ]

        withResponseJson clientConn
          (\responseJson -> do
              responseJson `shouldMatchJson` Aeson.object
                [ "type" .= ("unsubscribe" :: Text)
                , "code" .= (200 :: Int)
                , "paths" .= ([ ] :: [Text])
                ])

successfulUnsubscribeNoUpdates :: SpecWith (Icepeak, WS.Connection)
successfulUnsubscribeNoUpdates = context "when client unsubscribes" $ do
  it "should no longer receive updates for unsusbscribed paths" $
    \(icepeak, clientConn) -> do
      makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "B"] "C")
      expectNoMessage clientConn >>= shouldBe ()

      makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "C")
      expectNoMessage clientConn >>= shouldBe ()

  it "should be able to resubscribe after unsubscribing" $
    \(icepeak, clientConn) -> do
      sendJson clientConn $ Aeson.object
        [ "type" .= ("subscribe" :: Text)
        , "paths" .= ([ "A/B" ] :: [Text]) ]
      withResponseJson clientConn
         (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("subscribe" :: Text)
              , "code" .= (200 :: Int)
              , "paths" .= [ Aeson.object [ "path" .= ("A/B" :: Text), "value" .= ("C" :: Text)] ]
              ])
      makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "B"] "D")
      withResponseJson clientConn
        (\responseJson -> do
            responseJson `shouldMatchJson` Aeson.object
              [ "type" .= ("update" :: Text)
              , "value" .= ("D" :: Text)
              , "path" .= ("A/B" :: Text)
              ])
      expectNoMessage clientConn >>= shouldBe ()

spec :: Spec
spec =
  aroundAll
  (\testSpec -> do
      icepeak <- withIcepeak
      createDataSet icepeak
      testSpec icepeak
      icepeakShutdown icepeak)
  $ do describe "MultiSubscription Connection Protocol" $ do
         invalidPayloadsSpec
         singleConnectionCommunicationSpec
       describe "MultiSubscription Subscription Timeout Deadline Authorisation Mechanism" $ do
         deadlineTimeoutSpec

deadlineTimeoutSpec :: SpecWith Icepeak
deadlineTimeoutSpec = do
  it "should cause the server to close the connection if send subscribe late"
    (\icepeak -> openReusableIcepeakConn
      (\conn -> do
        let timeoutDeadline = Icepeak.configInitialSubscriptionTimeoutMicroSeconds
              $ Icepeak.coreConfig
              $ icepeakCore icepeak
        Concurrent.threadDelay $ timeoutDeadline + 50_000
        sendJson conn $ Aeson.object
          [ "type" .= ("subscribe" :: Text)
          , "paths" .= ([ "A/B", "A/A" ] :: [Text]) ]
        WS.receive conn `shouldThrow` connectionClosed
     ))
  it "should cause the server to close the connection if other request on time but still not subscribed"
    (\icepeak -> openReusableIcepeakConn
      (\conn -> do
        let timeoutDeadline = Icepeak.configInitialSubscriptionTimeoutMicroSeconds
              $ Icepeak.coreConfig
              $ icepeakCore icepeak
            dummyMsg = Aeson.object
              [ "type" .= ("unsubscribe" :: Text)
              , "paths" .= ([ "A/B", "A/A" ] :: [Text]) ]

        Concurrent.threadDelay $ timeoutDeadline - 50_000

        sendJson conn dummyMsg
        _ <- expectDataMessage conn
        sendJson conn dummyMsg
        _ <- expectDataMessage conn

        Concurrent.threadDelay 100_000

        sendJson conn (Aeson.object
          [ "type" .= ("subscribe" :: Text)
          , "paths" .= ([ "A/B", "A/A" ] :: [Text]) ])
        WS.receive conn `shouldThrow` connectionClosed
     ))

connectionClosed :: Selector WS.ConnectionException
connectionClosed WS.ConnectionClosed = True
connectionClosed _ = False
