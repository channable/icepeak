{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Icepeak.Server.MultiSubscriptionSpec (spec) where

import qualified Icepeak.Server.Server as Icepeak
import qualified Icepeak.Server.Logger as Icepeak
import qualified Icepeak.Server.Config as Icepeak
import qualified Icepeak.Server.WebsocketServer as Icepeak
import qualified Icepeak.Server.Core as Icepeak
import qualified Icepeak.Server.Store as Icepeak
import qualified Icepeak.Server.HttpServer as IcepeakHttp
import qualified Icepeak.Server.WebsocketServer.Payload as Icepeak

import Test.Hspec
import Test.Hspec.Expectations.Json

import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets as WS

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

import qualified Control.Concurrent as MVar
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent as Concurrent

import qualified Control.Exception as Exception

import Data.Word (Word16)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BS.Lazy


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
  }

withIcepeak :: IO Icepeak
withIcepeak = do
  let config = defaultConfig
  logger <- Icepeak.newLogger config
  (Right core) <- Icepeak.newCore config logger Nothing
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
    , icepeakShutdown = mapM_ Async.cancel [webserverThread, commandLoopThread, webSocketThread ]
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
invalidPayloadsSpec = describe "Opening and sending invalid payloads" $ do
  it "should close connection upon invalid payload" $ const $ do
    let openThenSend dataMessage = openReusableIcepeakConn $ \conn ->
          do WS.sendDataMessage conn dataMessage
             Exception.catch
               (do unexpectedDataMessage <- WS.receiveDataMessage conn
                   pure (Unexpected $ "Received data message when socket close was expected: " <> show unexpectedDataMessage))
               
               (\case (WS.CloseRequest code _) -> pure $ CloseCode code
                      otherException -> pure $ Unexpected ("Unexpected exception: " <> show otherException))

    notJSON <- openThenSend (WS.Text "Hello" Nothing)
    notJSON `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeJsonDecodeError)

    binaryMessage <- openThenSend (WS.Binary "Hello")
    binaryMessage `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeBinaryPayload)

    notAnObject <- openThenSend (WS.Text (Aeson.encode $ Aeson.Number 3) Nothing)
    notAnObject `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypePayloadNotObject)

    unexpectedType <- openThenSend (WS.Text (Aeson.encode $ Aeson.object [ "type" .= ("subskribe" :: String) ]) Nothing)
    unexpectedType `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeMissingOrUnexpectedType)

    noType <- openThenSend (WS.Text (Aeson.encode $ Aeson.object [ ]) Nothing)
    noType `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeMissingOrUnexpectedType)

    outOfBounds <- openThenSend (WS.Text (BS.Lazy.replicate (fromInteger (toInteger $ Icepeak.maxPayloadBytes + 1)) 0) Nothing)
    outOfBounds `shouldBe` CloseCode (Icepeak.closeCode Icepeak.TypeSizeOutOfBounds)

    insideOfBounds <- openThenSend (WS.Text (BS.Lazy.replicate (fromInteger (toInteger Icepeak.maxPayloadBytes)) 0) Nothing)
    insideOfBounds `shouldNotBe` CloseCode (Icepeak.closeCode Icepeak.TypeSizeOutOfBounds)

singleConnectionCommunicationSpec :: SpecWith Icepeak
singleConnectionCommunicationSpec = aroundAllWith
  (\specUsingArgs icepeak -> openReusableIcepeakConn (curry specUsingArgs icepeak))
  $ describe "Communication over a single connection" $ do
  succesfulSubscribe
  succesfulReceiveUpdates
  succesfulUnsubscribe
  succesfulUnsubscribeNoUpdates
  
succesfulSubscribe :: SpecWith (Icepeak, WS.Connection)
succesfulSubscribe = it "should subscribe and receive success response with values"
  $ \(_, clientConn) -> do
  
  sendJson clientConn $ Aeson.object
    [ "type" .= ("subscribe" :: Text)
    , "paths" .= ([ "A/B", "A/A" ] :: [Text]) ]
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

succesfulReceiveUpdates :: SpecWith (Icepeak, WS.Connection)
succesfulReceiveUpdates = it "should receive updates" $
  \(icepeak, clientConn) -> do
    makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "C")
    withResponseJson clientConn
      (\responseJson -> do
          responseJson `shouldMatchJson` Aeson.object
            [ "type" .= ("update" :: Text)
            , "value" .= ("C" :: Text)
            ])

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

    makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "C")
    withResponseJson clientConn
      (\responseJson -> do
          responseJson `shouldMatchJson` Aeson.object
            [ "type" .= ("update" :: Text)
            , "value" .= ("C" :: Text)
            , "path" .= ("A/A" :: Text)
            ])

    makeModification (icepeakCore icepeak) (Icepeak.Delete ["A", "A"])
    withResponseJson clientConn
      (\responseJson -> do
          responseJson `shouldMatchJson` Aeson.object
            [ "type" .= ("update" :: Text)
            , "value" .= Aeson.Null
            , "path" .= ("A/A" :: Text)
            ])

    makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "D")
    withResponseJson clientConn
      (\responseJson -> do
          responseJson `shouldMatchJson` Aeson.object
            [ "type" .= ("update" :: Text)
            , "value" .= ("D" :: Text)
            , "path" .= ("A/A" :: Text)
            ])

succesfulUnsubscribe :: SpecWith (Icepeak, WS.Connection)
succesfulUnsubscribe = it "should unsubscribe and receive success response" $
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

    sendJson clientConn $ Aeson.object
      [ "type" .= ("unsubscribe" :: Text)
      , "paths" .= ([ "NULL/NULL", "NULL/NULL" ] :: [Text]) ]
    withResponseJson clientConn
      (\responseJson -> do
          responseJson `shouldMatchJson` Aeson.object
            [ "type" .= ("unsubscribe" :: Text)
            , "code" .= (200 :: Int)
            , "paths" .= ([ "NULL/NULL", "NULL/NULL" ] :: [Text])
            ])

succesfulUnsubscribeNoUpdates :: SpecWith (Icepeak, WS.Connection)
succesfulUnsubscribeNoUpdates = it "should no longer receive updates for unsusbscribed paths" $
  \(icepeak, clientConn) -> do
    makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "B"] "C")
    expectNoMessage clientConn >>= shouldBe ()

    makeModification (icepeakCore icepeak) (Icepeak.Put ["A", "A"] "C")
    expectNoMessage clientConn >>= shouldBe ()

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

    expectNoMessage clientConn >>= shouldBe ()

    
            
spec :: Spec
spec =
  aroundAll
  (\testSpec -> do
      icepeak <- withIcepeak
      createDataSet icepeak
      testSpec icepeak
      icepeakShutdown icepeak)
  $ describe "MultiSubscription connection protocol"
  $ do invalidPayloadsSpec
       singleConnectionCommunicationSpec
