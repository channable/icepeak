{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}


module Icepeak.Server.WebsocketServer.Payload where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Bifunctor

import Data.Functor

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.:?))

import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString as BS
import qualified Network.WebSockets as WS

-- * Request

data RequestPayload
  = RequestPayloadSubscribe RequestSubscribe
  | RequestPayloadUnsubscribe RequestUnsubscribe
  | RequestPayloadMalformed RequestMalformedError

data RequestSubscribe = RequestSubscribe
  { requestSubscribePaths :: [Text]
  , requestSubscribeToken :: Maybe Text
  }

data RequestUnsubscribe = RequestUnsubscribe
  { requestUnsubscribePaths :: [Text]
  }

data RequestMalformedError
  = PayloadSizeOutOfBounds
  | UnexpectedBinaryPayload

  | JsonDecodeError Text
  | PayloadNotAnObject
  | UnexpectedType Text

  | SubscribePathsMissingOrMalformed Text
  | SubscribeTokenNotAString Text

  | UnsubscribePathsMissingOrMalformed Text
  deriving Show

-- * Response

data ResponsePayload
  = ResponsePayloadSubscribeSuccess ResponseSubscribeSuccess
  | ResponsePayloadeSubscribeFailure ResponseSubscribeFailure
  | ResponsePayloadUnsubscribeSuccess ResponseUnsubscribeSuccess
  | ResponsePayloadUnsubscribeFailure ResponseUnsubscribeFailure


data ResponseSubscribeSuccess
  = ResponseSubscribeSuccess
  { subscribeSuccessPathsValues :: [( Text, Maybe Value)] }

instance Aeson.ToJSON ResponseSubscribeSuccess where
  toJSON (ResponseSubscribeSuccess pathsValues) =
    Aeson.object
    [ "type"  .= ("subscribe" :: Text)
    , "status" .= Aeson.object
      [ "code" .= (200 :: Int)
      , "message" .= ("You've been successfully subscribed to the paths" :: Text)
      , "extra" .= Aeson.object [] ]
    , "paths" .=
      (pathsValues <&>
        \(path, value) ->
          Aeson.object [ "path" .= path, "value" .= value ])
    ]

data ResponseSubscribeFailure
  = ResponseSubscribeFailure
  { subscribeFailureStatusCode :: Int
  , subscribeFailureMessage :: Text
  , subscribeFailurePaths :: Maybe [Text]
  , subscribeFailureExtraData :: Maybe Value
  }

instance Aeson.ToJSON ResponseSubscribeFailure where
  toJSON (ResponseSubscribeFailure code message mbPaths extra) =
    Aeson.object $ baseKeyValues <> addonKeyValues
    where
      baseKeyValues :: [ Aeson.Pair ]
      baseKeyValues =
        [ "type"  .= ("subscribe" :: Text)
        , "status" .= Aeson.object
          [ "code" .= (code :: Int)
          , "message" .= (message :: Text)
          , "extra" .= extra
          ]
        ]

      addonKeyValues :: [ Aeson.Pair ]
      addonKeyValues =
        case mbPaths of
          Just paths -> [ "paths" .= paths ]
          Nothing  -> []

data ResponseUnsubscribeSuccess
  = ResponseUnsubscribeSuccess
  { unsubscribeSuccessPaths :: [Text]
  }

instance Aeson.ToJSON ResponseUnsubscribeSuccess where
  toJSON (ResponseUnsubscribeSuccess paths) =
    Aeson.object
    [ "type"  .= ("unsubscribe" :: Text)
    , "paths" .= paths
    , "status" .= Aeson.object
      [ "code" .= (200 :: Int)
      , "message" .= ("You've been successfully unsubscribed from the paths" :: Text)
      , "extra" .= Aeson.object [] ]
    , "paths" .=  (paths <&> \path -> Aeson.object [ "path" .= path ])
    ]

data ResponseUnsubscribeFailure
  = ResponseUnsubscribeFailure
  { unsubscribeFailureStatusCode :: Int
  , unsubscribeFailureMessage :: Text
  , unsubscribeFailurePaths :: Maybe [Text]
  , unsubscribeFailureExtraData :: Maybe Value
  }

instance Aeson.ToJSON ResponseUnsubscribeFailure where
  toJSON (ResponseUnsubscribeFailure code message mbPaths extraData) =
    Aeson.object $ baseKeyValues <> addonKeyValues
    where
      baseKeyValues :: [ Aeson.Pair ]
      baseKeyValues =
        [ "type"  .= ("unsubscribe" :: Text)
        , "status" .= Aeson.object
          [ "code" .= (code :: Int)
          , "message" .= (message :: Text)
          , "extra" .= extraData
          ]
        ]

      addonKeyValues :: [ Aeson.Pair ]
      addonKeyValues =
        case mbPaths of
          Just paths -> [ "paths" .= paths ]
          Nothing  -> []
          

-- * Parsing

maxPayloadBytes :: Int
maxPayloadBytes = 1_000_000

checkBounds
  :: LBS.ByteString
  -> Either RequestMalformedError LBS.ByteString
checkBounds lazyBS = isOutOfBoundsAcc lazyBS 0
  where
    isOutOfBoundsAcc LBS.Empty _ = Right lazyBS
    isOutOfBoundsAcc (LBS.Chunk chunk rest) accSize =
      let accSize' = accSize + BS.length chunk in
        if accSize' > maxPayloadBytes
        then Left PayloadSizeOutOfBounds
        else isOutOfBoundsAcc rest accSize'

parseDataMessage
  :: WS.DataMessage -> RequestPayload
parseDataMessage (WS.Binary _ ) = RequestPayloadMalformed UnexpectedBinaryPayload
parseDataMessage (WS.Text utf8EncodedLazyByteString _ ) =
  case parsedPayload of
    (Left malformed) -> RequestPayloadMalformed malformed
    (Right (Left subscribe)) -> RequestPayloadSubscribe subscribe
    (Right (Right unsubscribe)) -> RequestPayloadUnsubscribe unsubscribe
  where
    parsedPayload
      :: Either RequestMalformedError (Either RequestSubscribe RequestUnsubscribe)
    parsedPayload = do
      let
        parseEither :: Aeson.Parser a -> Either String a
        parseEither parser = Aeson.parseEither (const parser) ()

        mapError = flip first
      
      boundedBytestring <- checkBounds utf8EncodedLazyByteString
      clientPayloadAeson <- Aeson.eitherDecode @Value boundedBytestring
        `mapError` (JsonDecodeError . Text.pack)

      case clientPayloadAeson of
        (Aeson.Object clientPayloadObject) -> do
          payloadType <- parseEither @RequestType (clientPayloadObject .: "type")
            `mapError` (UnexpectedType . Text.pack)

          case payloadType of
            RequestSubscribeType -> do
              pathsParsed <- parseEither @[Text] (clientPayloadObject .: "paths")
                `mapError` (SubscribePathsMissingOrMalformed . Text.pack)

              mbToken <- parseEither @(Maybe Text) (clientPayloadObject .:? "token")
                `mapError` (SubscribeTokenNotAString . Text.pack)

              pure $ Left $ RequestSubscribe pathsParsed mbToken

            RequestUnsubscribeType -> do
              parsedPaths <- parseEither @[Text] (clientPayloadObject .: "paths")
                `mapError` (UnsubscribePathsMissingOrMalformed . Text.pack)

              pure $ Right $ RequestUnsubscribe parsedPaths

        _ -> Left PayloadNotAnObject

data RequestType
  = RequestSubscribeType
  | RequestUnsubscribeType

instance Aeson.FromJSON RequestType where
  parseJSON (Aeson.String "subscribe") = pure RequestSubscribeType
  parseJSON (Aeson.String "unsubscribe") = pure RequestUnsubscribeType
  parseJSON _ = fail "Expecting 'type' to be either 'subscribe' or 'unsubscribe'"
  

