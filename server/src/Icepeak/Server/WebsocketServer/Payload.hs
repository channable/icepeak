{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Icepeak.Server.WebsocketServer.Payload where

import Data.Aeson (Value, (.:), (.:?), (.=))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Word (Word16)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Internal as Lazy.ByteString
import qualified Data.Text as Text
import qualified Network.WebSockets as WebSockets

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
  | MissingOrUnexpectedType Text
  | SubscribePathsMissingOrMalformed Text
  | SubscribeTokenNotAString Text
  | UnsubscribePathsMissingOrMalformed Text
  deriving (Show)

-- * Update

data Update = Update
  { updatePath :: Text
  , updateValue :: Value
  }

instance Aeson.ToJSON Update where
  toJSON (Update path value) =
    Aeson.object
      [ "type" .= ("update" :: Text)
      , "path" .= path
      , "value" .= value
      ]

-- * Response

data ResponseSubscribeSuccess = ResponseSubscribeSuccess
  {subscribeSuccessPathsValues :: [(Text, Maybe Value)]}

instance Aeson.ToJSON ResponseSubscribeSuccess where
  toJSON (ResponseSubscribeSuccess pathsValues) =
    Aeson.object
      [ "type" .= ("subscribe" :: Text)
      , "code" .= (200 :: Int)
      , "message" .= ("You've been successfully subscribed to the paths" :: Text)
      , "extra" .= Aeson.object []
      , "paths"
          .= ( pathsValues
                <&> \(path, value) ->
                  Aeson.object ["path" .= path, "value" .= value]
             )
      ]

data ResponseSubscribeFailure = ResponseSubscribeFailure
  { subscribeFailureStatusCode :: Int
  , subscribeFailureMessage :: Text
  , subscribeFailurePaths :: Maybe [Text]
  , subscribeFailureExtraData :: Maybe Value
  }

instance Aeson.ToJSON ResponseSubscribeFailure where
  toJSON (ResponseSubscribeFailure code message mbPaths extra) =
    Aeson.object $ baseKeyValues <> addonKeyValues
   where
    baseKeyValues :: [Aeson.Pair]
    baseKeyValues =
      [ "type" .= ("subscribe" :: Text)
      , "code" .= (code :: Int)
      , "message" .= (message :: Text)
      , "extra" .= extra
      ]

    addonKeyValues :: [Aeson.Pair]
    addonKeyValues =
      case mbPaths of
        Just paths -> ["paths" .= paths]
        Nothing -> []

newtype ResponseUnsubscribeSuccess = ResponseUnsubscribeSuccess
  { unsubscribeSuccessPaths :: [Text]
  }

instance Aeson.ToJSON ResponseUnsubscribeSuccess where
  toJSON (ResponseUnsubscribeSuccess paths) =
    Aeson.object
      [ "type" .= ("unsubscribe" :: Text)
      , "paths" .= paths
      , "code" .= (200 :: Int)
      , "message" .= ("You've been successfully unsubscribed from the paths" :: Text)
      , "extra" .= Aeson.object []
      ]

data ResponseUnsubscribeFailure = ResponseUnsubscribeFailure
  { unsubscribeFailureStatusCode :: Int
  , unsubscribeFailureMessage :: Text
  , unsubscribeFailurePaths :: Maybe [Text]
  , unsubscribeFailureExtraData :: Maybe Value
  }

instance Aeson.ToJSON ResponseUnsubscribeFailure where
  toJSON (ResponseUnsubscribeFailure code message mbPaths extraData) =
    Aeson.object $ baseKeyValues <> addonKeyValues
   where
    baseKeyValues :: [Aeson.Pair]
    baseKeyValues =
      [ "type" .= ("unsubscribe" :: Text)
      , "code" .= (code :: Int)
      , "message" .= (message :: Text)
      , "extra" .= extraData
      ]

    addonKeyValues :: [Aeson.Pair]
    addonKeyValues =
      case mbPaths of
        Just paths -> ["paths" .= paths]
        Nothing -> []

-- * Close Conn Reason

data CloseType
  = TypeSizeOutOfBounds
  | TypeBinaryPayload
  | TypeJsonDecodeError
  | TypePayloadNotObject
  | TypeMissingOrUnexpectedType
  deriving (Show, Eq)

-- https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close#parameters

-- Status codes in the range 3000-3999 are reserved for use by
-- libraries, frameworks, and applications. The interpretation of these codes
-- is undefined by this protocol.

closeCode :: CloseType -> Word16
closeCode TypeSizeOutOfBounds = 3001
closeCode TypeBinaryPayload = 3002
closeCode TypeJsonDecodeError = 3003
closeCode TypePayloadNotObject = 3004
closeCode TypeMissingOrUnexpectedType = 3005

-- A string providing a custom WebSocket connection close reason (a concise human-readable prose explanation for the closure).
-- The value must be no longer than 123 bytes (encoded in UTF-8).

closeMessage :: CloseType -> Text
closeMessage TypeSizeOutOfBounds =
  "Received a payload size that is out of bounds"
closeMessage TypeBinaryPayload =
  "Received a payload using Binary instead of Text"
closeMessage TypeJsonDecodeError =
  "Received a payload that resulted in a JSON decode error"
closeMessage TypePayloadNotObject =
  "Received a JSON payload which is not an object"
closeMessage TypeMissingOrUnexpectedType =
  "Received a payload with a missing or unexpected value for 'type'"

-- * Parsing

maxPayloadBytes :: Int
maxPayloadBytes = 1_000_000

checkBounds
  :: Lazy.ByteString
  -> Either RequestMalformedError Lazy.ByteString
checkBounds lazyBS = isOutOfBoundsAcc lazyBS 0
 where
  isOutOfBoundsAcc Lazy.ByteString.Empty _ = Right lazyBS
  isOutOfBoundsAcc (Lazy.ByteString.Chunk chunk rest) accSize =
    let accSize' = accSize + ByteString.length chunk
    in  if accSize' > maxPayloadBytes
          then Left PayloadSizeOutOfBounds
          else isOutOfBoundsAcc rest accSize'

parseDataMessage
  :: WebSockets.DataMessage -> RequestPayload
parseDataMessage (WebSockets.Binary _) = RequestPayloadMalformed UnexpectedBinaryPayload
parseDataMessage (WebSockets.Text utf8EncodedLazyByteString _) =
  case parsedPayload of
    (Left malformed)            -> RequestPayloadMalformed malformed
    (Right (Left subscribe))    -> RequestPayloadSubscribe subscribe
    (Right (Right unsubscribe)) -> RequestPayloadUnsubscribe unsubscribe
 where
  parsedPayload
    :: Either RequestMalformedError (Either RequestSubscribe RequestUnsubscribe)
  parsedPayload = do
    let
      parseEither :: Aeson.Parser a -> Either String a
      parseEither parser = Aeson.parseEither (const parser) ()

      mapError = flip Bifunctor.first

    boundedBytestring <- checkBounds utf8EncodedLazyByteString
    clientPayloadAeson <-
      Aeson.eitherDecode @Value boundedBytestring
        `mapError` (JsonDecodeError . Text.pack)

    case clientPayloadAeson of
      (Aeson.Object clientPayloadObject) -> do
        payloadType <-
          parseEither @RequestType (clientPayloadObject .: "type")
            `mapError` (MissingOrUnexpectedType . Text.pack)

        case payloadType of
          RequestSubscribeType -> do
            pathsParsed <-
              parseEither @[Text] (clientPayloadObject .: "paths")
                `mapError` (SubscribePathsMissingOrMalformed . Text.pack)

            mbToken <-
              parseEither @(Maybe Text) (clientPayloadObject .:? "token")
                `mapError` (SubscribeTokenNotAString . Text.pack)

            pure $ Left $ RequestSubscribe pathsParsed mbToken
          RequestUnsubscribeType -> do
            parsedPaths <-
              parseEither @[Text] (clientPayloadObject .: "paths")
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
