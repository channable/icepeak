{-# LANGUAGE OverloadedStrings #-}

-- | Functions are exported in descending order of abstraction level.
module Icepeak.Client
  ( setAtLeafRequest
  , requestPathForIcepeakPath
  ) where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Binary.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.URI as URI

-- | Return a HTTP request for setting a value at the leaf of a path.
setAtLeafRequest :: ToJSON a => [Text] -> a -> HTTP.Request
setAtLeafRequest path leaf =
  HTTP.defaultRequest
    { HTTP.method = "PUT"
    , HTTP.path = requestPathForIcepeakPath path
    , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode leaf)
    }

-- | Return the request path for an Icepeak path.
requestPathForIcepeakPath :: [Text] -> ByteString
requestPathForIcepeakPath =
  ByteString.Lazy.toStrict .
    Binary.Builder.toLazyByteString .
      URI.encodePathSegments
