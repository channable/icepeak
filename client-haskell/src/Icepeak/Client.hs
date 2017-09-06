{-# LANGUAGE OverloadedStrings #-}

-- | Functions are exported in descending order of abstraction level.
--
-- >>> :set -XOverloadedStrings
-- >>> import Icepeak.Client (Client (..), setAtLeaf)
-- >>> import qualified Network.HTTP.Client as HTTP
-- >>> httpManager <- HTTP.newManager HTTP.defaultManagerSettings
-- >>> let client = Client "localhost" 3000 "mS7karSP9QbD2FFdgBk2QmuTna7fJyp7ll0Vg8gnffIBHKILSrusMslucBzMhwO"
-- >>> setAtLeaf httpManager client ["foo", "bar", "baz"] ([Just 1, Just 2, Nothing, Just 4] :: [Maybe Int])
-- Status {statusCode = 202, statusMessage = "Accepted"}
module Icepeak.Client
  ( Client (..)
  , setAtLeaf
  , setAtLeafRequest
  , requestPathForIcepeakPath
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Text (Text)
import Data.Word (Word16)

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Binary.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI as URI

-- | How to connect to Icepeak?
data Client = Client
  { clientHost :: ByteString
  , clientPort :: Word16
  , clientAuth :: ByteString
  }

-- | Set a value at the leaf of a path.
--
-- Will rethrow any exceptions thrown by the I/O actions from
-- "Network.HTTP.Client". Will not throw any other exceptions.
setAtLeaf :: (MonadIO m, ToJSON a) => HTTP.Manager -> Client -> [Text] -> a -> m HTTP.Status
setAtLeaf httpManager client path leaf =
  let request = setAtLeafRequest client path leaf
  in liftIO . fmap HTTP.responseStatus $ HTTP.httpNoBody request httpManager

-- | Return a HTTP request for setting a value at the leaf of a path.
setAtLeafRequest :: ToJSON a => Client -> [Text] -> a -> HTTP.Request
setAtLeafRequest (Client host port auth) path leaf =
  HTTP.defaultRequest
    { HTTP.host = host
    , HTTP.port = fromIntegral port
    , HTTP.method = "PUT"
    , HTTP.path = requestPathForIcepeakPath path
    , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode leaf)
    }
  & HTTP.setQueryString [("auth", Just auth)]

-- | Return the request path for an Icepeak path.
requestPathForIcepeakPath :: [Text] -> ByteString
requestPathForIcepeakPath =
  ByteString.Lazy.toStrict .
    Binary.Builder.toLazyByteString .
      URI.encodePathSegments
