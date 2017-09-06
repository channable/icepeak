{-# LANGUAGE OverloadedStrings #-}

-- | Functions are exported in descending order of abstraction level.
--
-- >>> :set -XOverloadedStrings
-- >>> import Icepeak.Client (Client (..), setAtLeaf)
-- >>> import qualified Network.HTTP.Client as HTTP
-- >>> manager <- HTTP.newManager HTTP.defaultManagerSettings
-- >>> let client = Client manager "localhost" 3000 "mS7karSP9QbD2FFdgBk2QmuTna7fJyp7ll0Vg8gnffIBHKILSrusMslucBzMhwO"
-- >>> setAtLeaf client ["foo", "bar", "baz"] ([Just 1, Just 2, Nothing, Just 4] :: [Maybe Int])
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
  { clientHTTPManager :: HTTP.Manager
  , clientIcepeakHost :: ByteString
  , clientIcepeakPort :: Word16
  , clientIcepeakAuth :: ByteString
  }

-- | Set a value at the leaf of a path.
--
-- Will rethrow any exceptions thrown by the I/O actions from
-- "Network.HTTP.Client". Will not throw any other exceptions.
setAtLeaf :: (MonadIO m, ToJSON a) => Client -> [Text] -> a -> m HTTP.Status
setAtLeaf (Client http host port auth) path leaf =
  let request = setAtLeafRequest path leaf
      request' = request { HTTP.host = host, HTTP.port = fromIntegral port }
      request'' = HTTP.setQueryString [("auth", Just auth)] request'
  in liftIO . fmap HTTP.responseStatus $ HTTP.httpNoBody request'' http

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
