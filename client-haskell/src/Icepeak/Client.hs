{-# LANGUAGE OverloadedStrings #-}

-- | Functions are exported in descending order of abstraction level.
--
-- >>> :set -XOverloadedStrings
-- >>> import Icepeak.Client (Client (..), deleteAtLeaf, setAtLeaf)
-- >>> import qualified Network.HTTP.Client as HTTP
-- >>> httpManager <- HTTP.newManager HTTP.defaultManagerSettings
-- >>> let client = Client "localhost" 3000 (Just "<token>")
-- >>> setAtLeaf httpManager client ["foo", "bar", "baz"] ([Just 1, Just 2, Nothing, Just 4] :: [Maybe Int])
-- Status {statusCode = 202, statusMessage = "Accepted"}
-- >>> deleteAtLeaf httpManager client ["foo", "bar"]
-- Status {statusCode = 202, statusMessage = "Accepted"}
module Icepeak.Client
  ( -- * Connection data
    Client (..)

    -- * Performing requests
    -- $updatebehavior
  , setAtLeaf
  , deleteAtLeaf

    -- * Constructing requests
  , setAtLeafRequest
  , deleteAtLeafRequest
  , baseRequest
  , requestPathForIcepeakPath
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Word (Word16)

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Binary.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.URI as URI

-- | How to connect to Icepeak?
data Client = Client
  { clientHost  :: ByteString
  , clientPort  :: Word16
  , clientToken :: Maybe ByteString
  }

-- $updatebehavior Returns the status code of the HTTP response. Icepeak returns
-- 202 if the update was accepted, 503 if the high water mark was reached, and
-- 401 if the client has insufficient permissions (as determined by the supplied
-- JSON Web Token). Any other status code indicates a different error, or a
-- misconfigured proxy.
--
-- Will rethrow any exceptions thrown by the I/O actions from
-- "Network.HTTP.Client". Will not throw any other exceptions.

-- | Set a value at the leaf of a path.
setAtLeaf :: (MonadIO m, ToJSON a) => HTTP.Manager -> Client -> [Text] -> a -> m HTTP.Status
setAtLeaf httpManager client path leaf =
  let request = setAtLeafRequest client path leaf
  in liftIO . fmap HTTP.responseStatus $ HTTP.httpNoBody request httpManager

-- | Delete the value at the leaf of a path.
deleteAtLeaf :: MonadIO m => HTTP.Manager -> Client -> [Text] -> m HTTP.Status
deleteAtLeaf httpManager client path =
  let request = deleteAtLeafRequest client path
  in liftIO . fmap HTTP.responseStatus $ HTTP.httpNoBody request httpManager

-- | Return a HTTP request for setting a value at the leaf of a path.
setAtLeafRequest :: ToJSON a => Client -> [Text] -> a -> HTTP.Request
setAtLeafRequest client path leaf =
  (baseRequest client)
    { HTTP.method = "PUT"
    , HTTP.path = requestPathForIcepeakPath path
    , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode leaf)
    }

-- | Return a HTTP request for deleting a value at the leaf of a path.
deleteAtLeafRequest :: Client -> [Text] -> HTTP.Request
deleteAtLeafRequest client path =
  (baseRequest client)
    { HTTP.method = "DELETE"
    , HTTP.path = requestPathForIcepeakPath path
    }

-- | Return a template for requests off a client.
baseRequest :: Client -> HTTP.Request
baseRequest (Client host port maybeToken) =
  let mkAuthHeader token = (Header.hAuthorization, "Bearer " <> token)
  in HTTP.defaultRequest
    { HTTP.host = host
    , HTTP.port = fromIntegral port
    , HTTP.requestHeaders = toList $ fmap mkAuthHeader maybeToken
    }

-- | Return the request path for an Icepeak path.
requestPathForIcepeakPath :: [Text] -> ByteString
requestPathForIcepeakPath =
  ByteString.Lazy.toStrict .
    Binary.Builder.toLazyByteString .
      URI.encodePathSegments
