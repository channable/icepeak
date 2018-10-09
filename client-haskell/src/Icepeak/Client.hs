{-# LANGUAGE OverloadedStrings #-}

-- | Functions are exported in descending order of abstraction level.
--
-- >>> :set -XOverloadedStrings
-- >>> import Icepeak.Client (Client (..), Config (..), deleteAtLeaf, setAtLeaf)
-- >>> import qualified Network.HTTP.Client as HTTP
-- >>> httpManager <- HTTP.newManager HTTP.defaultManagerSettings
-- >>> let client = Client httpManager (Config "localhost" 3000 (Just "<token>"))
-- >>> setAtLeaf client ["foo", "bar", "baz"] ([Just 1, Just 2, Nothing, Just 4] :: [Maybe Int])
-- Status {statusCode = 202, statusMessage = "Accepted"}
-- >>> deleteAtLeaf client ["foo", "bar"]
-- Status {statusCode = 202, statusMessage = "Accepted"}
module Icepeak.Client
  ( -- * Connection data
    Client (..)
  , Config (..)

    -- * Performing requests
    -- $updatebehavior
  , setAtLeaf
  , deleteAtLeaf

    -- * Constructing requests
  , Durability (..)
  , RequestOptions (..)
  , defaultRequestOptions
  , setAtLeafRequest
  , setAtLeafRequestWithOptions
  , deleteAtLeafRequest
  , deleteAtLeafRequestWithOptions
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

-- | Config paired with HTTP manager for making requests.
data Client = Client HTTP.Manager Config

-- | Client connection details: how to connect to Icepeak?
data Config = Config
  { configHost  :: ByteString
  , configPort  :: Word16
  , configToken :: Maybe ByteString
  } deriving (Eq, Show)

data Durability
  = EventualDurability
    -- ^ If the request is answered with a 202 response, it will eventually be
    -- processed, but no guarantees are made about when that will happen. A GET
    -- request issued after the modification could still return stale data.
    -- However, writes are ordered in that sending sequential (i.e. waiting on
    -- the previous request to be answered) PUT/DELETE requests are processed in
    -- order.
  | StrongDurability
    -- ^ If the request is answered with a 202 response, the modification has
    -- been applied and subsequent reads will receive the updated data.

-- | Options to further specify request behavior
newtype RequestOptions = RequestOptions { requestDurability :: Durability }

-- | Default options for requests.
defaultRequestOptions :: RequestOptions
defaultRequestOptions = RequestOptions EventualDurability

-- $updatebehavior Returns the status code of the HTTP response. Icepeak returns
-- 202 if the update was accepted, 503 if the high water mark was reached, and
-- 401 if the client has insufficient permissions (as determined by the supplied
-- JSON Web Token). Any other status code indicates a different error, or a
-- misconfigured proxy.
--
-- Will rethrow any exceptions thrown by the I/O actions from
-- "Network.HTTP.Client". Will not throw any other exceptions.

-- | Set a value at the leaf of a path.
setAtLeaf :: (MonadIO m, ToJSON a) => Client -> [Text] -> a -> m HTTP.Status
setAtLeaf (Client httpManager config) path leaf =
  let request = setAtLeafRequest config path leaf
  in liftIO . fmap HTTP.responseStatus $ HTTP.httpNoBody request httpManager

-- | Delete the value at the leaf of a path.
deleteAtLeaf :: MonadIO m => Client -> [Text] -> m HTTP.Status
deleteAtLeaf (Client httpManager config) path =
  let request = deleteAtLeafRequest config path
  in liftIO . fmap HTTP.responseStatus $ HTTP.httpNoBody request httpManager

-- | Return a HTTP request for setting a value at the leaf of a path.
setAtLeafRequest :: ToJSON a => Config -> [Text] -> a -> HTTP.Request
setAtLeafRequest = setAtLeafRequestWithOptions defaultRequestOptions

-- | Return a HTTP request for deleting a value at the leaf of a path.
deleteAtLeafRequest :: Config -> [Text] -> HTTP.Request
deleteAtLeafRequest = deleteAtLeafRequestWithOptions defaultRequestOptions

-- | Return a HTTP request for setting a value at the leaf of a path.
setAtLeafRequestWithOptions :: ToJSON a => RequestOptions -> Config -> [Text] -> a -> HTTP.Request
setAtLeafRequestWithOptions options config path leaf =
  (baseRequest config)
    { HTTP.method = "PUT"
    , HTTP.path = requestPathForIcepeakPath path (optionsToQuery options)
    , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode leaf)
    }

-- | Return a HTTP request for deleting a value at the leaf of a path.
deleteAtLeafRequestWithOptions :: RequestOptions -> Config -> [Text] -> HTTP.Request
deleteAtLeafRequestWithOptions options config path =
  (baseRequest config)
    { HTTP.method = "DELETE"
    , HTTP.path = requestPathForIcepeakPath path (optionsToQuery options)
    }

-- | Build a query string for request options that affect server behavior.
optionsToQuery :: RequestOptions -> [URI.QueryItem]
optionsToQuery opts = durabilityParam (requestDurability opts)
  where
    durabilityParam EventualDurability = []
    durabilityParam StrongDurability = [("durable", Nothing)]

-- | Return a template for requests off a config.
baseRequest :: Config -> HTTP.Request
baseRequest (Config host port maybeToken) =
  let mkAuthHeader token = (Header.hAuthorization, "Bearer " <> token)
  in HTTP.defaultRequest
    { HTTP.host = host
    , HTTP.port = fromIntegral port
    , HTTP.requestHeaders = toList $ fmap mkAuthHeader maybeToken
    }

-- | Return the request path for an Icepeak path.
requestPathForIcepeakPath :: [Text] -> [URI.QueryItem] -> ByteString
requestPathForIcepeakPath pathSegments query = toStrictBS builder
  where
    toStrictBS = ByteString.Lazy.toStrict . Binary.Builder.toLazyByteString
    builder = URI.encodePathSegments pathSegments <> URI.renderQueryBuilder True query
