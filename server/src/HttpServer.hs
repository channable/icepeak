{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Traversable (for)
import Network.HTTP.Types
import Network.Wai (Application)
import Web.Scotty (delete, get, json, jsonData, put, regex, middleware, request, scottyApp, status, ActionM)

import qualified Data.Text.Lazy as LText
import qualified Network.Wai as Wai
import qualified Web.Scotty.Trans as Scotty

import JwtMiddleware (jwtMiddleware)
import Core (Core (..), EnqueueResult (..))
import Config (Config (..))
import qualified Store
import qualified Core
import qualified Metrics

new :: Core -> IO Application
new core =
  scottyApp $ do
    -- first middleware is the outermost. this has to be the metrics middleware
    -- in order to intercept all requests their corresponding responses
    forM_ (coreMetrics core) $ middleware . metricsMiddleware

    when (configEnableJwtAuth $ coreConfig core) $
      middleware $ jwtMiddleware $ configJwtSecret $ coreConfig core

    get (regex "^") $ do
      path <- Wai.pathInfo <$> request
      maybeValue <- liftIO $ Core.getCurrentValue core path
      maybe (status status404) json maybeValue

    put (regex "^") $ do
      path <- Wai.pathInfo <$> request
      value <- jsonData
      result <- postModification core (Store.Put path value)
      buildResponse result

    delete (regex "^") $ do
      path <- Wai.pathInfo <$> request
      result <- postModification core (Store.Delete path)
      buildResponse result

-- | Enqueue modification and wait for it to be processed, if desired by the client.
postModification :: (Scotty.ScottyError e, MonadIO m) => Core -> Store.Modification -> Scotty.ActionT e m EnqueueResult
postModification core op = do
  -- the parameter is parsed as type (), therefore only presence or absence is important
  durable <- maybeParam "durable"
  waitVar <- liftIO $ for durable $ \() -> newEmptyMVar
  result <- liftIO $ Core.tryEnqueueCommand (Core.Modify op waitVar) core
  liftIO $ for_ waitVar $ takeMVar
  pure result

buildResponse :: EnqueueResult -> ActionM ()
buildResponse Enqueued = status accepted202
buildResponse Dropped  = status serviceUnavailable503

metricsMiddleware :: Metrics.IcepeakMetrics -> Wai.Middleware
metricsMiddleware metrics app req sendResponse = app req sendWithMetrics
  where
    sendWithMetrics resp = do
      Metrics.notifyRequest (Wai.requestMethod req) (Wai.responseStatus resp) metrics
      sendResponse resp

maybeParam :: (Scotty.Parsable a, Scotty.ScottyError e, Monad m) => LText.Text -> Scotty.ActionT e m (Maybe a)
maybeParam name = fmap (parseMaybe <=< lookup name) Scotty.params where
  parseMaybe = either (const Nothing) Just . Scotty.parseParam
