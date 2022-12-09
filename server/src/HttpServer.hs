{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Control.Applicative (liftA2)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Text (pack)
import Network.HTTP.Types
import Network.Wai (Application)
import Web.Scotty (delete, get, json, jsonData, put, regex, middleware, request, scottyApp, status, ActionM)

import qualified Data.Text.Lazy as LText
import qualified Network.Wai as Wai
import qualified Web.Scotty.Trans as Scotty

import HTTPMethodInvalid (canonicalizeHTTPMethods,limitHTTPMethods)
import JwtMiddleware (jwtMiddleware)
import Core (Core (..), EnqueueResult (..))
import Config (Config (..))
import Logger (postLog, LogLevel(LogError))
import qualified Store
import qualified Core
import qualified Metrics

import qualified Prometheus

newtype Action e m a = Action { runAction :: Scotty.ActionT e m a }

instance (Scotty.ScottyError e, MonadIO m) => Functor (Action e m) where
  fmap f = Action . fmap f . runAction
instance (Scotty.ScottyError e, MonadIO m) => Applicative (Action e m) where
  pure = Action . return
  liftA2 f a b = Action $ liftA2 f (runAction a) (runAction b)
instance (Scotty.ScottyError e, MonadIO m) => Monad (Action e m) where
  m >>= f = Action $ runAction m >>= runAction . f
instance (Scotty.ScottyError e, MonadIO m) => MonadIO (Action e m) where
  liftIO = Action . Scotty.liftAndCatchIO
instance (Scotty.ScottyError e, MonadIO m) => Prometheus.MonadMonitor (Action e m) where
  doIO = liftIO

new :: Core -> IO Application
new core =
  scottyApp $ do
    -- First we check whether the request HTTP method is a recognised HTTP method.
    -- Any arbitrary ByteString is accepted as a request method and we store those 
    -- in the exposed metrics, this is a DoS vector.
    middleware canonicalizeHTTPMethods
    -- Second middleware is the metrics middleware in order to intercept
    -- all requests and their corresponding responses
    forM_ (coreMetrics core) $ middleware . metricsMiddleware
    -- Exit on unknown HTTP verb after the request has been stored in the metrics.
    middleware limitHTTPMethods
    -- Use the Sentry logger if available
    -- Scottys error handler will only catch errors that are thrown from within
    -- a ```liftAndCatchIO``` function.
    Scotty.defaultHandler (\e -> do
        liftIO $ postLog (coreLogger core) LogError . pack . show $ e
        status status503
        Scotty.text "Internal server error"
       )

    when (configEnableJwtAuth $ coreConfig core) $
      middleware $ jwtMiddleware $ configJwtSecret $ coreConfig core

    get (regex "^") $ measureRequestDuration core $ do
      path <- Wai.pathInfo <$> request
      maybeValue <- Scotty.liftAndCatchIO $ Core.getCurrentValue core path
      maybe (status status404) json maybeValue

    put (regex "^") $ measureRequestDuration core $ do
      path <- Wai.pathInfo <$> request
      value <- jsonData
      result <- postModification core (Store.Put path value)
      buildResponse result

    delete (regex "^") $ measureRequestDuration core $ do
      path <- Wai.pathInfo <$> request
      result <- postModification core (Store.Delete path)
      buildResponse result

measureRequestDuration :: (Scotty.ScottyError e, MonadIO m) => Core -> Scotty.ActionT e m a -> Scotty.ActionT e m a
measureRequestDuration core =
  runAction .  maybe id Metrics.measureHttpReqDuration (coreMetrics core) . Action

-- | Enqueue modification and wait for it to be processed, if desired by the client.
postModification :: (Scotty.ScottyError e, MonadIO m) => Core -> Store.Modification -> Scotty.ActionT e m EnqueueResult
postModification core op = do
  -- the parameter is parsed as type (), therefore only presence or absence is important
  durable <- maybeParam "durable"
  waitVar <- Scotty.liftAndCatchIO $ for durable $ \() -> newEmptyMVar
  result <- Scotty.liftAndCatchIO $ Core.tryEnqueueCommand (Core.Modify op waitVar) core
  when (result == Enqueued) $
    Scotty.liftAndCatchIO $ for_ waitVar $ takeMVar
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
