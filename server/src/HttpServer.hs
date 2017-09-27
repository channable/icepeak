{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Types
import Network.Wai (Application)
import Web.Scotty (delete, get, json, jsonData, put, regex, middleware, request, scottyApp, status, ActionM)
import qualified Network.Wai as Wai

import JwtMiddleware (jwtMiddleware)
import Core (Core (..), EnqueueResult (..))
import Config (Config (..))
import qualified Core

new :: Core -> IO Application
new core =
  scottyApp $ do
    when (configEnableJwtAuth $ coreConfig core) $
      middleware $ jwtMiddleware $ configJwtSecret $ coreConfig core

    get (regex "^") $ do
      path <- Wai.pathInfo <$> request
      maybeValue <- liftIO $ Core.getCurrentValue core path
      maybe (status status404) json maybeValue

    put (regex "^") $ do
      path <- Wai.pathInfo <$> request
      value <- jsonData
      result <- liftIO $ Core.enqueueOp (Core.Put path value) core
      buildResponse result

    delete (regex "^") $ do
      path <- Wai.pathInfo <$> request
      result <- liftIO $ Core.enqueueOp (Core.Delete path) core
      buildResponse result

buildResponse :: EnqueueResult -> ActionM ()
buildResponse Enqueued = status accepted202
buildResponse Dropped  = status serviceUnavailable503
