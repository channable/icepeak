{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Control.Monad.IO.Class
import Network.HTTP.Types
import Network.Wai (Application)
import Web.Scotty (delete, get, json, jsonData, put, regex, request, scottyApp, status)
import qualified Network.Wai as Wai

import Core (Core, EnqueueResult (..))
import Guardian(protected)
import qualified Core

new :: Core -> IO Application
new core =
  scottyApp $ do
    get (regex "^") $ do
      path <- Wai.pathInfo <$> request
      maybeValue <- liftIO $ Core.getCurrentValue core path
      case maybeValue of
        Just value -> json value
        Nothing    -> status status404

    put (regex "^") $
      protected $ do
        path <- Wai.pathInfo <$> request
        value <- jsonData
        result <- liftIO $ Core.enqueueOp (Core.Put path value) core
        case result of
          Enqueued -> status accepted202
          Dropped  -> status serviceUnavailable503

    delete (regex "^") $
      protected $ do
        path <- Wai.pathInfo <$> request
        result <- liftIO $ Core.enqueueOp (Core.Delete path) core
        case result of
          Enqueued -> status accepted202
          Dropped  -> status serviceUnavailable503
