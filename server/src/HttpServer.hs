{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Control.Monad.IO.Class
import Network.HTTP.Types
import Network.Wai (Application)
import Web.Scotty (delete, get, json, jsonData, put, regex, request, scottyApp, status, ActionM)
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
      maybe (status status404) json maybeValue

    put (regex "^") $
      protected $ do
        path <- Wai.pathInfo <$> request
        value <- jsonData
        result <- liftIO $ Core.enqueueOp (Core.Put path value) core
        buildResponse result

    delete (regex "^") $
      protected $ do
        path <- Wai.pathInfo <$> request
        result <- liftIO $ Core.enqueueOp (Core.Delete path) core
        buildResponse result


buildResponse :: EnqueueResult -> ActionM ()
buildResponse Enqueued = status accepted202
buildResponse Dropped  = status serviceUnavailable503
