{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Control.Monad.IO.Class
import Network.HTTP.Types
import Network.Wai (Application)
import Web.Scotty (delete, get, json, jsonData, put, regex, request, scottyApp, status)
import qualified Data.ByteString        as B
import           Control.Monad          (when, join)
import qualified Network.Wai as Wai

import Core (Core, EnqueueResult (..))

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

    put (regex "^") $ do
      authorized <- auth <$> request
      when authorized $ do
        path <- Wai.pathInfo <$> request
        value <- jsonData
        result <- liftIO $ Core.enqueueOp (Core.Put path value) core
        case result of
          Enqueued -> status accepted202
          Dropped  -> status serviceUnavailable503

    delete (regex "^") $ do
      authorized <- auth <$> request
      when authorized $ do
        path <- Wai.pathInfo <$> request
        -- we should add Delete ADT and then enqueueDelete.
        -- if the delete queue if full, we should return a 503,
        -- otherwise a 202.
        -- TODO(nuno): Dry this enqueue->202/503 up.
        _ <- liftIO $ Core.enqueueOp (Core.Delete path) core
        status status202

auth :: Wai.Request -> Bool
auth = maybe False (==accessToken) . getAuthToken

accessToken :: B.ByteString
accessToken = "mS7karSP9QbD2FFdgBk2QmuTna7fJyp7ll0Vg8gnffIBHKILSrusMslucBzMhwO"

getAuthToken :: Wai.Request -> Maybe B.ByteString
getAuthToken = join . lookup "auth" . Wai.queryString
