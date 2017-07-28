{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import           Control.Monad.IO.Class
import           Network.HTTP.Types
import           Network.Wai            (Application)
import           Web.Scotty             (delete, get, json, jsonData, put,
                                         regex, request, scottyApp, status)

import qualified Network.Wai            as Wai
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
      path <- Wai.pathInfo <$> request
      value <- jsonData
      let putCommand = Core.Put path value
      result <- liftIO $ Core.enqueuePut putCommand core
      case result of
        Enqueued -> status accepted202
        Dropped  -> status serviceUnavailable503

    delete (regex "^") $ do
      path <- Wai.pathInfo <$> request
      -- we should add Delete ADT and then enqueueDelete.
      -- if the delete queue if full, we should return a 503,
      -- otherwise a 202.
      liftIO $ Core.deleteValue path core
      status status202
