{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Control.Monad.IO.Class
import Network.HTTP.Types
import Network.Wai (Application)
import Web.Scotty (scottyApp, get, put, status, jsonData, regex, request, json)

import qualified Network.Wai as Wai

import Core (Core)

import qualified Core

new :: Core -> IO Application
new core =
  scottyApp $ do
    get (regex "^") $ do
      path <- Wai.pathInfo <$> request
      maybeValue <- liftIO $ Core.getCurrentValue core path
      case maybeValue of
        Just value -> json value
        Nothing -> status status404

    put (regex "^") $ do
      req <- request
      value <- jsonData
      let putCommand = Core.Put (Wai.pathInfo req) value
      liftIO $ Core.enqueuePut putCommand core
      status status201
