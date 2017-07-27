{-# LANGUAGE OverloadedStrings #-}

module HttpServer (new) where

import Data.Text (Text)
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
    get (regex "^") $
        request >>= json . statusFor . Wai.pathInfo

    put (regex "^") $ do
      req <- request
      value <- jsonData
      let putCommand = Core.Put (Wai.pathInfo req) value
      liftIO $ Core.enqueuePut putCommand core
      status status201

-- stub
statusFor :: [Text] -> Text
statusFor ["a"] = "{downloading}"
statusFor ["a", "b"] = "{failed}"
statusFor ["a", "b", "c"] = "{succeeded}"
statusFor _ = "{dikke Kip}"
