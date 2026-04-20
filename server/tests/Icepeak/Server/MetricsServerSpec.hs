{-# LANGUAGE OverloadedStrings #-}
module Icepeak.Server.MetricsServerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Icepeak.Server.MetricsServer (metricsApp)

spec :: Spec
spec = describe "metricsApp" $ with (pure metricsApp) $ do
  describe "GET /healthcheck" $ do
    it "returns 200 OK" $
      get "/healthcheck" `shouldRespondWith` 200

    it "returns an empty body" $
      get "/healthcheck" `shouldRespondWith` "" { matchStatus = 200 }

  describe "GET /metrics" $ do
    it "returns 200 OK" $
      get "/metrics" `shouldRespondWith` 200
