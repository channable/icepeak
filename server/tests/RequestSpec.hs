{-# LANGUAGE OverloadedStrings #-}
module RequestSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           HTTPMethodInvalid (canonicalizeHTTPMethods, limitHTTPMethods)

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

import qualified Data.ByteString.Char8 as ByteString

spec :: Spec
spec = describe "Request method" $ with (pure app) $ do
  it "accepts method get" $ do
    (request HTTP.methodGet "/" [] "") `shouldRespondWith` 200
  it "accepts method post" $ do
    (request HTTP.methodPost "/" [] "") `shouldRespondWith` 200
  it "accepts method head" $ do
    (request HTTP.methodHead "/" [] "") `shouldRespondWith` 200
  it "accepts method put" $ do
    (request HTTP.methodPut "/" [] "") `shouldRespondWith` 200
  it "accepts method delete" $ do
    (request HTTP.methodDelete "/" [] "") `shouldRespondWith` 200
  it "accepts method trace" $ do
    (request HTTP.methodTrace "/" [] "") `shouldRespondWith` 200
  it "accepts method connect" $ do
    (request HTTP.methodConnect "/" [] "") `shouldRespondWith` 200
  it "accepts method options" $ do
    (request HTTP.methodOptions "/" [] "") `shouldRespondWith` 200
  it "accepts method patch" $ do
    (request HTTP.methodPatch "/" [] "") `shouldRespondWith` 200

  it "declines other methods yqus" $ do
    (request yqus "/" [] "") `shouldRespondWith` 400
  it "declines other methods badmethod" $ do
    (request badMethod "/" [] "") `shouldRespondWith` 400
  it "declines other methods invalid" $ do
    (request invalid "/" [] "") `shouldRespondWith` 400

app :: Wai.Application
app = (canonicalizeHTTPMethods . limitHTTPMethods) return200

return200 :: Wai.Application
return200 _ respond = respond $
    Wai.responseLBS HTTP.status200 [("Content-Type", "text/plain")] ""

yqus :: HTTP.Method
yqus = ByteString.pack "YQUS"

badMethod :: HTTP.Method
badMethod = ByteString.pack "BADMETHOD"

invalid :: HTTP.Method
invalid = ByteString.pack "INVALID"
