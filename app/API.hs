{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import  Data.Text (Text)
import qualified Network.Wai        as Wai
-- import Data.Aeson (Value(..))
import Network.HTTP.Types
import qualified MainLoop as ML

main :: IO ()
main = scotty 3000 $ do
    get (regex "^") $
        request >>= json . statusFor . Wai.pathInfo

    put (regex "^") $ do
      req <- request
      value <- jsonData
      let _updatedValue = ML.handlePut $ ML.Put (Wai.pathInfo req) value
      status status201

-- stub
statusFor :: [Text] -> Text
statusFor ["a"] = "{downloading}"
statusFor ["a", "b"] = "{failed}"
statusFor ["a", "b", "c"] = "{succeeded}"
statusFor _ = "{dikke Kip}"
