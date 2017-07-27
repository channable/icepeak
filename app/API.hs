{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import  Data.Text (Text)
import qualified Network.Wai        as Wai
import Data.Aeson (Value(..))
import Network.HTTP.Types

main :: IO ()
main = scotty 3000 $ do
    get (regex "^") $
        request >>= json . statusFor . Wai.pathInfo

    put (regex "^") $ do
      req <- request
      payload <- jsonData
      status $ mkStatus (setStatus (Wai.pathInfo req) payload) "Created"


statusFor :: [Text] -> Text
statusFor ["a"] = "{downloading}"
statusFor ["a", "b"] = "{failed}"
statusFor ["a", "b", "c"] = "{succeeded}"
statusFor _ = "{dikke Kip}"

setStatus :: [Text] -> Value -> Int
setStatus ["a", "b"] _ = 201
setStatus _ _ = 404
