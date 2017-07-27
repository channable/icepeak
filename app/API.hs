{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import  Data.Text (Text)
import qualified Network.Wai        as Wai

main :: IO ()
main = scotty 3000 $ do
    get (regex "^") $
        request >>= json . statusFor . Wai.pathInfo

    post "/status/:key" $ do
        json ("{ status: ok }" :: Text)

statusFor :: [Text] -> Text
statusFor ["a"] = "{downloading}"
statusFor ["a", "b"] = "{failed}"
statusFor ["a", "b", "c"] = "{succeeded}"
statusFor _ = "{dikke Kip}"
