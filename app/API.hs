{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text.Lazy as LT

main :: IO ()
main = scotty 3000 $ do
    get "/status/:key" $ do
        key <- param "key"
        json $ statusFor key

    post "/status/:key" $ do
        json ("{ status: ok }" :: LT.Text)

statusFor :: LT.Text -> LT.Text
statusFor "a" = "{downloading}"
statusFor "b" = "{failed}"
statusFor "c" = "{succeeded}"
statusFor _ = "{dikkip}"
