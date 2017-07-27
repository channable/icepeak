{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import  Data.Text (Text)
import qualified Network.Wai        as Wai
import Network.HTTP.Types
import qualified Core
import Control.Monad.IO.Class

main :: IO ()
main = do
  core <- Core.newCore

  scotty 3000 $ do
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
