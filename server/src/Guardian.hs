{-# LANGUAGE OverloadedStrings #-}

module Guardian (protected) where

import Network.HTTP.Types
import Web.Scotty (request, status, ActionM)
import qualified Data.ByteString        as B
import           Control.Monad          (join)
import qualified Network.Wai as Wai
import           Data.SecureMem                  (toSecureMem)

protected :: ActionM () -> ActionM ()
protected action = do
    authorized <- auth <$> request
    if authorized then action else status status401

auth :: Wai.Request -> Bool
auth = maybe False (timeSafeEquals accessToken) . getAuthToken

timeSafeEquals :: B.ByteString -> B.ByteString -> Bool
timeSafeEquals a b = toSecureMem a == toSecureMem b

accessToken :: B.ByteString
accessToken = "mS7karSP9QbD2FFdgBk2QmuTna7fJyp7ll0Vg8gnffIBHKILSrusMslucBzMhwO"

getAuthToken :: Wai.Request -> Maybe B.ByteString
getAuthToken = join . lookup "auth" . Wai.queryString
