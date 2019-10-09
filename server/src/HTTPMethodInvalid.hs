module HTTPMethodInvalid (canonicalizeHTTPMethods, limitHTTPMethods) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Method as Method
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Checks that the HTTP method is one of the StdMethods.
-- StdMethod: HTTP standard method (as defined by RFC 2616, and PATCH which is defined by RFC 5789).
-- Otherwise sets the HTTP method to INVALID.
-- post: HTTP method is canonicalized
canonicalizeHTTPMethods :: Wai.Middleware
canonicalizeHTTPMethods app request respond = do
  let method = Wai.requestMethod request
      parsedMethod = either (\_ -> invalid) (Method.renderStdMethod) (Method.parseMethod method)
      request' = request { Wai.requestMethod = parsedMethod }
  app request' respond

-- | Early exit for INVALID HTTP methods.
-- pre: HTTP method is canonicalized.
limitHTTPMethods :: Wai.Middleware
limitHTTPMethods app request respond =
  if Wai.requestMethod request == invalid
    then respond (Wai.responseLBS HTTP.badRequest400 [(HTTP.hContentType, (ByteString.pack "application/json"))] (LBS.pack "{}"))
    else app request respond

invalid :: Method.Method
invalid = ByteString.pack "INVALID"
