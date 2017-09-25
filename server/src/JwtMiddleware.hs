{-# LANGUAGE OverloadedStrings #-}
-- | This module provides functionality for verifying the JSON Web Tokens in a wai setting.
module JwtMiddleware where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson            ((.=))
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString       as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import qualified Data.Text             as Text
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Clock
import qualified Network.HTTP.Types    as Http
import qualified Network.Wai           as Wai
import qualified Web.JWT               as JWT

import           AccessControl
import           JwtAuth
import           Store                 (Path)

-- | Defines the kinds of errors that cause authorization to fail.
data AuthError
  = TokenError TokenError
    -- ^ Authorization was denied due to an invalid token.
  | OperationNotAllowed
    -- ^ Authorization was denied because the operation is not allowed by the token.

-- | Result of checking authorization
data AuthResult
  = AuthRejected AuthError
    -- ^ Authorization was denied because of the specified reason
  | AuthAccepted
    -- ^ Authorization was successful

-- * Requests

-- | Check whether accessing the given path with the given mode is authorized by
-- the token supplied in the request headers or query string (which may not be
-- present, then failing the check).
isRequestAuthorized :: Http.RequestHeaders -> Http.Query -> POSIXTime -> Maybe JWT.Secret -> Path -> AccessMode -> AuthResult
isRequestAuthorized headers query now maybeSecret path mode =
  case getRequestClaim headers query now maybeSecret of
    Left err -> AuthRejected (TokenError err)
    Right claim | isAuthorizedByClaim claim path mode
                  -> AuthAccepted
                | otherwise
                  -> AuthRejected OperationNotAllowed

-- | Extract the JWT claim from the request.
getRequestClaim :: Http.RequestHeaders -> Http.Query -> POSIXTime -> Maybe JWT.Secret -> Either TokenError IcepeakClaim
getRequestClaim headers query now maybeSecret =
  let getTokenBytes = maybe (Left $ VerificationError TokenNotFound) Right (findTokenBytes headers query)
  in case maybeSecret of
       Nothing     ->
         -- authorization is enabled, but no secret provided, accept all tokens
         getTokenBytes >>= extractClaimUnverified
       Just secret -> getTokenBytes >>= extractClaim now secret

-- | Lookup a token, first in the @Authorization@ header of the request, then
-- falling back to the @auth@ query parameter.
findTokenBytes :: Http.RequestHeaders -> Http.Query -> Maybe SBS.ByteString
findTokenBytes headers query = headerToken headers <|> queryToken query

-- | Look up a token from the @Authorization@ header.
-- Header should be in the format @Bearer <token>@.
headerToken :: Http.RequestHeaders -> Maybe SBS.ByteString
headerToken =
  SBS.stripPrefix "Bearer " <=< List.lookup Http.hAuthorization

-- | Look up a token from the @auth@ query parameter
queryToken :: Http.Query -> Maybe SBS.ByteString
queryToken = join . lookup "auth"

-- * Responses

instance Aeson.ToJSON AuthError where
  toJSON aerr = case aerr of
    TokenError terr -> case terr of
      ClaimError ce -> Aeson.object [ "error" .= ce ]
      VerificationError ve | ve `elem` [TokenInvalid, TokenNotFound]
                             -> Aeson.object [ "error" .= Text.pack "invalid token format" ]
      _ -> Aeson.object [ "data" .= Aeson.Null ]
    OperationNotAllowed -> Aeson.object [ "error" .= Text.pack "not allowed" ]

-- | Generate a 401 Unauthorized response for a given authorization error.
errorResponseBody :: AuthError -> LBS.ByteString
errorResponseBody = Aeson.encode

-- * Middleware

jwtMiddleware :: Maybe JWT.Secret -> Wai.Application -> Wai.Application
jwtMiddleware secret app req respond = do
    now <- Clock.getPOSIXTime
    case getRequestClaim headers query now secret of
      Left err -> rejectUnauthorized (TokenError err)
      Right claim | isAuthorized claim -> app req respond
                  | otherwise -> rejectUnauthorized OperationNotAllowed
  where
    -- read request
    path = Wai.pathInfo req
    query = Wai.queryString req
    headers = Wai.requestHeaders req

    -- translate HTTP request methods to modes
    maybeMode | Wai.requestMethod req == Http.methodGet = Just ModeRead
              | Wai.requestMethod req == Http.methodPut = Just ModeWrite
              | Wai.requestMethod req == Http.methodDelete = Just ModeWrite
              | otherwise = Nothing

    isAuthorized claim = maybe False (isAuthorizedByClaim claim path) maybeMode

    rejectUnauthorized err = respond $ Wai.responseLBS
           Http.unauthorized401
           [(Http.hContentType, "application/json")]
           (Aeson.encode err)
