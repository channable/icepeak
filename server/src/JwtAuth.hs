{-# LANGUAGE OverloadedStrings #-}
module JwtAuth where

import           Control.Monad         ((<=<))
import           Data.Aeson            ((.:), (.=))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson
import           Data.Bifunctor        (first)
import qualified Data.ByteString       as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List             as List
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Data.Time.Clock.POSIX (POSIXTime)
import           Web.JWT               (JWT, UnverifiedJWT, VerifiedJWT)
import qualified Web.JWT               as JWT

-- * Claim datatypes

-- | Defines the structure of a JWT claim for Icepeak.
data IcepeakClaim = IcepeakClaim
  { icepeakClaimWhitelist :: [AuthPath]
    -- ^ The whitelist containing all authorizations.
  } deriving (Read, Show, Eq, Ord)

type Path = [Text]

data AuthPath = AuthPath
  { authPathPrefix :: Path
    -- ^ The prefix of all the paths to which this authorization applies.
  , authPathModes  :: [AccessMode]
    -- ^ The modes that are authorized on this path prefix.
  } deriving (Read, Show, Eq, Ord)

data AccessMode = ModeCreate | ModeRead | ModeUpdate | ModeDelete
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

-- * Token verification

data VerificationError
  = TokenUsedTooEarly
  | TokenExpired
  | TokenInvalid
  | TokenNotFound
  | TokenSignatureInvalid
  deriving (Show, Eq)

-- | Verify that the token is not used before it was issued.
verifyNotBefore :: POSIXTime -> JWT VerifiedJWT -> Either VerificationError (JWT VerifiedJWT)
verifyNotBefore now token =
 case JWT.nbf . JWT.claims $ token of
   Nothing -> Right token
   Just notBefore ->
     if now <= JWT.secondsSinceEpoch notBefore
       then Left TokenUsedTooEarly
       else Right token

-- | Verify that the token is not used after is has expired.
verifyExpiry :: POSIXTime -> JWT VerifiedJWT -> Either VerificationError (JWT VerifiedJWT)
verifyExpiry now token =
 case JWT.exp . JWT.claims $ token of
   Nothing -> Right token
   Just expiry ->
     if now > JWT.secondsSinceEpoch expiry
       then Left TokenExpired
       else Right token

-- | Verify that the token contains a valid signature.
verifySignature :: JWT.Secret -> JWT UnverifiedJWT -> Either VerificationError (JWT VerifiedJWT)
verifySignature secret token =
 case JWT.verify secret token of
   Nothing     -> Left TokenSignatureInvalid
   Just token' -> Right token'

decodeToken :: SBS.ByteString -> Either VerificationError (JWT UnverifiedJWT)
decodeToken bytes =
 case JWT.decode (Text.decodeUtf8 bytes) of
   Nothing    -> Left TokenInvalid
   Just token -> Right token

-- | Check that a token is valid at the given time for the given secret.
verifyToken :: POSIXTime -> JWT.Secret -> SBS.ByteString -> Either VerificationError (JWT VerifiedJWT)
verifyToken now secret = verifyNotBefore now
                       <=< verifyExpiry now
                       <=< verifySignature secret
                       <=< decodeToken

-- * Claim parsing

data TokenError
  = VerificationError VerificationError -- ^ JWT could not be verified.
  | ClaimError String                   -- ^ The claims do not fit the schema.
  deriving (Show, Eq)

-- | Verify the token and extract the icepeak claim from it.
extractClaim :: POSIXTime -> JWT.Secret -> SBS.ByteString -> Either TokenError IcepeakClaim
extractClaim now secret tokenBytes = do
  jwt <- first VerificationError $ verifyToken now secret tokenBytes
  claim <- first ClaimError $ getIcepeakClaim jwt
  pure claim

-- | Extract the icepeak claim from the token without verifying it.
extractClaimUnverified :: SBS.ByteString -> Either TokenError IcepeakClaim
extractClaimUnverified tokenBytes = do
  jwt <- first VerificationError $ decodeToken tokenBytes
  claim <- first ClaimError $ getIcepeakClaim jwt
  pure claim

getIcepeakClaim :: JWT r -> Either String IcepeakClaim
getIcepeakClaim token = do
  claimJson <- maybe (Left "Icepeak claim missing.") Right
    $ Map.lookup "icepeak"
    $ JWT.unregisteredClaims
    $ JWT.claims token
  Aeson.parseEither Aeson.parseJSON claimJson

-- * Authorization

-- | Defines the kinds of errors that cause authorization to fail.
data AuthError
  = TokenError TokenError
    -- ^ Authorization was denied due to an invalid token.
  | OperationNotAllowed Path AccessMode
    -- ^ Authorization was denied because the operation is not allowed by the token.

-- | Result of checking authorization
data AuthResult
  = AuthRejected AuthError
    -- ^ Authorization was denied because of the specified reason
  | AuthAccepted
    -- ^ Authorization was successful

-- | Check whether accessing the given path with the given mode is authorized by
-- the supplied token (which may not be present, always failing the check).
isAuthorizedByToken :: Maybe SBS.ByteString -> POSIXTime -> Maybe JWT.Secret -> Path -> AccessMode -> AuthResult
isAuthorizedByToken maybeTokenBytes now maybeSecret path mode =
  let getTokenBytes = maybe (Left $ VerificationError TokenNotFound) Right maybeTokenBytes
      claimOrError = case maybeSecret of
            -- authorization is enabled, but no secret provided, accept all tokens
            Nothing -> getTokenBytes >>= extractClaimUnverified
            Just secret -> getTokenBytes >>= extractClaim now secret
  in case claimOrError of
        Left err -> AuthRejected (TokenError err)
        Right claim | isAuthorizedByClaim claim path mode
                      -> AuthAccepted
                    | otherwise
                      -> AuthRejected (OperationNotAllowed path mode)

-- | Check whether accessing the given path with the given mode is authorized by
-- the supplied claim.
isAuthorizedByClaim :: IcepeakClaim -> Path -> AccessMode -> Bool
isAuthorizedByClaim claim path mode = any allows (icepeakClaimWhitelist claim) where
  allows (AuthPath prefix modes) = List.isPrefixOf prefix path && mode `elem` modes

-- this is temporary, to help smoothing the migration to JWT
errorResponseBody :: AuthError -> LBS.ByteString
errorResponseBody aerr = case aerr of
  TokenError terr -> case terr of
    ClaimError ce -> Aeson.encode $ Aeson.object [ "error" .= ce ]
    VerificationError ve | ve `elem` [TokenInvalid, TokenNotFound]
                           -> Aeson.encode $ Aeson.object [ "error" .= Text.pack "invalid token format" ]
    _ -> Aeson.encode $ Aeson.object [ "data" .= Aeson.Null ]
  OperationNotAllowed _ _ -> Aeson.encode $ Aeson.object [ "error" .= Text.pack "not allowed" ]

-- * JSON encoding and decoding

accessModeToText :: AccessMode -> Text
accessModeToText mode = case mode of
    ModeCreate -> "create"
    ModeRead   -> "read"
    ModeUpdate -> "update"
    ModeDelete -> "delete"

textToAccessMode :: Text -> Maybe AccessMode
textToAccessMode mode
  | mode == "create" = Just ModeCreate
  | mode == "read" = Just ModeRead
  | mode == "update" = Just ModeUpdate
  | mode == "delete" = Just ModeDelete
  | otherwise = Nothing

instance Aeson.ToJSON AccessMode where
  toJSON = Aeson.String . accessModeToText

instance Aeson.FromJSON AccessMode where
  parseJSON = Aeson.withText "mode string" $ \txt -> case textToAccessMode txt of
    Nothing -> fail "Invalid mode value."
    Just m  -> pure m

instance Aeson.ToJSON AuthPath where
  toJSON (AuthPath prefix modes) = Aeson.object
    [ "prefix" .= prefix
    , "modes" .= modes ]

instance Aeson.FromJSON AuthPath where
  parseJSON = Aeson.withObject "path and modes" $ \v -> AuthPath
    <$> v .: "prefix"
    <*> v .: "mode"

instance Aeson.ToJSON IcepeakClaim where
  toJSON claim = Aeson.object
    [ "version"   .= (1 :: Int)
    , "whitelist" .= icepeakClaimWhitelist claim
    ]

instance Aeson.FromJSON IcepeakClaim where
  parseJSON = Aeson.withObject "icepeak claim" $ \v -> do
    version <- v .: "version"
    if version == (1 :: Int)
      then IcepeakClaim <$> v .: "whitelist"
      else fail "unsupported version"
