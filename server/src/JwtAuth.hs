{-# LANGUAGE OverloadedStrings #-}
-- | This module contains all the web framework independent code for parsing and verifying
-- JSON Web Tokens.
module JwtAuth where

import           Control.Monad         ((<=<))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson
import           Data.Bifunctor        (first)
import qualified Data.ByteString       as SBS
import qualified Data.Map.Strict       as Map
import qualified Data.Text.Encoding    as Text
import           Data.Time.Clock.POSIX (POSIXTime)
import           Web.JWT               (JWT, UnverifiedJWT, VerifiedJWT)
import qualified Web.JWT               as JWT

import           AccessControl

-- * Token verification

data VerificationError
  = TokenUsedTooEarly
  | TokenExpired
  | TokenInvalid
  | TokenNotFound
  | TokenSignatureInvalid
  deriving (Show, Eq)

-- | Check that a token is valid at the given time for the given secret.
verifyToken :: POSIXTime -> JWT.VerifySigner -> SBS.ByteString -> Either VerificationError (JWT VerifiedJWT)
verifyToken now secret = verifyNotBefore now
                       <=< verifyExpiry now
                       <=< verifySignature secret
                       <=< decodeToken

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
verifySignature :: JWT.VerifySigner -> JWT UnverifiedJWT -> Either VerificationError (JWT VerifiedJWT)
verifySignature secret token =
 case JWT.verify secret token of
   Nothing     -> Left TokenSignatureInvalid
   Just token' -> Right token'

decodeToken :: SBS.ByteString -> Either VerificationError (JWT UnverifiedJWT)
decodeToken bytes =
 case JWT.decode (Text.decodeUtf8 bytes) of
   Nothing    -> Left TokenInvalid
   Just token -> Right token

-- * Claim parsing

data TokenError
  = VerificationError VerificationError -- ^ JWT could not be verified.
  | ClaimError String                   -- ^ The claims do not fit the schema.
  deriving (Show, Eq)

-- | Verify the token and extract the icepeak claim from it.
extractClaim :: POSIXTime -> JWT.VerifySigner -> SBS.ByteString -> Either TokenError IcepeakClaim
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
  let (JWT.ClaimsMap claimsMap) = JWT.unregisteredClaims $ JWT.claims token
      maybeClaim = Map.lookup "icepeak" claimsMap
  claimJson <- maybe (Left "Icepeak claim missing.") Right maybeClaim
  Aeson.parseEither Aeson.parseJSON claimJson

-- * Token generation

-- | Add the icepeak claim to a set of JWT claims.
addIcepeakClaim :: IcepeakClaim -> JWT.JWTClaimsSet -> JWT.JWTClaimsSet
addIcepeakClaim claim claims = claims
  { JWT.unregisteredClaims = newClaimsMap <> JWT.unregisteredClaims claims }
    where
      newClaimsMap = JWT.ClaimsMap $ Map.fromList [("icepeak", Aeson.toJSON claim)]