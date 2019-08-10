#!/usr/bin/env stack
-- stack --stack-yaml ../../server/stack.yaml runghc --package QuickCheck --package quickcheck-text -- -Wall

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (fail)

import AccessControl
import JwtAuth
import qualified Data.Map.Strict as Map
import qualified Web.JWT as JWT
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.putStrLn $ JWT.encodeSigned testSecret joseHeader testClaims

joseHeader = JWT.JOSEHeader
  { JWT.typ = Just "JWT"
  , JWT.cty = Nothing
  , JWT.alg = Just JWT.HS256
  , JWT.kid = Nothing
  }

testAccess :: IcepeakClaim
testAccess = IcepeakClaim
  [ AuthPath ["foo"] [ModeRead]
  , AuthPath ["bar", "baz"] [ModeRead, ModeWrite]
  ]

testClaims :: JWT.JWTClaimsSet
testClaims = addIcepeakClaim testAccess $ JWT.JWTClaimsSet
  { JWT.iss = Nothing
  , JWT.sub = Nothing
  , JWT.aud = Nothing
  , JWT.exp = Nothing
  , JWT.nbf = Nothing
  , JWT.iat = Nothing
  , JWT.jti = Nothing
  , JWT.unregisteredClaims = mempty
  }

testSecret :: JWT.Signer
testSecret = JWT.HMACSecret "foo"
