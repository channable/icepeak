#!/usr/bin/env stack
-- stack --stack-yaml ../../server/stack.yaml runghc --package QuickCheck --package quickcheck-text -- -Wall

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (fail)

import qualified Data.ByteString as SBS

import AccessControl
import JwtAuth
import qualified Data.Map.Strict as Map
import qualified Web.JWT as JWT
import qualified Data.Text.IO as Text

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
  , JWT.unregisteredClaims = Map.empty
  }

testSecret :: JWT.Secret
testSecret = JWT.binarySecret "foo"

main :: IO ()
main = Text.putStrLn $ JWT.encodeSigned JWT.HS256 testSecret testClaims

