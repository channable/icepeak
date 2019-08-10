{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Semigroup      ((<>))
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           Options.Applicative
import           System.Environment  (getEnvironment)
import qualified Web.JWT             as JWT
import qualified Data.Time.Clock.POSIX as Clock

import           AccessControl
import           JwtAuth

data Config = Config
  { configJwtSecret      :: Maybe JWT.Signer
  , configExpiresSeconds :: Maybe Integer
  , configWhitelist      :: [AuthPath]
  }

type EnvironmentConfig = [(String, String)]

main :: IO ()
main = do
  env <- getEnvironment
  config <- execParser (configInfo env)
  now <- Clock.getPOSIXTime

  let joseHeader = JWT.JOSEHeader
        { JWT.typ = Just "JWT"
        , JWT.cty = Nothing
        , JWT.alg = Just JWT.HS256
        , JWT.kid = Nothing
        }

  let access = IcepeakClaim (configWhitelist config)
      claims = addIcepeakClaim access $ JWT.JWTClaimsSet
             { JWT.iss = Nothing
             , JWT.sub = Nothing
             , JWT.aud = Nothing
             , JWT.exp = fmap (\secs -> realToFrac secs + now) (configExpiresSeconds config) >>= JWT.numericDate
             , JWT.nbf = Nothing
             , JWT.iat = Nothing
             , JWT.jti = Nothing
             , JWT.unregisteredClaims = mempty
             }
      token = case configJwtSecret config of
                Nothing -> JWT.encodeUnsigned claims joseHeader
                Just key -> JWT.encodeSigned key joseHeader claims
  Text.putStrLn token

configParser :: EnvironmentConfig -> Parser Config
configParser environment = Config
  <$> optional (
       secretOption (long "jwt-secret"
                   <> metavar "JWT_SECRET"
                   <> environ "JWT_SECRET"
                   <> short 's'
                   <> help "Secret used for signing the JWT, defaults to the value of the JWT_SECRET environment variable if present. If no secret is passed, JWT tokens are not signed."))
  <*> optional(
        option auto (long "expires"
                  <> short 'e'
                  <> metavar "EXPIRES_SECONDS"
                  <> help "Generate a token that expires in EXPIRES_SECONDS seconds from now."))
  <*> many (option authPathReader
             (long "path"
             <> short 'p'
             <> metavar "PATH:MODES"
             <> help "Adds the PATH to the whitelist, allowing the access modes MODES. MODES can be 'r' (read), 'w' (write) or 'rw' (read/write). This option may be used more than once."
             ))
  where
    environ var = foldMap value (lookup var environment)
    secretOption m = JWT.hmacSecret . Text.pack <$> strOption m

configInfo :: EnvironmentConfig -> ParserInfo Config
configInfo environment = info parser description
  where
    parser = helper <*> configParser environment
    description = fullDesc <>
      header "Icepeak Token Generator - Generates and signs JSON Web Tokens for authentication and authorization in Icepeak."

authPathReader :: ReadM AuthPath
authPathReader = eitherReader (go . Text.pack) where
  go input = let (pathtxt, modetxt) = Text.breakOn ":" input
                 modeEither | modetxt == ":r" = Right [ModeRead]
                            | modetxt == ":w" = Right [ModeWrite]
                            | modetxt == ":rw" = Right [ModeRead, ModeWrite]
                            | otherwise = Left $ "Invalid mode: " ++ Text.unpack modetxt
                 pathComponents = filter (not . Text.null) $ Text.splitOn "/" pathtxt
             in AuthPath pathComponents <$> modeEither
