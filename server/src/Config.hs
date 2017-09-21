module Config (
  Config (..),
  configInfo
) where

import Control.Applicative (optional)
import Data.Semigroup      ((<>))
import Data.List           (lookup)
import Options.Applicative (Parser, ParserInfo, fullDesc, header, help,
  helper, info, long, switch, metavar, strOption, value)
import qualified Data.Text as Text

import qualified Web.JWT as JWT

-- command-line arguments
data Config = Config {
    configDataFile :: FilePath,
    -- | Enables the use of JWT for authorization in JWT.
    configEnableJwtAuth :: Bool,
    -- | The secret used for verifying the JWT signatures. If no secret is
    -- specified even though JWT authorization is enabled, tokens will still be
    -- used, but not be verified.
    configJwtSecret :: Maybe JWT.Secret
}

-- Parsing of command-line arguments

configParser :: [(String, String)] -> Parser Config
configParser environment =
  Config
    <$> strOption (long "data-file" <>
                   metavar "DATA_FILE" <>
                   value "icepeak.json" <>
                   help "File where data is persisted to. Default: icepeak.json")
    <*> switch (long "enable-jwt-auth" <>
                help "Enable authorization using JSON Web Tokens.")
    <*> optional (secretOption (
                         long "jwt-secret" <>
                         metavar "JWT_SECRET" <>
                         environ "JWT_SECRET" <>
                         help "Secret used for JWT verification, defaults to the value of the JWT_SECRET environment variable if present. If no secret is passed, JWT tokens are not checked for validity."))
  where
    environ var = foldMap value (lookup var environment)
    secretOption m = JWT.secret . Text.pack <$> strOption m

configInfo :: [(String, String)] -> ParserInfo Config
configInfo environment = info parser description
  where
    parser = helper <*> configParser environment
    description = fullDesc <>
      header "Icepeak - Fast Json document store with push notification support."
