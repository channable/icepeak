module Config (
  Config (..),
  MetricsConfig (..),
  configInfo,
) where

import Control.Applicative (optional)
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read
import Data.String (fromString)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Web.JWT as JWT

-- command-line arguments
data Config = Config
  { configDataFile :: FilePath
    -- | Enables the use of JWT for authorization in JWT.
  , configEnableJwtAuth :: Bool
    -- | The secret used for verifying the JWT signatures. If no secret is
    -- specified even though JWT authorization is enabled, tokens will still be
    -- used, but not be verified.
  , configJwtSecret :: Maybe JWT.Secret
  , configMetricsEndpoint :: Maybe MetricsConfig
  }

data MetricsConfig = MetricsConfig
  { metricsConfigHost :: Warp.HostPreference
  , metricsConfigPort :: Warp.Port
  }

metricsConfigReader :: ReadM MetricsConfig
metricsConfigReader = eitherReader $ \input ->
  case List.break (== ':') input of
    (hostStr, ':':portStr) -> MetricsConfig (fromString hostStr) <$> Read.readEither portStr
    (_, _) -> Left "no port specified"

-- Parsing of command-line arguments

type EnvironmentConfig = [(String, String)]

configParser :: EnvironmentConfig -> Parser Config
configParser environment = Config
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
  <*> optional (option metricsConfigReader
                 (long "metrics" <>
                  metavar "HOST:PORT" <>
                  help "If provided, Icepeak collects various metrics and provides them to Prometheus on the given endpoint."
                 ))
  where
    environ var = foldMap value (lookup var environment)
    secretOption m = JWT.secret . Text.pack <$> strOption m

configInfo :: EnvironmentConfig -> ParserInfo Config
configInfo environment = info parser description
  where
    parser = helper <*> configParser environment
    description = fullDesc <>
      header "Icepeak - Fast Json document store with push notification support."
