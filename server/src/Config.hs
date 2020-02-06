module Config (
  Config (..),
  MetricsConfig (..),
  StorageBackend (..),
  periodicSyncingEnabled,
  configInfo,
) where

import Control.Applicative (optional)
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read
import qualified Data.Char as Char
import Data.Maybe (isJust)
import Data.String (fromString)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Web.JWT as JWT


data StorageBackend = File | Sqlite | Postgres

-- command-line arguments
data Config = Config
  { configDataFile :: Maybe FilePath
  , configPort :: Int
    -- | Enables the use of JWT for authorization in JWT.
  , configEnableJwtAuth :: Bool
  -- | The secret used for verifying the JWT signatures. If no secret is
  -- specified even though JWT authorization is enabled, tokens will still be
  -- used, but not be verified.
  , configJwtSecret :: Maybe JWT.Signer
  , configMetricsEndpoint :: Maybe MetricsConfig
  , configQueueCapacity :: Word
  , configSyncIntervalMicroSeconds :: Maybe Int
  -- | Enable journaling, only in conjunction with periodic syncing
  , configEnableJournaling :: Bool
  -- | Indicates that the sentry logging is disabled, can be used to overwrite
  -- ```configSentryDSN``` or the environment variable
  , configDisableSentryLogging :: Bool
  -- | The SENTRY_DSN key that Sentry uses to communicate, if not set, use Nothing.
  -- Just indicates that a key is given.
  , configSentryDSN :: Maybe String
  , configStorageBackend :: StorageBackend
  }

data MetricsConfig = MetricsConfig
  { metricsConfigHost :: Warp.HostPreference
  , metricsConfigPort :: Warp.Port
  }

periodicSyncingEnabled :: Config -> Bool
periodicSyncingEnabled = isJust . configSyncIntervalMicroSeconds

-- Parsing of command-line arguments

type EnvironmentConfig = [(String, String)]

configParser :: EnvironmentConfig -> Parser Config
configParser environment = Config
  -- Note: If no --data-file is given we default either to icepeak.json or icepeak.db
  <$> optional (strOption (long "data-file" <>
                   metavar "DATA_FILE" <>
                   help "File where data is persisted to. Default: icepeak.json"))
  <*> option auto (long "port" <>
                   metavar "PORT" <>
                   maybe (value 3000) value (readFromEnvironment "ICEPEAK_PORT") <>
                   help "Port to listen on, defaults to the value of the ICEPEAK_PORT environment variable if present, or 3000 if not")
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
  <*> option auto
       (long "queue-capacity" <>
        metavar "INTEGER" <>
        value 256 <>
        help ("Smaller values decrease the risk of data loss during a crash, while " <>
              "higher values result in more requests being accepted in rapid succession."))
  <*> optional (option timeDurationReader
       (long "sync-interval" <>
        metavar "DURATION" <>
        help ("If supplied, data is only persisted to disc every DURATION time units." <>
              "The units 'm' (minutes), 's' (seconds) and 'ms' (milliseconds) can be used. " <>
              "When omitting this argument, data is persisted after every modification")))
  <*> switch (long "journaling" <>
             help "Enable journaling. This only has an effect when periodic syncing is enabled.")
  <*> switch (long "disable-sentry-logging" <>
             help "Disable error logging via Sentry")
  <*> optional (strOption (
              long "sentry-dsn" <>
              metavar "SENTRY_DSN" <>
              environ "SENTRY_DSN" <>
              help "Sentry DSN used for Sentry logging, defaults to the value of the SENTRY_DSN environment variable if present. If no secret is passed, Sentry logging will be disabled."))
  <*> storageBackend

  where
    environ var = foldMap value (lookup var environment)

    readFromEnvironment :: Read a => String -> Maybe a
    readFromEnvironment var = lookup var environment >>= Read.readMaybe

    secretOption m = JWT.hmacSecret . Text.pack <$> strOption m

configInfo :: EnvironmentConfig -> ParserInfo Config
configInfo environment = info parser description
  where
    parser = helper <*> configParser environment
    description = fullDesc <>
      header "Icepeak - Fast Json document store with push notification support."

-- * Parsers

storageBackend :: Parser StorageBackend
storageBackend = fileBackend <|> sqliteBackend <|> postgresBackend

fileBackend :: Parser StorageBackend
-- The first 'File' here is the default value. We want --file to be used by default, when nothing
-- is specified on the command-line. This ensures backwards-compatibility.
fileBackend = flag File File (long "file" <> help "Use a file as the storage backend." )

sqliteBackend :: Parser StorageBackend
sqliteBackend = flag' Sqlite (long "sqlite" <> help "Use a sqlite file as the storage backend." )

postgresBackend :: Parser StorageBackend
postgresBackend = flag' Postgres (long "postgres" <> help "Use Postgres as the storage backend." )

-- * Reader functions

metricsConfigReader :: ReadM MetricsConfig
metricsConfigReader = eitherReader $ \input ->
  case List.break (== ':') input of
    (hostStr, ':':portStr) -> MetricsConfig (fromString hostStr) <$> Read.readEither portStr
    (_, _) -> Left "no port specified"

-- | Read an option as a time duration in microseconds.
timeDurationReader :: ReadM Int
timeDurationReader = eitherReader $ \input ->
  case List.break Char.isLetter input of
    ("", _) -> Left "no amount specified"
    (amount, unit) -> case lookup unit units of
      Nothing -> Left "invalid unit"
      Just factor -> fmap (* factor) $ Read.readEither amount
  where
    -- defines the available units and how they convert to microseconds
    units = [ ("s", 1000000)
            , ("ms", 1000)
            , ("m", 60000000)
            ]
