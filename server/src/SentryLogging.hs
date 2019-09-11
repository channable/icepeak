-- | Module for logging (crash) reports to Sentry
module SentryLogging(
  getCrashLogger, logException, logCrashMessage, runWithCrashLogger
) where

import Control.Exception (handle, SomeException)
import Data.Foldable (for_)

import qualified System.Log.Raven as Sentry
import qualified System.Log.Raven.Transport.Debug as Sentry
import qualified System.Log.Raven.Types as Sentry

-- | Returns a Maybe SentryService which can be used to send error information
-- to Sentry. Return value is Nothing is the environment variable SENTRY_DSN is
-- not set.
getCrashLogger :: String -> IO Sentry.SentryService
getCrashLogger dsn = Sentry.initRaven dsn id Sentry.dumpRecord Sentry.errorFallback

-- | Send some exception that has occured to Sentry. Function does nothing when
-- Sentry service is Nothing
logException :: String -> Maybe Sentry.SentryService -> SomeException -> IO ()
logException name mSentryService exception =
  maybe
    (pure ())
    (\ss -> Sentry.register ss name Sentry.Fatal (show (exception :: SomeException)) id)
    mSentryService

-- | Send a crash message to Sentry, used in cases when no exception is available,
-- Which is the case for Scotty errors. Function does nothing when Sentry service
-- is Nothing.
logCrashMessage :: String -> Maybe Sentry.SentryService -> String -> IO ()
logCrashMessage name mSentryService message =
  for_ mSentryService (\service -> Sentry.register service name Sentry.Fatal message id)

-- | Run some IO operation with possible exceptions and log any exception that
-- may occur to Sentry.
runWithCrashLogger :: String -> Maybe Sentry.SentryService -> IO () -> IO ()
runWithCrashLogger name mSentryService = handle (logException name mSentryService)
