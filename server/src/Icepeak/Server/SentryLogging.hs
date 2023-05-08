-- | Module for logging (crash) reports to Sentry
module Icepeak.Server.SentryLogging(
  getCrashLogger, logCrashMessage
) where

import qualified System.Log.Raven as Sentry
import qualified System.Log.Raven.Transport.HttpConduit as Sentry
import qualified System.Log.Raven.Types as Sentry

-- | Returns a Maybe SentryService which can be used to send error information
-- to Sentry. Return value is Nothing is the environment variable SENTRY_DSN is
-- not set.
getCrashLogger :: String -> IO Sentry.SentryService
getCrashLogger dsn = Sentry.initRaven dsn id Sentry.sendRecord Sentry.stderrFallback

-- | Send a crash message to Sentry, used in cases when no exception is available,
-- Which is the case for Scotty errors. Function does nothing when Sentry service
-- is Nothing.
logCrashMessage :: String -> Sentry.SentryService -> String -> IO ()
logCrashMessage name service message = Sentry.register service name Sentry.Fatal message id
