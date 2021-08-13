#!/bin/bash
# Report the success of a CI cronjob.
# Requires $CRONJOB_HEALTHCHECK_URL to be set in the calling job.

# Curl flags:
#  - --retry: retry this many times on failure
#  - --fail: fail on non-200 HTTP statuses
#  - --silent: don't show progress bar or errors
#  - --show-error: do show error messages
#  - --data: Include this as HTTP POST body (shows up in the log)
set -v

curl \
    --retry 3 \
    --fail \
    --silent \
    --show-error \
    --data "https://channable.semaphoreci.com/jobs/$SEMAPHORE_JOB_ID" \
    "$CRONJOB_HEALTHCHECK_URL"
