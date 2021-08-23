#!/bin/bash
# Report the start of a CI cronjob.
# Requires $CRONJOB_HEALTHCHECK_URL to be set in the calling job.
# See report-cronjob-success.sh for an explanation of the flags.
# The only difference between this file and the success script is the `/start`
# suffix to the URL and the guarantee of a successful exit.
set -v

curl \
    --retry 3 \
    --fail \
    --silent \
    --show-error \
    --data "https://channable.semaphoreci.com/jobs/$SEMAPHORE_JOB_ID" \
    "$CRONJOB_HEALTHCHECK_URL/start"

# Make sure the exit code is "success" so we don't stop the pipeline should
# the above fail.
exit 0
