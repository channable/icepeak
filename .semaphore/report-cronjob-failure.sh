#!/bin/bash
# Report the failure of a CI cronjob.
# Requires $CRONJOB_HEALTHCHECK_URL to be set in the calling job.
# See report-category-success.sh for an explanation of the flags.
# The only difference between this file and the success script is the `/fail`
# suffix to the URL.
set -v

curl \
    --retry 3 \
    --fail \
    --silent \
    --show-error \
    --data "https://channable.semaphoreci.com/jobs/$SEMAPHORE_JOB_ID" \
    "$CRONJOB_HEALTHCHECK_URL/fail"
