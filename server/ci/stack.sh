#!/bin/bash

set -e

# NOTE: Stack expects STACK_ROOT to be an absolute path,
# and STACK_WORK to be relative.
export STACK_ROOT="${PWD}/.stack-root"
export STACK_WORK=".stack-work"

# NOTE: We do not quote the $@ here, so if the arguments contain whitespace,
# then this will do additional splitting. That is as intended: in the Concourse
# vars section we can only provide string vars, not arrays, so we need to
# provide the entire command line as a string and let Bash do the splitting.
stack --no-terminal $@
