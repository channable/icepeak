#!/bin/bash

set -e

# NOTE: Stack expects STACK_ROOT to be an absolute path,
# and STACK_WORK to be relative.
export STACK_ROOT="${PWD}/.stack-root"
export STACK_WORK=".stack-work"

stack --no-terminal "$@"
