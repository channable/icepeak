#!/bin/bash

set -e

export STACK_ROOT=".stack-root"
export STACK_WORK=".stack-work"

stack --no-terminal "$@"
