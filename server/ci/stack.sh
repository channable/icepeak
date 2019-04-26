#!/bin/bash

set -e

export STACK_ROOT="${PWD}/.stack-root"
export STACK_WORK="${PWD}/.stack-work"

stack --no-terminal "$@"
