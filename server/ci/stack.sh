#!/bin/bash

set -e

# NOTE: On every build, we get a different working directory. And as build
# input, the Stack root dir is provided in there. But installing GHC involves
# creating some scripts with absolute paths, so the Stack root directory should
# reside in a fixed location, it cannot be moved. Like any problem in computer
# science, we solve it with a layer of indirection: symlink /root/.stack to the
# stack-root in the build working dir. /root/.stack is the default STACK_ROOT.
ln --symbolic $(realpath ../../stack-root) /root/.stack

set -x

# NOTE: We do not quote the $@ here, so if the arguments contain whitespace,
# then this will do additional splitting. That is as intended: in the Concourse
# vars section we can only provide string vars, not arrays, so we need to
# provide the entire command line as a string and let Bash do the splitting.
stack --no-terminal $@
