#!/usr/bin/env bash

if [ ! -f "stack.yaml" ]; then
    echo "Run test in server package directory"
    exit 1
fi

########## Preparations
stack build

BACKEND_FLAGS=("--file" "--sqlite")

########## 1st Pass

for item in ${BACKEND_FLAGS[*]}
do
echo "--------------------- Testing backend: $item"
DATA_FILE=`mktemp`
stack exec -- icepeak $item --data-file="$DATA_FILE" > /dev/null &
ICEPEAK_PID=$!
echo "Icepeak started PID=$ICEPEAK_PID"
sleep 1
echo "writing data to /foo/a"
VALUE_FOO_A="123"
curl -X PUT -d "$VALUE_FOO_A" localhost:3000/foo/a
# make sure data is persisted
sleep 1
kill $ICEPEAK_PID
echo "Waiting for Icepeak to exit"
wait $ICEPEAK_PID
sleep 1

########## 2nd PASS
stack exec -- icepeak $item --data-file="$DATA_FILE" > /dev/null &
ICEPEAK_PID=$!
echo "Icepeak started PID=$ICEPEAK_PID"
sleep 1
echo "writing data to /foo/b"
curl -X PUT -d '456' localhost:3000/foo/b
echo "reading data from /foo/a"
RESULT=`curl -sS localhost:3000/foo/a`
kill $ICEPEAK_PID
wait $ICEPEAK_PID

########## Cleanup
rm "$DATA_FILE"
done

########## Evaluation
if [ "$RESULT" -eq "$VALUE_FOO_A" ]; then
    echo "PASSED"
    exit 0
else
    echo "FAILED"
    exit 1
fi
