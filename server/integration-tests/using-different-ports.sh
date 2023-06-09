#!/usr/bin/env bash

cd "$(dirname "$0")"
cd ..

########## Preparations
cabal build -j icepeak
DATA_FILE=`mktemp`
echo '{"foo": {"a": 123}}' > "$DATA_FILE"
lsof -i :3000 > /dev/null
if [ $? -eq 0 ]; then
    echo "ERROR: Another program is already using port 3000"
    exit 1
fi
lsof -i :3001 > /dev/null
if [ $? -eq 0 ]; then
    echo "ERROR: Another program is already using port 3001"
    exit 1
fi
RESULT_CODE=0

########## Running tests
echo ""; echo "## Should default on port 3000"
cabal run -j icepeak -- --file --data-file="$DATA_FILE" > /dev/null &
ICEPEAK_PID=$!
sleep 1
RESULT=`curl -sS localhost:3000/foo/a`
if [ "$RESULT" -eq 123 ]; then
    echo "PASSED"
else
    echo "FAILED"
    (( RESULT_CODE=RESULT_CODE+1 ))
fi
kill -9 $ICEPEAK_PID; wait $ICEPEAK_PID 2> /dev/null

echo ""; echo "## Should use --port if given"
cabal run -j icepeak -- --file --data-file="$DATA_FILE" --port 3001 > /dev/null &
ICEPEAK_PID=$!
sleep 1
RESULT=`curl -sS localhost:3001/foo/a`
if [ "$RESULT" -eq "123" ]; then
    echo "PASSED"
else
    echo "FAILED"
    (( RESULT_CODE=RESULT_CODE+1 ))
fi
kill -9 $ICEPEAK_PID; wait $ICEPEAK_PID 2> /dev/null

echo ""; echo "## Should use ICEPEAK_PORT if given"
ICEPEAK_PORT=3001 cabal run -j icepeak -- --file --data-file="$DATA_FILE" > /dev/null &
ICEPEAK_PID=$!
sleep 1
RESULT=`curl -sS localhost:3001/foo/a`
if [ "$RESULT" -eq "123" ]; then
    echo "PASSED"
else
    echo "FAILED"
    (( RESULT_CODE=RESULT_CODE+1 ))
fi
kill -9 $ICEPEAK_PID; wait $ICEPEAK_PID 2> /dev/null

echo ""; echo "## Should prefer --port over ICEPEAK_PORT"
ICEPEAK_PORT=9999 cabal run -j icepeak -- --file --data-file="$DATA_FILE" --port=3001 > /dev/null &
ICEPEAK_PID=$!
sleep 1
RESULT=`curl -sS localhost:3001/foo/a`
if [ "$RESULT" -eq "123" ]; then
    echo "PASSED"
else
    echo "FAILED"
    (( RESULT_CODE=RESULT_CODE+1 ))
fi
kill -9 $ICEPEAK_PID; wait $ICEPEAK_PID 2> /dev/null

echo ""; echo "## Should ignore ICEPEAK_PORT if it is not a number"
ICEPEAK_PORT=notanumber cabal run -j icepeak -- --file --data-file="$DATA_FILE" > /dev/null &
ICEPEAK_PID=$!
sleep 1
RESULT=`curl -sS localhost:3000/foo/a`
if [ "$RESULT" -eq "123" ]; then
    echo "PASSED"
else
    echo "FAILED"
    (( RESULT_CODE=RESULT_CODE+1 ))
fi
kill -9 $ICEPEAK_PID; wait $ICEPEAK_PID 2> /dev/null

echo ""; echo "## Should fail if --port is not a number"
cabal run -j icepeak -- --file --data-file="$DATA_FILE" --port=notanumber > /dev/null 2> /dev/null
if [ "$?" -eq "0" ]; then
    echo "FAILED"
    (( RESULT_CODE=RESULT_CODE+1 ))
else
    echo "PASSED"
fi

########## Cleanup
rm "$DATA_FILE"

########## Evaluation
echo ""
if [ "$RESULT_CODE" -eq "0" ]; then
    echo "ALL PASSED"
    exit 0
else
    echo "$RESULT_CODE FAILURES"
    exit $RESULT_CODE
fi
