#/bin/bash

if [ ! -f "stack.yaml" ]; then
    echo "Run test in server package directory"
    exit 1
fi

########## Preparations
stack build
DATA_FILE=`mktemp`
echo '{}' > "$DATA_FILE"

########## 1st Pass
stack exec -- icepeak --data-file="$DATA_FILE" --sync-interval 60s --journaling > /dev/null &
ICEPEAK_PID=$!
echo "Icepeak started PID=$ICEPEAK_PID"
sleep 1
echo "writing data to /foo/b"
curl -X PUT -d '456' localhost:3000/foo/b
echo "writing data to /foo/a"
VALUE_FOO_A="123"
curl -X PUT -d "$VALUE_FOO_A" localhost:3000/foo/a
echo "Killing icepeak"
kill -9 $ICEPEAK_PID
wait $ICEPEAK_PID
sleep 1

########## 2nd PASS
stack exec -- icepeak --data-file="$DATA_FILE" --sync-interval 60s --journaling > /dev/null &
ICEPEAK_PID=$!
echo "Icepeak started PID=$ICEPEAK_PID"
sleep 1
echo "reading data from /foo/a"
RESULT=`curl -sS localhost:3000/foo/a`
kill $ICEPEAK_PID
wait $ICEPEAK_PID

########## Cleanup
rm "$DATA_FILE"
rm "$DATA_FILE.journal"

########## Evaluation
if [ "$RESULT" -eq "$VALUE_FOO_A" ]; then
    echo "PASSED"
    exit 0
else
    echo "FAILED"
    exit 1
fi
