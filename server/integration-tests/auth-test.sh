echo "it should not require auth on GET:"
  status=`curl -s -I localhost:3000/a/b | grep HTTP/1.1 | awk {'print $2'}`
  if [ $status -ne "401" ] ; then
    echo "\tpassed"
  else
    echo "\tFAILED"
  fi

echo "it should require auth on POST:"
  status=`curl -v -H "Content-Type: application/json" -X PUT -d '{"fo":"bar"}' -s -I localhost:3000/a/b | grep HTTP/1.1 | awk {'print $2'}`
  echo "$status"
  if [ $status -eq "201" ] ; then
    echo "\tpassed"
  else
    echo "\tFAILED"
  fi

  #
  # if [ "$i" -le "$LIMIT_REACHED_AFTER" -o "$i" -eq "$FORGIVEN_AT" ]; then
  #   echo "$STATUS_CODE should be 200 (OK)"
  #   if [ "$STATUS_CODE" -ne 200 ]; then exit 1; fi
  # else
  #   echo "$STATUS_CODE should be 429 (Too Many Requests)"
  #   if [ "$STATUS_CODE" -ne 429 ]; then exit 1; fi
