#!/usr/bin/env python3
"""
Test PUTing some data into Icepeak and getting it back over a websocket.

Requires a running Icepeak instance.

Requirements can be installed with: pip install requests websockets
"""

import asyncio
import json
import requests
import websockets

json1 = {}
json2 = {'so': {}}
json3 = {'so': {'cool': {}}}
json4 = {'so': {'cool': {}, 'hot': {}}}


# 2. Get the data back over a websocket
async def assert_result_equals(uri, expected_result):
    async with websockets.connect(uri) as websocket:
        result = await websocket.recv()

        parsed_result = json.loads(result)

        assert expected_result == parsed_result, 'Input data: {} is different from output data: {}'.format(
            expected_result, parsed_result)


def read_after_write_check(url_path, data, expected_result):
    # Note, we must send a durable request here, since we otherwise would have a race condition
    url = f'http://localhost:3000{url_path}?durable'
    print(f'Putting: {data} to {url}')
    requests.put(url, json.dumps(data))

    test = assert_result_equals(f'ws://localhost:3000', expected_result)
    asyncio.get_event_loop().run_until_complete(test)


read_after_write_check('', json1, json1)
read_after_write_check('', json2, json2)
read_after_write_check('', json3, json3)
read_after_write_check('/so', {'cool': {}}, json3)
read_after_write_check('/so/hot', json1, json4)

print('All read-after-write tests passed successfully!')
