#!/usr/bin/env python3.7
"""
Test PUTing some data into Icepeak and getting it back over a websocket.

Requires a running Icepeak instance.

Requirements can be installed with: pip install requests websockets
"""

import asyncio
import json
import requests
import websockets

# 1. Put some data into icepeak over HTTP
new_data = {'status': 'freezing'}
requests.put('http://localhost:3000/so/cool',
    json.dumps(new_data))

# 2. Get the data back over a websocket
async def hello(uri):
    async with websockets.connect(uri) as websocket:
        result = await websocket.recv()

        parsed_result = json.loads(result)

        assert new_data == parsed_result, 'Input data: {} is different from output data: {}'.format(
            new_data, parsed_result)

print('Initial data was successfully sent to client!')

asyncio.get_event_loop().run_until_complete(
    hello('ws://localhost:3000/so/cool'))
