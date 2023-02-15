#!/usr/bin/env python3
"""
This test creates 10 websocket connections, then PUTs some new data, and then tests that all 10 clients
received the same data.
"""

import asyncio
import json
import requests
import websockets

test_url = 'http://localhost:3000/so/cool'


async def validate_data(connections, expected_result):
    for connection in connections:
        result = await connection.recv()
        parsed_result = json.loads(result)

        assert parsed_result == expected_result, 'Input data: {} is different from output data: {}'.format(
            expected_result, parsed_result)

    print(f'All 10 clients received {expected_result} successfully!')


async def main():
    # Make sure the value is null initially
    requests.put(test_url, json.dumps(None))

    connections = []
    for _ in range(10):
        connection = await websockets.connect("ws://localhost:3000/so/cool")
        connections.append(connection)

    await validate_data(connections, None)

    # Put some data into icepeak over HTTP
    new_data = {'status': 'freezing'}
    requests.put(test_url, json.dumps(new_data))

    # Make sure all clients get the new data
    await validate_data(connections, new_data)

    for connection in connections:
        await connection.close()

    # Reset the value to make the test idempotent
    requests.put(test_url, json.dumps(None))


if __name__ == '__main__':
    asyncio.get_event_loop().run_until_complete(main())
