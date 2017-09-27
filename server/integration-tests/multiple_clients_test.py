#!/usr/bin/env python2.7
from __future__ import absolute_import, division, unicode_literals

import json
import requests
import websocket

test_url ='http://localhost:3000/so/cool'

def validate_data(connections, expected_result):
    for connection in connections:
        print 'receiving {}'.format(connection)
        result = connection.recv()
        parsed_result = json.loads(result)

        assert parsed_result == expected_result, 'Input data: {} is different from output data: {}'.format(
            expected_result, parsed_result)

    print 'All 10 clients received {} successfully!'.format(expected_result)


def main():
    # Make sure the value is null initially
    requests.put(test_url, json.dumps(None))

    connections = []
    for i in range(10):
        connection = websocket.create_connection("ws://localhost:3000/so/cool")
        connections.append(connection)

    validate_data(connections, None)

    # Put some data into icepeak over HTTP
    new_data = {'status': 'freezing'}
    requests.put(test_url, json.dumps(new_data))

    # Make sure all clients get the new data
    validate_data(connections, new_data)

    for connection in connections:
        connection.send_close()
        # connection.close()

    # Reset the value to make the test idempotent
    requests.put(test_url, json.dumps(None))

if __name__ == '__main__':
    main()
