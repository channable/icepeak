#!/usr/bin/env python2.7
from __future__ import absolute_import, division, unicode_literals

import json
import requests
import websocket

# 1. Put some data into icepeak over HTTP
new_data = {'status': 'freezing'}
requests.put('http://localhost:3000/so/cool/', json.dumps(new_data))

# 2. Get the data back over a websocket
conn = websocket.create_connection("ws://localhost:3000/so/cool")
result = conn.recv()
parsed_result = json.loads(result)

assert new_data == parsed_result, 'Input data: {} is different from output data: {}'.format(
        new_data, parsed_result)
