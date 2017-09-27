#!/usr/bin/env python2.7
"""
Test that we can read data back from disk after PUTing it.
"""
from __future__ import absolute_import, division, unicode_literals

import json
import requests
import websocket

auth = ''
json1 = {}
json2 = {'so': {}}
json3 = {'so': {'cool': {}}}
json4 = {'so': {'cool': {}, 'hot': {}}}

icepeak_endpoint = 'http://localhost:3000'

def read_after_write_check(url_path, data, expected_result):
    """
    First PUT the given `data` to the given `url_path`.
    Then make sure we can read the `expected_result` back from disk.
    """
    url = '{}{}?{}'.format(icepeak_endpoint, url_path, auth)
    requests.put(url, json.dumps(data))

    with open('../icepeak.json', 'r') as f:
        on_disk = json.loads(f.read())
        assert on_disk == expected_result, \
               'Invalid data. Expected: {}. Got: {}.'.format(expected_result, on_disk)

    print 'Successfully wrote and read: {}'.format(data)

read_after_write_check('', json1, json1)
read_after_write_check('', json2, json2)
read_after_write_check('', json3, json3)
read_after_write_check('/so', {'cool' : {}}, json3)
read_after_write_check('/so/hot', json1, json4)
