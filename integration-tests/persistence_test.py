#!/usr/bin/env python2.7
"""
Test that we can read data back from disk after PUTing it.
"""
from __future__ import absolute_import, division, unicode_literals

import json
import requests
import websocket

auth = 'auth=mS7karSP9QbD2FFdgBk2QmuTna7fJyp7ll0Vg8gnffIBHKILSrusMslucBzMhwO'
json1 = {}
json2 = {'so': {}}
json3 = {'so': {'cool': {}}}
json4 = {'so': {'cool': {}, 'hot': {}}}

def read_after_write_check(url, data, expected_result=None):
    """
    First PUT the given `data` to the given `url`.
    Then make sure we can read the data back from disk.

    `expected_result` can be passed if it is different from `data`.
    """
    if expected_result is None:
        expected_result = data

    requests.put(url, json.dumps(data))

    with open('../icepeak.json', 'r') as f:
        on_disk = json.loads(f.read())
        assert on_disk == expected_result, \
               'Invalid data. Expected: {}. Got: {}.'.format(expected_result, on_disk)

    print 'Successfully wrote and read: {}'.format(data)

read_after_write_check('http://localhost:3000?{}'.format(auth), json1)
read_after_write_check('http://localhost:3000?{}'.format(auth), json2)
read_after_write_check('http://localhost:3000?{}'.format(auth), json3)
read_after_write_check('http://localhost:3000/so?{}'.format(auth), {'cool' : {}}, json3)
read_after_write_check('http://localhost:3000/so/hot?{}'.format(auth), json1, json4)
