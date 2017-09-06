#!/usr/bin/env python3

from json import dumps
from random import randint
from sys import argv
from urllib.error import HTTPError
from urllib.request import Request, urlopen


def put_payload(addr: str, payload):
    auth = 'mS7karSP9QbD2FFdgBk2QmuTna7fJyp7ll0Vg8gnffIBHKILSrusMslucBzMhwO'
    url = 'http://localhost:3000/{}?auth={}'.format(addr, auth)
    headers = {'Content-Type': 'application/json'}
    payload_bytes = dumps(payload).encode('utf8')
    request = Request(url, payload_bytes, headers, method='PUT')

    try:
        response = urlopen(request)
    except HTTPError as error:
        response = error

    print('{}: {}'.format(response.code, response.read().decode('utf8')))


def put_random_payload():
    x = str(randint(0, 1000))
    y = str(randint(0, 100))
    z = str(randint(0, 10))
    addr = '{}/{}/{}'.format(x, y, z)
    payloads = ('fmap', 'bimap', '<$>', '>>=', 'pure', 'join')
    payload = payloads[randint(0, len(payloads) - 1)]
    put_payload(addr, payload)


def main():
    """
    usage: stress_test.py <count>
    """
    if len(argv) < 2:
        print(main.__doc__)
        return

    count = int(argv[1])

    for i in range(0, count):
        put_random_payload()


main()
