<h1 align="center">
  <a href="https://github.com/channable/icepeak"><img src="docs/icepeak.png" alt="Icepeak" width="256"></a>
</h1>

<p align="center">
  <a href="https://travis-ci.org/channable/icepeak"><img src="https://travis-ci.org/channable/icepeak.svg?branch=master" alt="Build Status"></a>
  <img src="https://img.shields.io/badge/license-BSD3-blue.svg" alt="BSD3 Licensed">
</p>

Icepeak is a fast JSON document store with push notification support.

## Disclaimer

This is alpha-quality software developed under Hackathon conditions.
Do not use it.

## Building and running Icepeak

Cd into `/server`.
Build with `stack build`.
Run the tests with `stack test`.
And run `icepeak` itself with `stack exec icepeak`.
Install with `stack install`.

Integration tests are in `/server/integration-tests`.
They are stand-alone scripts that can be executed directly, e.g. `./connection_test.py`.

## Building the Haskell client library

Cd into `/client-haskell`.
Build with `stack build`.
Run the tests with `stack test`.

Run `stack haddock --no-haddock-deps` to generate the Haskell API documentation.

## Usage:

```
Usage: icepeak [--data-file DATA_FILE] [--enable-jwt-auth]
               [--jwt-secret JWT_SECRET]

Available options:
  -h,--help                Show this help text
  --data-file DATA_FILE    File where data is persisted to. Default:
                           icepeak.json
  --enable-jwt-auth        Enable authorization using JSON Web Tokens.
  --jwt-secret JWT_SECRET  Secret used for JWT verification, defaults to the
                           value of the JWT_SECRET environment variable if
                           present. If no secret is passed, JWT tokens are not
                           checked for validity.
```

## JWT Authorization

Optionally, requests can be required to contain a [JSON Web Token][jwt] with an
`icepeak` claim describing the set of permissions of that client. JWT
authorization can be enabled with the `--enable-jwt-auth` command line option. A
token can be specified in an `Authorization: Bearer <token>` header or an
`access_token=<token>` query string parameter, the former taking precedence over the
latter.

Additionally, a secret is may be passed to the application via `--jwt-secret` or
the `JWT_SECRET` environment variable, the former taking precedence over the
latter. This secret is used to verify the HS256 signature of incoming tokens. A
supplied token is only considered valid if it

- has a valid signature,
- has not expired and
- has been issued before it is used.

If no secret is supplied, icepeak assumes that the tokens it receives have been
checked by a proxy that did all the verification. In that case, only the
`icepeak` claim is extracted and parsed, but no verification takes place.

The `icepeak` claim has the following JSON schema:

```json
{
  "type": "object",
  "required": ["version"],
  "properties": {
    "version": {"enum": [1]},
    "whitelist": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["prefix", "modes"],
        "properties": {
          "prefix": {"type": "array", "items": {"type": "string"}},
          "modes": {"type": "array", "items": {"enum": ["read", "write"]}}
        }
      }
    }
  }
}
```

Example JWT token claim set:

```json
{
  "icepeak": {
    "version": 1,
    "whitelist": [
      {"prefix": ["foo"], "modes": ["read"]},
      {"prefix": ["bar", "1"], "modes": ["read", "write"]}
    ]
  }
}
```

A request with this claim set may only:
 - Read paths that have `/foo` as a prefix.
 - Update paths that have `/bar/1` as a prefix.
Any other request will result in a *401 Unauthorized* response.

Generally, a claim contains a list of permissions which apply to the listed path
itself and all sub-paths. A request is considered valid if there is at least one
entry in the whitelist that allows it.

`GET` requests on the REST API and websocket connections require the `read`
permission for the given path, `PUT` and `DELETE` requests require the `write`
permission.

[jwt]: https://tools.ietf.org/html/rfc7519

## Connecting a client

The websocket connection can be interactively tested with ipython.
First install the `websocket-client` library:

```bash
pip install websocket-client
ipython
```

Then create a websocket and receive the the current value at `foo`, and updates
when it is modified:

```python
import websocket
conn = websocket.create_connection("ws://localhost:3000/foo")
while True:
    result = conn.recv()
    print result
```
