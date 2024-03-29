<h1 align="center">
  <a href="https://github.com/channable/icepeak"><img src="docs/icepeak.png" alt="Icepeak" width="256"></a>
</h1>

[![Build Status](https://channable.semaphoreci.com/badges/icepeak/branches/master.svg?style=shields&key=a0e7e842-7ce9-4c10-aea6-3c7dce768470)](https://channable.semaphoreci.com/projects/icepeak)

Icepeak is a fast JSON document store with push notification support.

Icepeak has an HTTP API that can be used to both read and write data.
It also has a websocket interface that clients can use to get push updates about
data that changed.

Clients can subscribe to specific paths in the JSON document like e.g. `/users/123/status`
via the websocket-based API and they will get a push update whenever any of the
JSON data below this path changes.

If a client subscribes to the document root `/` they will receive _all_ updates.

Icepeak supports JWT-based authorization. JWT claims can be used
to restrict the prefixes in the JSON document that a client can read or write.
See [JWT Authorization](#jwt-authorization).

See [read_after_write_test.py](server/integration-tests/read_after_write_test.py) for a simple
example that PUTs some data via the HTTP API and then retrieves it again over a websocket
connection.

See [listener.html](server/integration-tests/listener.html) for a Javascript-based example of a
websocket listener.

## Status

Icepeak has been used in production by Channable since October 2017.
It has been performing very well and there are no known issues at this point.
Note, that our use case is for non-critical data in a small database.

While Icepeak has been working flawlessly, we still consider it beta-quality
at this point, since it has not been widely tested by other people and use cases.

We would love to hear from other users!

## History

Icepeak was started during a Channable Hackathon on 23 July 2017.
After 24h we had a first workable version with an in-memory JSON store, handling of
websocket connections and an HTTP API.
Over the next few months the prototype was built out into a production-ready application
with a persistent backend, JWT-based authentication, prometheus-based metrics and
a Haskell client library.

## Building and running Icepeak

Icepeak can be built using [Cabal](https://cabal.readthedocs.io/en/stable/),
which can be installed through [ghcup](https://www.haskell.org/ghcup/). You can
also use the [Nix](https://nixos.org/) shell provided by this repo.

- Run the tests with `cabal test -j icepeak`.
- And run `icepeak` itself with `cabal run -j icepeak`.

Integration tests are in `/server/integration-tests`.
They are stand-alone scripts that can be executed directly, e.g. `./read_after_write_test.py`.

### Running with the new low-latency garbage collector for GHC 8.10

If you compiled icepeak with GHC 8.10 (or newer) then you can use the new low-latency
garbage collector by passing the following runtime flags to icepeak:

`icepeak +RTS -xn`

## Building the Haskell client library

Build with `cabal build -j icepeak-client`.
Run the tests with `cabal test -j icepeak-client`.

Run `cabal haddock` to generate the Haskell API documentation.

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
    "version": { "enum": [1] },
    "whitelist": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["prefix", "modes"],
        "properties": {
          "prefix": { "type": "array", "items": { "type": "string" } },
          "modes": { "type": "array", "items": { "enum": ["read", "write"] } }
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
      { "prefix": ["foo"], "modes": ["read"] },
      { "prefix": ["bar", "1"], "modes": ["read", "write"] }
    ]
  }
}
```

A request with this claim set may only:

- Read paths that have `/foo` as a prefix.
- Update paths that have `/bar/1` as a prefix.
  Any other request will result in a _401 Unauthorized_ response.

Generally, a claim contains a list of permissions which apply to the listed path
itself and all sub-paths. A request is considered valid if there is at least one
entry in the whitelist that allows it.

`GET` requests on the REST API and websocket connections require the `read`
permission for the given path, `PUT` and `DELETE` requests require the `write`
permission.

[jwt]: https://tools.ietf.org/html/rfc7519

## Generating Tokens

JSON Web Tokens with the `icepeak` claim can be generated using the included
`icepeak-token-gen` executable.

```
Usage: icepeak-token-gen [-s|--jwt-secret JWT_SECRET]
                         [-e|--expires EXPIRES_SECONDS] [-p|--path PATH:MODES]

Available options:
  -h,--help                Show this help text
  -s,--jwt-secret JWT_SECRET
                           Secret used for signing the JWT, defaults to the
                           value of the JWT_SECRET environment variable if
                           present. If no secret is passed, JWT tokens are not
                           signed.
  -e,--expires EXPIRES_SECONDS
                           Generate a token that expires in EXPIRES_SECONDS
                           seconds from now.
  -p,--path PATH:MODES     Adds the PATH to the whitelist, allowing the access
                           modes MODES. MODES can be 'r' (read), 'w' (write) or
                           'rw' (read/write). This option may be used more than
                           once.
```

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

## Metrics

Icepeak can provide usage metrics to Prometheus with the `--metrics HOST:PORT` command line option.

- `HOST` denotes the hostname the metrics endpoint is listening on. It can
  either be a specific hostname or IP address, or one of the special values
  supported by [https://hackage.haskell.org/package/warp-3.2.13/docs/Network-Wai-Handler-Warp.html#t:HostPreference](Network.Wai.Handler.Warp.HostPreference).

- `PORT` denotes the port number the metrics endpoint is listening on.

## Tests

There are both unit and integration tests.

The unit tests for the server can be found in `server/tests`.
They can be run with:

```
cabal test -j all
```

The integration tests can be found in `server/integration-tests`.
Currently, there are integration tests written in Bash, Python and Haskell.

Each file is an executable script that can be run directly (even the Haskell ones).
For example:

```
cd server/integration-tests
./generate_test_token.hs
```

The tests in Python require the `json`, `requests`, and `websocket` packages to be
installed. They can be installed in a virtual environment with `pip install <name>`.
