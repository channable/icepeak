<h1 align="center">
  <a href="https://github.com/channable/icepeak"><img src="docs/icepeak.png" alt="Icepeak" width="256"></a>
</h1>

Icepeak is a fast json document store with push notification support.

## Disclaimer

This is alpha-quality software developed under Hackathon conditions.
Do not use it.

## Connecting a client

The websocket connection can be interactively tested with ipython.
First install the `websocket-client` library:

```bash
pip install websocket-client
ipython
```

Then create a websocket and send some data:

```python
import websocket
conn = websocket.create_connection("ws://localhost:9160")
conn.send('hello')
result =  conn.recv()
print result
```
