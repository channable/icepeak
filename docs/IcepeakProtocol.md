## The Icepeak Protocol

The Icepeak protocol is very simple:

The client opens a new websocket connection to the server and will only ever receive data, never send data.
The client will subscribe to a specific channel by specifying it in the first HTTP request that
establishes the connection to the server.

An example URL is `ws://yourdomain.com/path/to/channel`.

`/path/to/channel` is then the identifier of a JSON collection.
If any value of that collection changes, the whole collection under this path will be sent to
the client.

Example:

1. The client establishes the connection: `GET ws://yourdomain.com/path/to/channel`
2. The server adds client to ServerState under the `/path/to/channel` path
3. The server immediately sends the current value of `/path/to/channel`, e.g. `{"status": "updating"}`
4. The value gets updated to `{"status": "done"}` (by another process)
5. The server then immediately sends the updated value to all clients listening to `/path/to/channel`
