## The Icepeak Protocol

The Icepeak protocol is very simple:

The client opens a new websocket connection to the server and will only ever receive data, never send data.
The client will subscribe to a specific channel by specifying it in the first HTTP request that
establishes the connection to the server.

An example URL is `ws://yourdomain.com/path/to/channel`.

### Receiving data

There are 2 ways to receive data from an Icepeak server. The first is with a direct GET command over
the HTTP protocol. An example of such a call would be `GET http://localhost:3000/path/to/channel`. 
This would result in the information that is stored under the `path/to/channel` object, if any. 
The result would be a JSON object with all keys. 

The other way to receive data is with a GET call that is upgraded to a WebSocket.
As long as the connection remains open, Icepeak will send updates whenever a value changes on
the server. For example, the connection `ws://localhost:3000/path/to/channel`. 
`/path/to/channel` is then the identifier of a JSON collection.
If any value of that collection changes, the whole collection under this path will be sent to
the client.

Example:

1. The client establishes the connection: `GET ws://yourdomain.com/path/to/channel`
2. The server adds client to ServerState under the `/path/to/channel` path
3. The server immediately sends the current value of `/path/to/channel`, e.g. `{"status": "updating"}`
4. The value gets updated to `{"status": "done"}` (by another process)
5. The server then immediately sends the updated value to all clients listening to `/path/to/channel`

### Modifying data

Data on Icepeak can be modified by either a PUT request or a DELETE request. The PUT will introduce
or overwrite new information to the Icepeak server and DELETE will remove information from the server.
In case some information changes, all information that is changed will be send to every 
client that is listening to that channel. 
