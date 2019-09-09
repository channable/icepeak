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

- `Put` writes a JSON value to a given path in the JSON structure.

  - If the path descends into a JSON value that is not an object, that value is
    replaced by an object that just contains the newly created path.
  - If a value along the path is already an object, the field specified by the
    path is added to it.

- `Delete` removes a JSON value at a given path.

  - If the path does not exist, nothing happens.
  - If the path refers to a field in an object, that field is removed from the
    object.

Both of these operations have an optional parameter, called `durable`, which if set,
ensures that the action is completed before the response code 202 (accepted) is
send back to the client. This ensures that no stale data is possible to get via
a get call. An example of a call that uses `durable` is `http://localhost:3000/foo/bar?durable`

Example if `durable` is **not** set:
1. The client sends a PUT command to the server with a new value for `foo`.
2. The server responds with a 202 (accepted) response immidiatly, before completing the action.
3. A GET request is send to the server from the same client,
    requesting the value for `foo`, but `foo` is not yet updated on the server.
4. The server responds with an out-of-date copy of the data.
5. The update of `foo` is executed on the server and the result is broadcasted.

Example if durable is set:
1. The client sends a PUT command to the server with a new value for `foo`.
2. The server waits for the new value of `foo` to be saved on the server.
3. The server responds with a 202 (accepted) response.
4. A GET request is send from the same client to the server, requesting the value for `foo`.
5. The server responds with the updated value of `foo`.
