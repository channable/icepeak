# The Icepeak Multiple-Subcription-Single-Connection Protocol

This protocol was developed atop the existing protocol to allow the client to subscribe to multiple "paths"/"topics" using a single websocket connection.

Whereas the original existing protocol required a new websocket connection for each path the client wanted to subscribe to.

# Initial Connection

A client can connect to a multiple-subscription protocol connection with a URL such as:
`ws://yourdomain.com/?method=reusable`

The URL path:
- points to the root of your icepeak service
- has the query parameter `method` set to `reusable`

The authorisation method follows the same protocol as the previous one.

# Subscribing, Unsubscribing & Updates

In summary:
- The client can request to subscribe to an array of paths.
- The client, on the single connection, can cumulatively keep subscribing and unsubscribing to paths by sending corresponding payloads.
  - The server also sends back a response about the status, and some data of the corresponding subscription or unsubscription request.
- The server will send the client the new value/update at the subscribed path whenever there is a change at that path.
- Both, subscription and unsubscription requests are idempotent.

## Update

`JSON Schema` declaration of the update the server will send to the client upon subscribed path change:
```javascript
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "wss://updates.channable.com",
  "title": "Path changes",
  "description": "Indicates changes occurred at the subscribed paths",
  "type": "object",
  "properties": {
    "type": { "const": "update" },
    "change": {
      "type": "object",
      "properties": {
        "path": { "type": "string" },
        "value": {}
      }
    }
  }
}
```

## Subscribe

In summary:
- The client can send a payload that contains an array of paths to subscribe to.
  - The client has to send a JWT that authorises all the paths.
- The client can expect a response from the server that contains the status/acknowledgement of the request.
  - Upon a successful request, the client can expect the payload to also contain the current value of the paths requested.

### Client Subscribe Request
Each subscription is checked against a JWT to see if the user is authorised to access the path(s).

`JSON Schema` declaration of the subscribe client request:
```javascript
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "wss://updates.channable.com/",
  "title": "Add subscription",
  "description": "Adding an additional subscription",
  "type": "object",
  "properties": {
    "type": { "const": "subscribe" },
    "paths": { "type": "array", "items": { "type": "string" } },
    "token": { "type": "string" }
  }
}
```

### Server Subscribe Response
The server sends back a payload to the client. The payload will always contain a status code:

| Status code | When                                  |
| ---- | -------------------------------------------- |
| 200  | Subscription was successfully processed      |
| 400  | Request payload was malformed                |
| 401  | No authorization token provided              |
| 403  | Authorization token was rejected / malformed |


If the status code is `200` and **whether or not the client is already subscribed**, the client can expect a payload from server that contains:
- The status code.
- Path(s) of the subscription requested.
- The current value(s) of that path(s).

`JSON Schema` declaration of the server response:
```javascript
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "wss://updates.channable.com",
  "title": "Subscription status",
  "description": "Indicates whether the subscription was successful",
  "type": "object",
  "properties": {
    "type": { "const": "subscribe" },
    "paths": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "path": { "type": "string" },
          "value": {}
        }
      }
    },
    "code": { "type": "number" },
    "message": { "type": "string" },
    "extra": {}
  }
}
```

## Unsubscribe

In summary:
- The client can send a payload that contains paths to unsubscribe from.
- The client can expect a response from the server that contains the status/acknowledgement of the request.
  - In the case of a successful request, the response also contains the list of paths in the client request.

### Client Unsubscribe Request

`JSON Schema` declaration of the unsubscribe client request:
```javascript
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "wss://updates.channable.com",
  "title": "Remove Subscription",
  "description": "Remove an existing subscription",
  "type": "object",
  "properties": {
    "type": { "const": "unsubscribe" },
    "paths": { "type": "array", "items": { "type": "string" } }
  }
}
```

### Server Unsubscribe Response
The server sends back a payload to the client. The payload will always contain a status code:

| Status code   | When                             |
| ------------- | -------------------------------- |
| 200  | Unsubscription was successfully processed |
| 400  | Request payload was malformed             |

If the status code is `200` and **whether or not the client is already unsubscribed**, the client can expect a payload from server that contains:
- the list of the unsubscribe paths that the client had sent in the request.

`JSON Schema` declaration of the unsubscribe client request:
```javascript
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "wss://updates.channable.com",
  "title": "Unsubscription status",
  "description": "Indicates whether the unsubscription was successful",
  "type": "object",
  "properties": {
    "type": { "const": "unsubscribe" },
    "paths": { "type": "array", "items": { "type": "string" } },
    "code": { "type": "number" },
    "message": { "type": "string" },
    "extra": {}
  }
}
```



# Invalid Client Message
The server will close the websocket connection if the client payload contains an unrecognised `type`.
