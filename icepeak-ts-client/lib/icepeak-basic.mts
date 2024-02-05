export { type SubscribePayload, type UnsubscribePayload, createSubscribePayload, createUnsubscribePayload, sendPayload }

import type * as ws from "ws"

type SubscribePayload =
{ type: string;
  paths: [string];
  token: string;
}

function createSubscribePayload(paths: [string], token: string): SubscribePayload {
  const subscribePayload = {
    type: "subscribe",
    paths: paths,
    token: token
  };
  return subscribePayload;
}


type UnsubscribePayload =
{ type: string ;
  paths: [string];
}

function createUnsubscribePayload(paths: [string]): UnsubscribePayload {
  const unsubscribePayload = {
    type: "unsubscribe",
    paths : paths,
  };
  return unsubscribePayload;
}



function sendPayload(
  conn: WebSocket,
  payload: UnsubscribePayload | SubscribePayload
): void {
    conn.send(JSON.stringify(payload));
}
