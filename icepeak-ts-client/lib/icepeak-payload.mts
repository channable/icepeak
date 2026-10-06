export {
  type ValueUpdate,
  type SubscribeError,
  type SubscribeSuccess,
  type UnsubscribeError,
  type UnsubscribeSuccess,
  type SubscribePayload,
  type UnsubscribePayload,
  type IncomingPayload,
  createSubscribePayload,
  createUnsubscribePayload,
  sendPayload,
  parseMessageEvent,
};

import {
  type Maybe,
  just,
  nothing,
  type Either,
  success,
  fail,
  parseArray,
} from "./util.mjs";

import * as ws from "ws";

type SubscribePayload = { type: string; paths: string[]; token: string };

function createSubscribePayload(
  paths: [string],
  token: string,
): SubscribePayload {
  const subscribePayload = {
    type: "subscribe",
    paths: paths,
    token: token,
  };
  return subscribePayload;
}

type UnsubscribePayload = { type: string; paths: string[] };

function createUnsubscribePayload(paths: [string]): UnsubscribePayload {
  const unsubscribePayload = {
    type: "unsubscribe",
    paths: paths,
  };
  return unsubscribePayload;
}

function sendPayload(
  conn: ws.WebSocket,
  payload: UnsubscribePayload | SubscribePayload,
): void {
  conn.send(JSON.stringify(payload));
}

type ValueUpdate = {
  type: "update";
  path: string;
  value: any;
};

type SubscribedPath = { path: string; value: any };

type SubscribeSuccess = {
  type: "subscribe";
  paths: SubscribedPath[];
  code: 200;
  message: string;
};

type SubscribeError = {
  type: "subscribe";
  code: 400 | 401 | 403;
  paths?: string[];
  message: string;
  extra: any;
};

type UnsubscribeSuccess = {
  type: "unsubscribe";
  code: 200;
  paths: string[];
  message: string;
};

type UnsubscribeError = {
  type: "unsubscribe";
  code: 400;
  paths?: string[];
  message: string;
  extra: any;
};

type IncomingPayload =
  | SubscribeSuccess
  | SubscribeError
  | UnsubscribeSuccess
  | UnsubscribeError
  | ValueUpdate;

function parseSubscribedPath(val: unknown): Maybe<SubscribedPath> {
  if (!(typeof val == "object" && val != null)) return nothing();
  if (!("path" in val && typeof val.path == "string" && "value" in val))
    return nothing();
  return just({ path: val.path, value: val.value });
}

function parseString(val: unknown): Maybe<string> {
  if (typeof val == "string") return success(val);
  return nothing();
}

function parseMessageEvent(
  event: ws.MessageEvent,
): Either<unknown, IncomingPayload> {
  const eventData = event.data;
  if (typeof eventData != "string") return fail(eventData);
  const json: unknown = JSON.parse(eventData);
  if (!(typeof json == "object" && json != null)) return fail(eventData);
  if (!("type" in json)) return fail(json);
  switch (json.type) {
    case "update":
      if (!("path" in json && typeof json.path == "string" && "value" in json))
        return fail(json);
      const update: ValueUpdate = {
        type: "update",
        path: json.path,
        value: json.value,
      };
      return success(update);

    case "subscribe":
      if (!("code" in json && typeof json.code == "number")) return fail(json);
      switch (json.code) {
        case 200:
          if (!("message" in json && typeof json.message == "string"))
            return fail(json);
          if (!("paths" in json && Array.isArray(json.paths)))
            return fail(json);

          const subscribePaths = parseArray(json.paths, parseSubscribedPath);
          if (subscribePaths.type == "Fail") return fail(json);
          const happySubscribe: SubscribeSuccess = {
            type: json.type,
            paths: subscribePaths.value,
            code: json.code,
            message: json.message,
          };
          return success(happySubscribe);
        case 400:
        case 401:
        case 403:
          if (!("message" in json && typeof json.message == "string"))
            return fail(json);
          if (!("extra" in json)) return fail(json);
          if ("paths" in json && Array.isArray(json.paths)) {
            const parsedArray = parseArray(json.paths, parseString);
            if (parsedArray.type == "Fail") return fail(json);
            const sadSubscribe: SubscribeError = {
              type: json.type,
              code: json.code,
              paths: parsedArray.value,
              message: json.message,
              extra: json.extra,
            };
            return success(sadSubscribe);
          } else {
            const sadSubscribe: SubscribeError = {
              type: json.type,
              code: json.code,
              message: json.message,
              extra: json.extra,
            };
            return success(sadSubscribe);
          }
      }
      return fail(json);

    case "unsubscribe":
      if (!("code" in json && typeof json.code == "number")) return fail(json);
      switch (json.code) {
        case 200:
          if (!("message" in json && typeof json.message == "string"))
            return fail(json);
          if (!("paths" in json && Array.isArray(json.paths)))
            return fail(json);

          const subscribePaths = parseArray(json.paths, parseString);
          if (subscribePaths.type == "Fail") return fail(json);
          const happyUnsubscribe: UnsubscribeSuccess = {
            type: json.type,
            paths: subscribePaths.value,
            code: json.code,
            message: json.message,
          };
          return success(happyUnsubscribe);
        case 400:
          if (!("message" in json && typeof json.message == "string"))
            return fail(json);
          if (!("extra" in json)) return fail(json);
          if ("paths" in json && Array.isArray(json.paths)) {
            const parsedArray = parseArray(json.paths, parseString);
            if (parsedArray.type == "Fail") return fail(json);
            const sadUnsubscribe: UnsubscribeError = {
              type: json.type,
              code: json.code,
              paths: parsedArray.value,
              message: json.message,
              extra: json.extra,
            };
            return success(sadUnsubscribe);
          } else {
            const sadUnsubscribe: UnsubscribeError = {
              type: json.type,
              code: json.code,
              message: json.message,
              extra: json.extra,
            };
            return success(sadUnsubscribe);
          }
      }
      return fail(json);
  }
  return fail(json);
}
