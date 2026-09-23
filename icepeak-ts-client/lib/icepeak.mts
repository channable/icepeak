export {
type Icepeak,
type IcepeakConfig,
type Token,
type CalculateRetryFn,
createIcepeak,

type Subscription
};

import {
  type CalculateRetryFn,
  type Token,
  type LogFn,
  type LogType,

  createIcepeakCore,
  type IcepeakCore,
  type IcepeakCoreConfig,
  type ExtraTokenData,
} from "./icepeak-core.mjs";

import type * as ws from "ws";

type IcepeakConfig = {
  websocketUrl: string,
  websocketConstructor: (url: string) => ws.WebSocket,
  fetchToken: (path: string) => Promise<Token>,
  calculateRetry?: CalculateRetryFn,
  logger?: LogFn,
}

type Icepeak = {
  readonly subscribe: (
    path: string,
    onUpdate: (newValue : unknown) => void
  ) => Subscription
}

type Subscription = {
  readonly unsubscribe : () => void
}

// Constructing Icepeak

function createIcepeak(config: IcepeakConfig): Icepeak {
  const icepeakCoreConfig : IcepeakCoreConfig<null, null> = {
    websocketUrl: config.websocketUrl,
    fetchTokenFn: (path: string) => config.fetchToken(path),
    calculateRetry: config.calculateRetry ?? (_ => null),
    websocketConstructor: config.websocketConstructor,
    logger: config.logger ?? defaultLogger
  }

  const icepeakCore = createIcepeakCore(icepeakCoreConfig)

  const icepeak : Icepeak = {
    subscribe: (
      path: string,
      onUpdate: (newValue : unknown) => void
    ) => subscribe(icepeakCore, path, onUpdate)
  };

  return icepeak
}

function defaultLogger(
  logType: LogType,
  logMessage : string,
  logExtra? : unknown
) : void {
  if (logType == 'UserError') {
    logExtra
      ? console.error(logMessage, logExtra)
      : console.error(logMessage)
  }
}

function subscribe(
  icepeakCore: IcepeakCore<null, null>,
  path: string,
  onUpdate: (newValue : unknown) => void
): Subscription {
  const subscriptionRef = icepeakCore.createSubscriptionRef(path)
  subscriptionRef.onUpdate = onUpdate
  subscriptionRef.subscribe(null)
  const subscription : Subscription = { unsubscribe: subscriptionRef.unusubscribe }
  return subscription
}
