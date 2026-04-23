export {
type TokenRequestError,
type ExtraTokenData,
type Token,

type SubscriptionRef,
type OnUpdate,
type OnSuccess,
type OnFailure,

type FailedSubscribe,
type SubscriptionResult,

createIcepeakCore,
type IcepeakCore,
type IcepeakCoreConfig,
type FetchTokenFn,
type CalculateRetryFn,
type LogFn,
type LogType
}

import * as icepeak_payload from "./icepeak-payload.mjs";

import type * as ws from "ws";

// Named Types

type TokenRequestError<T> = { tokenRequestError: T };
type ExtraTokenData<T> = { extraTokenData: T };
type Token = { token: string };

// SubscriptionRef Public Interface

type SubscriptionRef<FetchTokenError, FetchTokenExtraData> = {
  readonly path : string;
  onUpdate: OnUpdate;
  onSuccess: OnSuccess;
  onFailure: OnFailure<FetchTokenError>
  readonly subscribe: (extra: FetchTokenExtraData) => SubscriptionResult;
  readonly unusubscribe: () => void;
}

type OnUpdate = (updatedValue: any) => void;
type OnSuccess = (success: icepeak_payload.SubscribeSuccess) => void;
type OnFailure<TokenError> = (failure: FailedSubscribe<TokenError>) => void;

type FailedSubscribe<TokenError> =
  | TokenRequestError<TokenError>
  | icepeak_payload.SubscribeError;

type SubscriptionResult =
  | "SendingRequest"
  | "AlreadySubscribed"
  | "SubscriptionAlreadyInProgress";

// Subscription State

type IcepeakCoreState<FetchTokenError, FetchTokenExtraData> = {
  pathSubscriptions: PathSubscriptions<FetchTokenError, FetchTokenExtraData>,
  wsConnState: WsConnState,
}

type PathSubscriptions<FetchTokenError, FetchTokenExtraData> = {
  [path: string]: PathSubscriptionState<FetchTokenError, FetchTokenExtraData>;
};

type PathSubscriptionState<FetchTokenError, FetchTokenExtraData> = {
  readonly path: string;
  status: SubscriptionStatus;
  lastUsedExtraTokenData: ExtraTokenData<FetchTokenExtraData>;
  subscribers: Set<SubscriptionRef<FetchTokenError, FetchTokenExtraData>>;
};

type SubscriptionStatus =
  | "Subscribed"
  | "RequestInProgress"
  | "NotSubscribed"
  | "SendingRequestAfterIcepeakInitialised";

type WsConnState =
  | WsConnUninitialised
  | WsConnConnecting
  | WsConnConnected
  | WsConnClosed
  | WsConnRetrying;

// State when Icepeak object is first constructed
// State will go into 'Connecting' when the first subscribe() happens
type WsConnUninitialised = { connState: "Uninitialised"; };
// The socket connection establishment is in flight to the server.
type WsConnConnecting = { connState: "Connecting"; };
// The socket will retry the connection after set timeout expires
type WsConnRetrying = { connState: "Retrying"; };
// After there has been an internal error (ClosedEvent)
// or there was a socket error but not retried (ErrorEvent)
// An icepeak `subscribe` may semi-manually trigger a reconnect.
type WsConnClosed = { connState: "Closed"; connError: any; };
type WsConnConnected = { connState: "Connected"; wsConn: ws.WebSocket; };

// IcepeakCore Config

type IcepeakCoreConfig<FetchTokenError, ExtraTokenData> = {
  websocketUrl: string,
  websocketConstructor: (url: string) => ws.WebSocket,
  fetchTokenFn: FetchTokenFn<FetchTokenError, ExtraTokenData>,
  calculateRetry: CalculateRetryFn,
  logger: LogFn
}

type FetchTokenFn<FetchTokenError, FetchTokenExtraData> = (
  path: string,
  extraTokenData: ExtraTokenData<FetchTokenExtraData>,
) => Promise<Token | TokenRequestError<FetchTokenError>>

// Returns the number of milliseconds to wait before retrying.
type CalculateRetryFn = (event: ws.ErrorEvent | ws.CloseEvent) => null | number

type LogFn = (logType : LogType, logMessage : string, extra? : unknown) => void
type LogType = "Debug" | "InternalError" | "UserError"


// IcepeakCore Public Interface

type IcepeakCore<FetchTokenError, FetchTokenExtraData> = {
  createSubscriptionRef: (path: string) => SubscriptionRef<FetchTokenError, FetchTokenExtraData>
  destroy: () => void
}

function createIcepeakCore<FetchTokenError, FetchTokenExtraData> (
  config: IcepeakCoreConfig<FetchTokenError, FetchTokenExtraData>
) : IcepeakCore<FetchTokenError, FetchTokenExtraData> {

  const icepeakCorePrivate : IcepeakCorePrivate<FetchTokenError, FetchTokenExtraData> = {
    config : config,
    state : { pathSubscriptions: {}, wsConnState: { connState: "Uninitialised" } },

    connectWs : connectWs,
    connectWsOnOpen : connectWsOnOpen,
    connectWsOnError : connectWsOnError,
    onWsMessageEvent : onWsMessageEvent,

    onUpdatePayload : onUpdatePayload,
    onSubscribeResponse : onSubscribeResponse,
    onUnsubscribeResponse : onUnsubscribeResponse,

    onWsErrorOrClose : onWsErrorOrClose,

    subscribe : subscribe,
    unsubscribe : unsubscribe,
    syncSubscribers : syncSubscribers,

    sendSubscribe : sendSubscribe
  }

  const icepeakCore = {
    createSubscriptionRef: (createSubscriptionRef as
      (path: string) => SubscriptionRef<FetchTokenError, FetchTokenExtraData>
    ).bind(icepeakCorePrivate),
    destroy: destroy
      .bind(icepeakCorePrivate)
  };

  return icepeakCore
}

// IcepeakCore Private Interface

type IcepeakCorePrivate<FetchTokenError, FetchTokenExtraData> = {
  config : IcepeakCoreConfig<FetchTokenError, FetchTokenExtraData>
  state : IcepeakCoreState<FetchTokenError, FetchTokenExtraData>

  connectWs : () => Promise<void>
  connectWsOnOpen : (openedWsConn: ws.WebSocket) => void
  connectWsOnError : (
    event: ws.ErrorEvent,
    resolve: (res: void) => void,
    reject: (rej: void) => void
  ) => void

  onWsMessageEvent : (event: ws.MessageEvent) => void
  onUpdatePayload : (update: icepeak_payload.ValueUpdate) => void
  onSubscribeResponse : (subscribe : SubscribeResponse) => void
  onUnsubscribeResponse : (unsubscribe : UnsubscribeResponse) => void

  onWsErrorOrClose : (event: ws.ErrorEvent | ws.CloseEvent) => void

  subscribe : (
    subscriptionRef: SubscriptionRef<FetchTokenError, FetchTokenExtraData>,
    extraData: FetchTokenExtraData
  ) => SubscriptionResult

  unsubscribe : (subscriptionRef: SubscriptionRef<any, any>) => void

  syncSubscribers : (connectedWs: WsConnConnected) => void

  sendSubscribe : (
    connectedWs: WsConnConnected,
    pathSubscription: PathSubscriptionState<FetchTokenError, FetchTokenExtraData>
  ) => void
}

// IcepeakCore Implementation

function destroy(this: IcepeakCorePrivate<any, any>): void {
  this.config.logger("Debug", "Destroying...")
  for (const path of Object.keys(this.state.pathSubscriptions)) {
    this.config.logger("Debug", "Deleting path.", path)
    delete this.state.pathSubscriptions[path];
  }
  switch (this.state.wsConnState.connState) {
    case "Connected":
      this.state.wsConnState.wsConn.close();
      return;
    case "Uninitialised":
    case "Connecting":
    case "Closed":
      return;
  }
}

function createSubscriptionRef<FetchTokenError, FetchTokenExtraData>(
  this: IcepeakCorePrivate<FetchTokenError, FetchTokenExtraData>,
  path: string
): SubscriptionRef<FetchTokenError, FetchTokenExtraData> {
  const subscriptionRef : SubscriptionRef<FetchTokenError, FetchTokenExtraData> = {
    path : path,
    onUpdate: () => {},
    onSuccess: () => {},
    onFailure: () => {},
    subscribe: extraData => this.subscribe(subscriptionRef, extraData),
    unusubscribe: () => this.unsubscribe(subscriptionRef),
  }
  return subscriptionRef
}

function connectWs(this: IcepeakCorePrivate<any, any>): Promise<void> {
  return new Promise((resolve, reject) => {
    this.config.logger("Debug", "Connecting to server...")
    this.state.wsConnState = { connState: "Connecting" }
    const wsConn = this.config.websocketConstructor(this.config.websocketUrl)
    wsConn.onopen = _ => { this.connectWsOnOpen(wsConn); resolve() }
    wsConn.onerror = e => this.connectWsOnError(e, resolve, reject)
  })
}

function connectWsOnOpen(this: IcepeakCorePrivate<any, any>, openedWsConn: ws.WebSocket): void {
  this.config.logger("Debug", "Connected to server.")
  const connectedWs: WsConnConnected = { connState: "Connected", wsConn: openedWsConn }
  this.state.wsConnState = connectedWs
  openedWsConn.onmessage = this.onWsMessageEvent.bind(this)
  openedWsConn.onclose = this.onWsErrorOrClose.bind(this)
  openedWsConn.onerror = this.onWsErrorOrClose.bind(this)
  this.syncSubscribers(connectedWs)
}

function connectWsOnError(
  this : IcepeakCorePrivate<any, any>,
  errorEvent: ws.ErrorEvent,
  resolve: (res: void) => void,
  reject: (rej: void) => void
): void {
  this.config.logger("Debug", "Connection initialisation error.", errorEvent)
  const intervalMs = this.config.calculateRetry(errorEvent)
  if (null == intervalMs) {
    this.config.logger("Debug", "Will not retry, calculateRetry returned null.")
    this.state.wsConnState = { connState: "Closed", connError: errorEvent }
  } else {
    this.config.logger("Debug", "Retrying, calculateRetry returned a number.", intervalMs)
    this.state.wsConnState = { connState: "Retrying" }
    setTimeout(() => this.connectWs().then(resolve).catch(reject), intervalMs)
  }
}

function onWsMessageEvent(this: IcepeakCorePrivate<any, any>, event: ws.MessageEvent): void {
  const mbIncomingPayload = icepeak_payload.parseMessageEvent(event)
  if (mbIncomingPayload.type == "Fail") {
    this.config.logger(
      'InternalError',
      "Unexpected websocket payload from icepeak server.",
      mbIncomingPayload.error)
    return
  }
  const incomingPayload = mbIncomingPayload.value
  this.config.logger("Debug", "Incoming payload.", [incomingPayload, this.state.pathSubscriptions])
  switch (incomingPayload.type) {
  case "update": return this.onUpdatePayload(incomingPayload)
  case "subscribe": return this.onSubscribeResponse(incomingPayload)
  case "unsubscribe": return this.onUnsubscribeResponse(incomingPayload)
  }
}

type SubscribeResponse = icepeak_payload.SubscribeError | icepeak_payload.SubscribeSuccess
type UnsubscribeResponse = icepeak_payload.UnsubscribeError | icepeak_payload.UnsubscribeSuccess

function onUpdatePayload(
  this: IcepeakCorePrivate<any, any>,
  update : icepeak_payload.ValueUpdate
): void {
  if (!(update.path in this.state.pathSubscriptions)) return
  console.log(update.path)
  const subs = this.state.pathSubscriptions[update.path].subscribers;
  for (const sub of subs) sub.onUpdate(update.value);
}

function onSubscribeResponse(
  this: IcepeakCorePrivate<any, any>,
  subscribe : SubscribeResponse
): void {
  switch (subscribe.code) {
  case 200:
    for (const subscribedPath of subscribe.paths) {
      if (subscribedPath.path in this.state.pathSubscriptions) {
        const subscriptionState = this.state.pathSubscriptions[subscribedPath.path]
        subscriptionState.status = "Subscribed";
        for (const subscriber of subscriptionState.subscribers)
          subscriber.onSuccess(subscribe)
      }
    }
    return

  case 400:
  case 401:
  case 403:
    if (!("paths" in subscribe)) {
      this.config.logger('InternalError',
	'Subscribe response indicates malformed payload.', subscribe)
      return
    }

    for (const errorPath in subscribe.paths) {
      if (errorPath in this.state.pathSubscriptions) {
        const subscriptionState = this.state.pathSubscriptions[errorPath];
        subscriptionState.status = "NotSubscribed";
        for (const subscriber of subscriptionState.subscribers)
            subscriber.onFailure(subscribe);
      }
    }
    return
  }
}

function onUnsubscribeResponse(
  this: IcepeakCorePrivate<any, any>,
  unsubscribe : UnsubscribeResponse
): void {
  switch (unsubscribe.code) {
  case 200:
    return;
  case 400:
    this.config.logger('InternalError',
      'Unsubscribe response indicates malfored unsubscribe.', unsubscribe);
    return;
  }
}

function onWsErrorOrClose(
  this: IcepeakCorePrivate<any, any>,
  event: ws.ErrorEvent | ws.CloseEvent
) : void {

  for (const state of Object.values(this.state.pathSubscriptions))
      state.status = "NotSubscribed"

  if ("code" in event && event.code == 1000) {
    this.state.wsConnState = { connState: "Closed", connError: event }
    // 1000 indicates a normal closure, meaning that the purpose for
    // which the connection was established has been fulfilled.
    // https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4
    this.config.logger('Debug', "Clean close.")
    return
  }

  if ("code" in event && event.code > 3000 && event.code < 3006) {
    this.config.logger('InternalError', "Received close code indicating internal client error.")
    return
  }

  switch (this.state.wsConnState.connState) {
  case "Connected":
    this.state.wsConnState = { connState: "Closed", connError: event };
    this.config.logger('Debug', "Connection closed while connected, retrying...", event)
    this.connectWs()
    return
  case "Retrying":
  case "Uninitialised":
  case "Connecting":
  case "Closed":
    this.config.logger('InternalError', "Unexpected close while socket not in connected state.", event)
    return
  }
}


function subscribe<FetchTokenExtraData>(
  this: IcepeakCorePrivate<any, FetchTokenExtraData>,
  subscriptionRef: SubscriptionRef<any, FetchTokenExtraData>,
  extraData: FetchTokenExtraData
) : SubscriptionResult {
  this.config.logger('Debug', "Registering subscriptionRef.", subscriptionRef)

  // Initialise the pathSubscription state if it doesnt exist for the path
  if (!(subscriptionRef.path in this.state.pathSubscriptions)) {
    this.state.pathSubscriptions[subscriptionRef.path] = {
      path: subscriptionRef.path,
      status: "NotSubscribed",
      subscribers : new Set([subscriptionRef]),
      lastUsedExtraTokenData: { extraTokenData : extraData }
    }
  }
  const wsConnState = this.state.wsConnState

  if (wsConnState.connState == "Closed" || wsConnState.connState == "Uninitialised")
      this.connectWs()

  switch (wsConnState.connState) {
  case "Connected":
      switch (this.state.pathSubscriptions[subscriptionRef.path].status) {
      case "SendingRequestAfterIcepeakInitialised":
      case "RequestInProgress":
	this.state
	  .pathSubscriptions[subscriptionRef.path]
	  .subscribers.add(subscriptionRef)
	return "SubscriptionAlreadyInProgress"

      case "Subscribed":
	this.state
	  .pathSubscriptions[subscriptionRef.path]
	  .subscribers.add(subscriptionRef)
	return "AlreadySubscribed"

      case "NotSubscribed":
	this.config.logger('Debug', "NotSubscribed, but websocket connected.")
	const pathState = this.state.pathSubscriptions[subscriptionRef.path]

	pathState.subscribers.add(subscriptionRef)
	pathState.lastUsedExtraTokenData = { extraTokenData: extraData }

	this.sendSubscribe(wsConnState, pathState)
	return "SendingRequest"
      }

  case "Retrying":
  case "Closed":
  case "Connecting":
  case "Uninitialised":
    switch (this.state.pathSubscriptions[subscriptionRef.path].status) {
    case "SendingRequestAfterIcepeakInitialised":
    case "RequestInProgress":
	this.state
	  .pathSubscriptions[subscriptionRef.path]
	  .subscribers.add(subscriptionRef)
	return "SubscriptionAlreadyInProgress"

    case "NotSubscribed":
      const pathState = this.state.pathSubscriptions[subscriptionRef.path]
      pathState.subscribers.add(subscriptionRef)
      pathState.lastUsedExtraTokenData = { extraTokenData: extraData }
      pathState.status = "SendingRequestAfterIcepeakInitialised"
      return "SendingRequest"

    case "Subscribed": // This case should not be possible
      this.config.logger('InternalError', "Path state should not be subscribed while connection is closed.",
	this.state.pathSubscriptions[subscriptionRef.path])
      return "AlreadySubscribed"
    }
  }
}

function unsubscribe(
  this: IcepeakCorePrivate<any, any>,
  subscriptionRef: SubscriptionRef<any, any>
) : void {
  if (!(subscriptionRef.path in this.state.pathSubscriptions)) return

  this.config.logger('Debug', 'Unregistering subscriptionRef.', subscriptionRef)

  this.state
    .pathSubscriptions[subscriptionRef.path]
    .subscribers.delete(subscriptionRef);

  if (this.state
    .pathSubscriptions[subscriptionRef.path]
    .subscribers.size == 0
  ) {
    this.config.logger('Debug',
      'subscriptionRef count reached 0, removing path.',
      this.state.pathSubscriptions[subscriptionRef.path])

    delete this.state.pathSubscriptions[subscriptionRef.path]

    switch (this.state.wsConnState.connState) {
    case "Connected":
      const unsubscribePayload = icepeak_payload.createUnsubscribePayload([subscriptionRef.path])
      this.config.logger('Debug', 'Sending unsubscribe payload.', unsubscribePayload)
      icepeak_payload.sendPayload(this.state.wsConnState.wsConn, unsubscribePayload);
      return
    case "Uninitialised":
    case "Connecting":
    case "Closed":
      return
    }
  }
}

function syncSubscribers(
  this: IcepeakCorePrivate<any, any>,
  connectedWs: WsConnConnected
) : void {
  this.config.logger('Debug', "Syncing subscribers...")

  for (const state of Object.values(this.state.pathSubscriptions)) {
    if ( state.status == "NotSubscribed" ||
      state.status == "SendingRequestAfterIcepeakInitialised"
    ) {
      this.config.logger('Debug', "Syncing for path.", state)
      this.sendSubscribe(connectedWs, state);
    }
  }
}

function sendSubscribe<FetchTokenError, FetchTokenExtraData>(
  this: IcepeakCorePrivate<FetchTokenError, FetchTokenExtraData>,
  connectedWs: WsConnConnected,
  pathSubscription: PathSubscriptionState<FetchTokenError, FetchTokenExtraData>
) : void {
  this.config.logger('Debug', "Sending subscribe payload.", pathSubscription)

  const extraData = pathSubscription.lastUsedExtraTokenData;
  const path = pathSubscription.path;
  pathSubscription.status = "RequestInProgress";

  this.config
    .fetchTokenFn(path, extraData)
    .then(tokenResponse => {

      if ("token" in tokenResponse) {
	const subscribePayload = icepeak_payload
	  .createSubscribePayload([path], tokenResponse.token)
        icepeak_payload
	  .sendPayload(connectedWs.wsConn, subscribePayload)
      }

      if ("tokenRequestError" in tokenResponse) {
	this.config.logger('Debug', "Received a token error.", tokenResponse)
	this.config.logger('UserError', "Received a token error.", tokenResponse)
	pathSubscription.status = "NotSubscribed";
        pathSubscription.subscribers.forEach(
	  subscriptionRef => subscriptionRef.onFailure(tokenResponse));
      }
    });
}
