export { Icepeak, type TokenRequest, type Token }

import * as icepeak_basic from "./icepeak-basic.mjs";
import { connect, timeoutPromise } from "./util.mjs";
import type * as ws from "ws"

type Token = { token: string };

type TokenRequestError<T> = { tokenError : T }

type FailedSubscribe<TokenError>
  = TokenRequestError<TokenError>
  | icepeak_basic.SubscribeError

type SubscriptionResult
  = 'SendingRequest'
  | 'AlreadySubscribed'
  | 'SubscriptionAlreadyInProgress'

type OnUpdate = (updatedValue: any) => void
type OnSuccess = (success: icepeak_basic.SubscribeSuccess) => void
type OnFailure<TokenError> = (failure: FailedSubscribe<TokenError>) => void

type IcepeakSubscribe<TokenError, TokenData> =
  (path : string,
   subscription: SubscriptionRef<TokenError, TokenData>,
    subrequestData : TokenRequestData<TokenData>)
  => SubscriptionResult

type IcepeakUnsubscribe<TokenError, TokenData> =
  (path : string,
   subscription: SubscriptionRef<TokenError, TokenData>)
  => void


class Subscription<TokenError, TokenData> {
  constructor( subscriptionRef : SubscriptionRef<TokenError, TokenData>) {
    this.unsubscribe = subscriptionRef.unsubscribe.bind(subscriptionRef)
  }
  public unsubscribe : () => void;
}

class SubscriptionRef<TokenError, TokenData> {
  constructor(
    icepeak : InstanceType<typeof Icepeak<TokenError, TokenData>>,
    icepeakSubscribe : IcepeakSubscribe<TokenError, TokenData>,
    icepeakUnsubscribe : IcepeakUnsubscribe<TokenError, TokenData>,
    path : string
  ) {
    this._icepeak = icepeak;
    this._path = path;
    this._subscribe = icepeakSubscribe;
    this._unsubscribe = icepeakUnsubscribe;
    this.onUpdate = () => {};
    this.onSuccess = () => {};
    this.onFailure = () => {};
  }

  private _icepeak : InstanceType<typeof Icepeak<TokenError, TokenData>>;

  private _path : string;
  // These functions are provided by the parent Icepeak object which constructs this.
  private _subscribe : IcepeakSubscribe<TokenError, TokenData>;
  private _unsubscribe : IcepeakUnsubscribe<TokenError, TokenData>;

  // These get called whenever there is a global notification on that path.
  // So the callback must assume that there are other callbacks that also see
  // this notificaiton.
  public onUpdate : OnUpdate;
  public onSuccess : OnSuccess;
  public onFailure : OnFailure<TokenError>;

  // If subscribe is called by 2 different instances of Subscription at one
  // time, then the icepeak client will take the first subscribe request and
  // destory the other.

  public subscribe(extraTokenData : TokenRequestData<TokenData>) : SubscriptionResult {
    return this._subscribe.bind(this._icepeak)(this._path, this, extraTokenData);
  }

  public unsubscribe() : void {
    return this._unsubscribe.bind(this._icepeak)(this._path, this);
  }
}

type TokenRequestData<T> = { tokenData : T }

type TokenRequest<TokenError, TokenData>
  = ( path : String, extraData: TokenRequestData<TokenData> )
  => Promise<Token | TokenRequestError<TokenError>>;

type SubscriptionStatus
  = "Subscribed"
  | "RequestInProgress"
  | "NotSubscribed"
  | "SendingRequestAfterIcepeakInitialised"

type PathState<TokenData> = {
  readonly path : string
  status : SubscriptionStatus,
  extraTokenData : TokenRequestData<TokenData>,
  subscribers: Set<SubscriptionRef<any, any>>
}

type Paths<TokenRequestData> = {
  [ path : string ] : PathState<TokenRequestData>
}

// State when Icepeak object is first constructed
// State will go into 'Connecting' when the first subscribe() happens
type WsConnUninitialised = {
  connState : 'Uninitialised',
}

// The socket connection establishment is in flight to the server.
type WsConnConnecting = {
  connState : 'Connecting'
}

// The socket will retry the connection after set timeout expires
type WsConnRetrying = {
  connState : 'Retrying'
}

// After there has been an internal error (ClosedEvent)
// or there was a socket error but not retried (ErrorEvent)
// An icepeak `subscribe` may semi-manually trigger a reconnect.
type WsConnClosed = {
  connState : 'Closed',
  connError : any
}

type WsConnConnected = {
  connState : 'Connected',
  wsConn : ws.WebSocket
}

type WsConnState
  = WsConnUninitialised
  | WsConnConnecting
  | WsConnConnected
  | WsConnClosed
  | WsConnRetrying

class Icepeak<TokenError, TokenData> {
  constructor(
    websocketConstructor : (url : string) => ws.WebSocket,
    wsUrl: string,
    requestToken : TokenRequest<TokenError, TokenData>,
    reconnectIntervalMilis : (event : ws.ErrorEvent | ws.CloseEvent) => null | number
  ) {
    this._requestToken = requestToken;
    this._paths = {};
    this._wsConnState = { connState : 'Uninitialised' };
    this._wsUrl = wsUrl;
    this._websocketConstructor = websocketConstructor;
    this._reconnectIntervalMilis = reconnectIntervalMilis
  }

  private _websocketConstructor : (url : string) => ws.WebSocket;
  private _reconnectIntervalMilis : (event : ws.ErrorEvent | ws.CloseEvent) => null | number

  // connect will retry
  private connect() : Promise<void> {
    return new Promise(
      (res, rej) => {
	console.log("ICEPEAK, CONNECTING TO SERVER")
	this._wsConnState = { connState : 'Connecting' }
	const wsConn = this._websocketConstructor(this._wsUrl);

	wsConn.onopen = () => {
	  console.log("ICEPEAK, CONNECTED TO SERVER")
	  const connectedWs : WsConnConnected = { connState : 'Connected',  wsConn : wsConn }
	  this._wsConnState = connectedWs;

	  wsConn.onmessage = this._messageHandler.bind(this)
	  wsConn.onclose = (e) => {
	    console.log("as a close")
	    this._errorOrCloseHandler.bind(this)(e)
	  }
	  wsConn.onerror = (e) => {
	    console.log("as an error")
	    this._errorOrCloseHandler.bind(this)(e)
	  }
	  this._syncSubscribers(connectedWs)
	  res()
	}

	wsConn.onerror = (errorEvent) => {
	  console.log("ICEPEAK, CONNECTION ESTABLISHMENT ERROR:", errorEvent)
	  const intervalMs = this._reconnectIntervalMilis(errorEvent)
	  if (null == intervalMs) {
	    this._wsConnState = { connState : 'Closed',  connError : errorEvent }
	    console.log("ICEPEAK, WILL NOT RETRY CONNECTION")
	    rej(errorEvent)
	  }
          else {
	    this._wsConnState = { connState : 'Retrying' }
	    console.log("ICEPEAK, RETRYING AFTER:", intervalMs)
	    setTimeout(
	      () => this.connect().then(res).catch(rej),
	      intervalMs)
	  }
	}
      })
  }

  public subscribe(config: {
    path: string;
    onUpdate: OnUpdate;
    extraTokenData: TokenData
    onSuccess?: OnSuccess;
    onFailure?: OnFailure<TokenError>;
  }) : Subscription<TokenError, TokenData>
  {
    const s = this.createSubscriptionRef(config.path);
    s.onUpdate = config.onUpdate
    if (config?.onFailure) s.onFailure = config.onFailure
    if (config?.onSuccess) s.onSuccess = config.onSuccess
    s.subscribe({ tokenData : config.extraTokenData })
    return new Subscription(s)
  }

  public createSubscriptionRef( path : string ) : SubscriptionRef<TokenError, TokenData> {
    const subscriptionRef = new SubscriptionRef(
      this,
      this._subscribe,
      this._unsubscribe,
      path);
    return subscriptionRef;
  }

  public destroy() : void {
    console.log("ICEPEAK, DESTROYING")
    for (const path of Object.keys(this._paths)) {
      console.log("ICEPEAK, DELETING PATH", path)
      delete this._paths[path]
    }
    switch (this._wsConnState.connState) {
      case "Connected":
	this._wsConnState.wsConn.close()
	return
      case "Uninitialised":
      case "Connecting":
      case "Closed":
        return
    }
  }

  private _wsUrl : string;
  private _wsConnState : WsConnState
  private _requestToken : TokenRequest<TokenError, TokenData>;
  private _paths : Paths<TokenData>


  private _messageHandler(event : ws.MessageEvent) : void {
    const mbIncomingPayload = icepeak_basic.parseMessageEvent(event)
    if (mbIncomingPayload.type == "Fail") {
      console.error(
	"Unexpected websocket payload from icepeak server: ",
	mbIncomingPayload.error)
      return
    }
    const incomingPayload = mbIncomingPayload.value;
    console.log("ICEPEAK, INCOMING PAYLOAD:", incomingPayload)
    switch (incomingPayload.type) {
    case "update":
      if (!(incomingPayload.path in this._paths)) {
	return
      }
      const subs = this._paths[incomingPayload.path].subscribers;
      for (const sub of subs) sub.onUpdate(incomingPayload.value)
      return
    case "subscribe":
      switch (incomingPayload.code) {
      case 200:
	for (const subscribedPath of incomingPayload.paths){
	  if (subscribedPath.path in this._paths) {
	    const subscriptionState = this._paths[subscribedPath.path]
	    subscriptionState.status = "Subscribed"
	    for (const subscriber of subscriptionState.subscribers)
	      subscriber.onSuccess(incomingPayload);
	  }
	}
        return
      case 400:
        console.error(incomingPayload)
        return
      case 401:
      case 403:
	if ("paths" in incomingPayload) {
          for (const errorPath in incomingPayload.paths) {
	    if (errorPath in this._paths) {
	      const subscriptionState = this._paths[errorPath]
	      subscriptionState.status = "NotSubscribed"
	      for (const subscriber of subscriptionState.subscribers)
	        subscriber.onFailure(incomingPayload);
	    }
	  }
	}
	else console.error(incomingPayload)
        return
      }
    case "unsubscribe":
      switch (incomingPayload.code) {
      case 200:
        return
      case 400:
	console.error(incomingPayload)
        return
      }
    }
  }

  private _errorOrCloseHandler(event : ws.ErrorEvent | ws.CloseEvent) : void {
    for (const state of Object.values(this._paths)) state.status = "NotSubscribed"
    if ("code" in event && event.code == 1000) {
      this._wsConnState = { connState : 'Closed', connError : event };
      // 1000 indicates a normal closure, meaning that the purpose for
      // which the connection was established has been fulfilled.
      // https://www.rfc-editor.org/rfc/rfc6455.html#section-7.4
      console.log("ICEPEAK, CLEAN CLOSE")
      return
    }

    if ("code" in event && event.code > 3000 && event.code < 3006) {
      console.error("ICEPEAK, RECEIVED CLOSE CODE INDICATING INTERNAL CLIENT ERROR", event)
      return
    }

    switch (this._wsConnState.connState) {
      case "Connected":
	this._wsConnState = { connState : 'Closed', connError : event };
        console.log("ICEPEAK, CONNECTION CLOSED WHILE CONNECTED, RETRYING:", event)
	this.connect()
	  .catch(err => {
	    console.error(err)
	    throw err
	  })
        return
      case "Retrying":
      case "Uninitialised":
      case "Connecting":
      case "Closed":
	console.error("ICEPEAK, UNEXPECTED CLOSE WHILE SOCKET NOT IN CONNECTED STATE", event)
        return
    }
  }

  private _sendSubscribe(
    connectedWs : WsConnConnected,
    pathState : PathState<TokenData>,
  ) : void {
    const extraTokenData = pathState.extraTokenData
    const path = pathState.path
    pathState.status = "RequestInProgress"
    console.log("ICEPEAK, SENDING SUBSCRIBE:", path)

    this._requestToken(path, extraTokenData).then(tokenResponse => {
      if ("tokenError" in tokenResponse) {
	pathState.status = "NotSubscribed"
	pathState.subscribers.forEach(subscriber => {
	  subscriber.onFailure(tokenResponse)
	});
      }
      if ("token" in tokenResponse) {
	icepeak_basic.sendPayload(
	  connectedWs.wsConn,
	  icepeak_basic.createSubscribePayload([path], tokenResponse.token))
      }
    })
  }

  private _subscribe(
    path : string,
    subscription: SubscriptionRef<TokenError, TokenData>,
    extraTokenData : TokenRequestData<TokenData>
  ) : SubscriptionResult {
    console.log("ICEPEAK, INTERNALLY SUBSCRIBING:", path)
    if (!(path in this._paths)) {
      this._paths[path] = {
	path: path,
	status: "NotSubscribed",
	subscribers : new Set([subscription]),
	extraTokenData: extraTokenData
      }
    }

    const wsConnState = this._wsConnState

    if ( wsConnState.connState == "Closed" || wsConnState.connState == "Uninitialised")
      this.connect()

    switch (wsConnState.connState) {

    case "Connected":
      switch (this._paths[path].status) {
      case "SendingRequestAfterIcepeakInitialised":
      case "RequestInProgress":
	this._paths[path].subscribers.add(subscription)
	return "SubscriptionAlreadyInProgress"

      case "Subscribed":
	this._paths[path].subscribers.add(subscription)
	return "AlreadySubscribed"

      case "NotSubscribed":
	console.log("ICEPEAK, NotSubscribed, BUT WEBSOCKET Connected:", path)
	const pathState = this._paths[path]
	pathState.subscribers.add(subscription)
	pathState.extraTokenData = extraTokenData
	this._sendSubscribe(wsConnState, pathState)
	return "SendingRequest"
      }

    case "Retrying":
    case "Closed":
    case "Connecting":
    case "Uninitialised":
      switch (this._paths[path].status) {
      case "SendingRequestAfterIcepeakInitialised":
      case "RequestInProgress":
	this._paths[path].subscribers.add(subscription)
	return "SubscriptionAlreadyInProgress"

      case "NotSubscribed":
	const pathState = this._paths[path]
	pathState.subscribers.add(subscription)
	pathState.extraTokenData = extraTokenData
	pathState.status = "SendingRequestAfterIcepeakInitialised"
	return "SendingRequest"

      case "Subscribed": // This case should not be possible
        console.error("Internal icepeak path state should not be subscribed while connection is closed.")
        return "AlreadySubscribed"
      }
    }
  }

  // Batch subscribe all existing subscribers.
  private _syncSubscribers(connectedWs : WsConnConnected) : void {
    console.log("ICEPEAK, SYNCING SUBSCRIBERS:")
    for (const state of Object.values(this._paths)) {
      if (state.status == "NotSubscribed" ||
	state.status == "SendingRequestAfterIcepeakInitialised") {
	  console.log(state)
	  this._sendSubscribe(connectedWs, state)
	}
    }
  }

  private _unsubscribe(
    path : string,
    subscription: SubscriptionRef<TokenError, TokenData>
  ) : void {
    if (!(path in this._paths)) return
    console.log("ICEPEAK, INTERNALLY UNSUBSCRIBING:", path)
    this._paths[path].subscribers.delete(subscription)
    if (this._paths[path].subscribers.size == 0) {
      delete this._paths[path]
      switch (this._wsConnState.connState) {
      case "Connected":
	console.log("ICEPEAK, SENDING UNSUBSCRIBE:", path)
      	icepeak_basic.sendPayload(
	  this._wsConnState.wsConn,
	  icepeak_basic.createUnsubscribePayload([path]))
	return
      case "Uninitialised":
      case "Connecting":
      case "Closed":
	// These cases should have the path un-subscribed not subscribed server
	console.log("ICEPEAK, UNSUBSCRIBE ON UN-INIT CONNECTION")
        return
      }
    }

  }
}
