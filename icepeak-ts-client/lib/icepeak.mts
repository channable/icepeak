import * as icepeak_basic from "./icepeak-basic.mjs";
import { connect } from "./util.mjs";
import type * as ws from "ws"

type Token = { token: string };

type SuccesfulSubscribe = string;

type TokenRequestError<T> = { tokenError : T }
type SubscriptionError = { subscriptionError : any }

type FailedSubscribe<TokenError> = TokenRequestError<TokenError> | SubscriptionError

type SubscriptionResult = 'SendingRequest' | 'AlreadySubscribed' | 'SubscriptionAlreadyInProgress';

type OnUpdate = (updatedValue: any) => void
type OnSuccess = (success: SuccesfulSubscribe) => void
type OnFailure<TokenError> = (failure: FailedSubscribe<TokenError>) => void

type IcepeakSubscribe<TokenError, TokenData> =
  (path : string,
   subscription: Subscription<TokenError, TokenData>,
    subrequestData : TokenRequestData<TokenData>)
  => SubscriptionResult

type IcepeakUnsubscribe<TokenError, TokenData> =
  (path : string,
   subscription: Subscription<TokenError, TokenData>)
  => void

class Subscription<TokenError, TokenData> {
  constructor(
    icepeakSubscribe : IcepeakSubscribe<TokenError, TokenData>,
    icepeakUnsubscribe : IcepeakUnsubscribe<TokenError, TokenData>,
    path : string
  ) {
    this._path = path;
    this._subscribe = icepeakSubscribe;
    this._unsubscribe = icepeakUnsubscribe;
    this.onUpdate = () => {}
    this.onSuccess = () => {}
    this.onFailure = () => {}
  }

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
  public subscribe(requestData : TokenRequestData<TokenData>) : SubscriptionResult {
    return this._subscribe(this._path, this, requestData);
  }

  public unsubscribe() : void {
    return this._unsubscribe(this._path, this);
  }
}


type TokenRequestData<T> = { tokenData : T }

type TokenRequest<TokenError, TokenData> =
  (path : String, extraData: TokenRequestData<TokenData>) => Promise<Token | TokenRequestError<TokenError>>;

type SubscriptionStatus = "Subscribed" | "RequestInProgress" | "NotSubscribed" | "SendingRequestAfterIcepeakInitialised"

type PathState<TokenData> = {
  readonly path : string
  status : SubscriptionStatus,
  tokenRequestData : TokenRequestData<TokenData>,
  subscribers: Set<Subscription<any, any>>
}

type Paths<TokenRequestData> = {
  [ path : string ] : PathState<TokenRequestData>
}



type WsConnUninitialised = {
  connState : 'Uninitialised',
}

type WsConnConnecting = {
  connState : 'Connecting'
}

type WsConnFailed = {
  connState : 'Failed',
  connError : any
}

type WsConnConnected = {
  connState : 'Connected',
  wsConn : WebSocket
}

type WsConnState = WsConnUninitialised | WsConnConnecting | WsConnConnected | WsConnFailed

class Icepeak<TokenError, TokenData> {
  constructor(wsUrl: string | URL, requestToken : TokenRequest<TokenError, TokenData>) {
    this._requestToken = requestToken;
    this._paths = {};
    this._wsConnState = { connState : 'Uninitialised' };
    this._wsUrl = wsUrl;
  }

  public initialiseWs() : Promise<void> {
    this._wsConnState = { connState : 'Connecting' }
    return connect(this._wsUrl)
      .then(wsConn => {
	const connectedWs : WsConnConnected = { connState : 'Connected',  wsConn : wsConn }
	this._wsConnState = connectedWs;

	wsConn.addEventListener("message", this._messageHandler)
	wsConn.addEventListener("close", this._closeHandler)
	wsConn.addEventListener("error", this._errorHandler)
	this._syncSubscribers(connectedWs)
      })
      .catch(err => {
	this._wsConnState = { connState : 'Failed',  connError : err }
	throw err;
      });
  }

  public createSubscriber( path : string ) : Subscription<TokenError, TokenData> {
    const subscription = new Subscription(this._subscribe, this._unsubscribe, path);
    return subscription;
  }

  public destroy() : void { }

  private _wsUrl : string | URL;
  private _wsConnState : WsConnState
  private _requestToken : TokenRequest<TokenError, TokenData>;
  private _paths : Paths<TokenData>

  private _messageHandler(event : MessageEvent<unknown>) : void {
    const mbIncomingPayload = icepeak_basic.parseMessageEvent(event)
    if (mbIncomingPayload.type == "Fail") {
      console.error("Unexpected websocket payload from icepeak server: ", mbIncomingPayload.error)
      return
    }
    const incomingPayload = mbIncomingPayload.value;
    switch (incomingPayload.type) {
    case "update":
      incomingPayload
      return;
    case "subscribe":
      switch (incomingPayload.code) {
      case 200:
	incomingPayload
        return
      case 400:
      case 401:
      case 403:
	incomingPayload
        return
      }
    case "unsubscribe":
      switch (incomingPayload.code) {
      case 200:
	incomingPayload
        return
      case 400:
	incomingPayload
        return
      }
    }
  }

  private _closeHandler(event : CloseEvent) : void { }
  private _errorHandler(event : Event) : void { }

  private _sendSubscribe(
    connectedWs : WsConnConnected,
    pathState : PathState<TokenData>,
  ) : void {
    const requestData = pathState.tokenRequestData
    const path = pathState.path
    pathState.status = "RequestInProgress"

    this._requestToken(path, requestData).then(tokenResponse => {
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
    subscription: Subscription<TokenError, TokenData>,
    requestData : TokenRequestData<TokenData>
  ) : SubscriptionResult {
    if (!(path in this._paths)) {
      this._paths[path] = {
	path: path,
	status: "NotSubscribed",
	subscribers : new Set([subscription]),
	tokenRequestData: requestData
      }
    }

    const wsConnState = this._wsConnState;
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
	const pathState = this._paths[path]
	pathState.subscribers.add(subscription)
	pathState.tokenRequestData = requestData
	this._sendSubscribe(wsConnState, pathState)
	return "SendingRequest"
      }

    case "Connecting":
    case "Failed":
    case "Uninitialised":
      switch (this._paths[path].status) {
      case "SendingRequestAfterIcepeakInitialised":
      case "RequestInProgress":
	this._paths[path].subscribers.add(subscription)
	return "SubscriptionAlreadyInProgress"
      case "Subscribed": // This case should not be possible
      case "NotSubscribed":
	const pathState = this._paths[path]
	pathState.subscribers.add(subscription)
	pathState.tokenRequestData = requestData
	pathState.status = "SendingRequestAfterIcepeakInitialised"
	return "SendingRequest"
      }
    }
  }

  // Batch subscribe all existing subscribers.
  private _syncSubscribers(connectedWs : WsConnConnected) : void {
    for (const state of Object.values(this._paths)) {
      if (state.status == "NotSubscribed" ||
	state.status == "SendingRequestAfterIcepeakInitialised") {
	  this._sendSubscribe(connectedWs, state)
	}
    }
  }

  private _unsubscribe(
    path : string,
    subscription: Subscription<TokenError, TokenData>
  ) : void {
    this._paths[path].subscribers.delete(subscription)
    if (this._paths[path].subscribers.size == 0)
      delete this._paths[path]
    // TODO send unsubscribe request, gracefully handle unsubcribing
  }
}
