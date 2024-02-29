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
  type IcepeakCoreConfig
} from "./icepeak-core.mjs";

import type * as ws from "ws";

type IcepeakConfig = {
  websocketUrl: string,
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

function subscribe(icepeakCore: IcepeakCore<null, null>): Subscription {
  return null as any as Subscription
}

function createIcepeak(config: IcepeakConfig): Icepeak {
  const icepeakCoreConfig = null as any as IcepeakCoreConfig<null, null>
  const icepeakCore = createIcepeakCore(icepeakCoreConfig)

  const icepeak : Icepeak = { subscribe: () => subscribe(icepeakCore) };
  return icepeak
}


// class Icepeak {
//   constructor(config: IcepeakConfig) {
//     const icepeakCoreConfig : IcepeakCoreConfig =
//       { websocketUrl = config.websocketUrl,
// 	fetchToken = (path , _) => config.fetchToken(path),

// 	calculateRetry =
// 	  config.calculateRetry
// 	    ? config.calculateRetry
// 	    : (_) => null,

// 	websocketConstructor =
// 	  config.websocketConstructor
// 	    ? config.websocketConstructor
// 	    : (s) => new WebSocket(s)

// 	logger =
// 	  config.logger
// 	    ? config.logger
// 	    : (_, _) => null,
//       }
//     this.icepeakCore = new IcepeakCore(icepeakCoreConfig);
//   }

//   private icepeakCore : IcepeakCore

//   public subscribe(
//     path: string,
//     onUpdate: (newValue: any) => void
//   ): Subscription {
//     const subscription = new Subscription(path, onUpdate);
//     return subscription;
//   }
// }

//   // public subscribe(config: {
//   //   path: string;
//   //   onUpdate: OnUpdate;
//   //   extraExtraTokenData: ExtraTokenData;
//   //   onSuccess?: OnSuccess;
//   //   onFailure?: OnFailure<FetchTokenError>;
//   // }): Subscription<FetchTokenError, ExtraTokenData> {
//   //   const s = this.createSubscriptionRef(config.path);
//   //   s.onUpdate = config.onUpdate;
//   //   if (config?.onFailure) s.onFailure = config.onFailure;
//   //   if (config?.onSuccess) s.onSuccess = config.onSuccess;
//   //   s.subscribe({ tokenData: config.extraExtraTokenData });
//   //   return new Subscription(s);
//   // }

// class Subscription {
//   constructor(
//     path : String,
//     onUpdate: (newValue: any) => void
//   ) {
//     const subscriptionRef = this.createSubscriptionRef(config.path);

//   }

//   public unsubscribe: () => void;
// }
