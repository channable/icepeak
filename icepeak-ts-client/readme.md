# Icepeak TypeScript Client

The icepeak client provides 2 interfaces:
- `lib/icepeak-core.mts` the base interface.
- `lib/icepeak.mts` simplified wrapper over `icepeak-core`.

There also exists a corresponding react hook for them in:
- `lib/useIcepeak.mts`

The general usage pattern is to import the constructor for icepeak i.e `createIcepeakCore` (full) or `createIcepeak` (simple), and then you need to provide it as an argument the corresponding config, i.e `IcepeakCoreConfig`, or `IcepeakConfig`


## Example: Using the Simplified `icepeak` Interface

```ts
import * as icepeak from "../lib/icepeak.mjs"

// Minimal required config
const config : icepeak.IcepeakConfig = {
  websocketUrl: "ws://localhost:3000/?method=reusable",
  websocketConstructor: url => new WebSocket(url),
  fetchToken: async (path: string) => { return "dummmy-token" }
}

const icepeakObject = icepeak.createIcepeak(config);

const subscription = icepeakObj.subscribe(
  "/root/sub1",
  val => console.log("I received an update!", val))
}

...

subscription.unsubscribe()
```

## Full Configuration Settings

```ts
// from icepeak-core.mts

// Returns the number of milliseconds to wait before retrying.
type CalculateRetryFn = (event: ws.ErrorEvent | ws.CloseEvent) => null | number

type LogFn = (logType : LogType, logMessage : string, extra? : unknown) => void
type LogType = "Debug" | "InternalError" | "UserError"

type FetchTokenFn<FetchTokenError, FetchTokenExtraData> = (
  path: string,
  extraTokenData: ExtraTokenData<FetchTokenExtraData>,
) => Promise<Token | TokenRequestError<FetchTokenError>>

type IcepeakCoreConfig<FetchTokenError, ExtraTokenData> = {
  websocketUrl: string,
  websocketConstructor: (url: string) => ws.WebSocket,
  fetchTokenFn: FetchTokenFn<FetchTokenError, ExtraTokenData>,
  calculateRetry: CalculateRetryFn,
  logger: LogFn
}
```

The `test` folder contains example usage for `icepeak-core`.
