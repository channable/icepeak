import { Icepeak, type TokenRequest } from "../lib/icepeak.mjs"
import WebSocket from 'ws'
import { setTimeout } from 'timers/promises';
import util from 'util';
import process from 'process';

import child_process from 'child_process';

import timers from "timers";

const exec = util.promisify(child_process.exec);

const fetchToken : TokenRequest<void, null>
  = async (_path, _extraTokenData) => {
    return { token: "dummy-token" }
  };

const putData = (jsonString : string, path : string) => {
  return exec("curl -X PUT -H 'Content-Type: application/json' -d " + `'${jsonString}'` + " http://localhost:3000" + path)
}


class Wait {
  public wait : Promise<null>;

  public done() : void {
    this.resolve();
  }

  private resolve : () => void;
  constructor(){
    this.resolve = () => {
      console.error("'Wait' internal error: Author had faulty understanding of JS semantics.")
      process.exit(1)
    }

    // Is this asynchronous? Probably not?
    this.wait = new Promise<null>((resolve, _) => {
      this.resolve = () => resolve(null)
    });
  };
}

async function runTest() {
  console.log("Starting Test")
  await putData(JSON.stringify({}), "")

  const icepeak = new Icepeak(
    url => new WebSocket(url),
    "ws://localhost:3000/?method=reusable",
    fetchToken,
    () => 100
  );

  const w1 = new Wait()
  const w2 = new Wait()
  const w3 = new Wait()


  console.log("Registering subscribers to Icepeak.")
  const subscription = icepeak.subscribe(
    { path: "hi",
      onUpdate : val => console.log("S1, UPDATE:", val),
      onFailure : err => console.log("S1, ERROR:", err),
      onSuccess : s => {
	console.log("S1, SUCCESS:", s)
	w1.done()
      },
      extraTokenData : null
    })

  const subscription3 = icepeak.subscribe(
    { path: "hi",
      onUpdate : val => console.log("S3, UPDATE:", val),
      onFailure : err => console.log("S3, ERROR:", err),
      onSuccess : s => {
	console.log("S3, SUCCESS", s)
	w3.done()
      },
      extraTokenData : null
    })

  const subscription2 = icepeak.subscribe(
    { path: "hi/there",
      onUpdate : val => console.log("S2, UPDATE:", val),
      onFailure : err => console.log("S2, ERROR:", err),
      onSuccess : s => {
	console.log("S2, SUCCESS", s)
	// w2.done()
      },
      extraTokenData : null
    })

  w2.done()
  subscription2.unsubscribe()

  await Promise.all([w1.wait, w2.wait, w3.wait]);
  console.log("Done waiting.")
  console.log("Putting data to the path 'hi/there'.")
  await putData(JSON.stringify({ there : "root of hi there" }), "/hi")

  const subscription4 = icepeak.subscribe(
    { path: "hi",
      onUpdate : val => console.log("S4, UPDATE:", val),
      onFailure : err => console.log("S4, ERROR:", err),
      onSuccess : s => {
	console.log("S4, SUCCESS", s)
      },
      extraTokenData : null
    })

  subscription.unsubscribe()
  subscription2.unsubscribe()
  subscription3.unsubscribe()


  console.log("Putting data to the path 'hi/there'.")
  await putData(JSON.stringify({ there : "root of hi there" }), "/hi")

  await setTimeout(3 * 1000)

  icepeak.destroy()
  await setTimeout(3 * 1000)

  console.log("Finishing Spec")
  return
}

async function runSpecRetry() {
  console.log("Starting Retry Test")

  const icepeak = new Icepeak(
    url => new WebSocket(url),
    "ws://localhost:3000/?method=reusable",
    fetchToken,
    (_errorEvent => {
      return 1000
    })
  );

  icepeak.subscribe(
    { path: "hi",
      onUpdate : val => console.log("S1, UPDATE:", val),
      onFailure : err => console.log("S1, ERROR:", err),
      onSuccess : s => {
	console.log("S1, SUCCESS", s)
      },
      extraTokenData : null
    })

  const w1 = new Wait()
  await w1.wait			// because the 'script' of this promise has exited, the node runtime does not wait to exit the program, since there is no scripts left on the event queue
  return
}

const forever = () => { timers.setTimeout(forever, 1000) }
forever()

runSpecRetry()
  .then(result => {
    console.log("Test finished:", result)
    process.exit(0)
  })
  .catch(err => {
    console.log("Test threw error:", err)
    process.exit(1)
  })

// runTest()
//   .then(result => {
//     console.log("Test finished:", result)
//     process.exit(0)
//   })
//   .catch(err => {
//     console.log("Test threw error:", err)
//     process.exit(1)
//   })
