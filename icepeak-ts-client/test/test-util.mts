export {
fetch_dummy_token,
put_data,
Wait,
fill_node_event_queue,
handle_test_result,
log_everything,
simplify_fetch_token
}

import timers from "timers";
import { setTimeout } from 'timers/promises';

import process from 'process';
import child_process from 'child_process';

import util from 'util';

import WebSocket from 'ws'

const exec = util.promisify(child_process.exec);


import type { FetchTokenFn, LogFn, Token } from "../lib/icepeak-core.mjs";

const log_everything : LogFn =
  (logType, logMessage, extra) => {
    extra
      ? console.log(logType + "\n", logMessage, "\n", extra)
      : console.log(logType + "\n", logMessage)
  }

const fetch_dummy_token : FetchTokenFn<null, null> =
  async (_path, _) => {
    return { token: "dummy-token" }
  };

// Adapt a 'IcepeakCore' FetchTokenFn for the simple fetch
// token function of the simpler 'Icepeak' interface
const simplify_fetch_token
  : (f : FetchTokenFn<null, null>)
  => (( p : string ) => Promise<Token>) = f =>
    (path => f(path, { extraTokenData: null })
      .then(t => { if ("tokenRequestError" in t) throw t; return t }))

function put_data(jsonString : string, path : string) : Promise<any> {
  return exec(
    "curl -X PUT -H 'Content-Type: application/json' -d " +
      `'${jsonString}'` +
      " http://localhost:3000" +
      path)
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

const fill_node_event_queue = () => { timers.setTimeout(fill_node_event_queue, 1000) }

function handle_test_result( test : Promise<unknown> ) : void {
  test
    .then(result => {
      console.log("Test finished succesfuly with result:\n" + result)
      process.exit(0)
    })
    .catch(err => {
      console.log("Test threw error:\n" + err)
      process.exit(1)
    })
}
