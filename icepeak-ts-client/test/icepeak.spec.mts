import * as test_util from "./test-util.mjs"
import WebSocket from 'ws'

import * as icepeak from "../lib/icepeak.mjs"

test_util.fill_node_event_queue()
test_util.handle_test_result(icepeakTest())

async function icepeakTest() {
  console.log("Starting Test")

  // Clear Icepeak server data
  await test_util.put_data(JSON.stringify({}), "")

  const config : icepeak.IcepeakConfig = {
    websocketUrl: "ws://localhost:3000/?method=reusable",
    websocketConstructor: url => new WebSocket(url),
    fetchToken: test_util.simplify_fetch_token(test_util.fetch_dummy_token),
    logger: test_util.log_everything,
  }

  const w1 = new test_util.Wait()
  const icepeakObj = icepeak.createIcepeak(config);
  icepeakObj.subscribe("hi", val => console.log("Received value:", val))
  await w1.wait


}


