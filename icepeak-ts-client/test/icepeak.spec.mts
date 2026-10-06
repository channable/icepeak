import * as test_util from "./test-util.mjs"
import WebSocket from 'ws'

import * as icepeak from "../lib/icepeak.mjs"

test_util.fill_node_event_queue()
test_util.handle_test_result(icepeakTest())

async function icepeakTest() {
  console.log("Starting Test")

  // Clear Icepeak server data
  await test_util.put_data({}, "")

  const config : icepeak.IcepeakConfig = {
    websocketUrl: "ws://localhost:3000/?method=reusable",
    websocketConstructor: url => new WebSocket(url),
    fetchToken: test_util.simplify_fetch_token(test_util.fetch_dummy_token),
    logger: test_util.log_everything,
  }

  const w1 = new test_util.Wait()
  const w2 = new test_util.Wait()
  const icepeakObj = icepeak.createIcepeak(config);
  // const s1 = icepeakObj.subscribe("root", val => { console.log("s1 received:", val); w1.done() })

  // await test_util.put_data(8, "/root")
  // await test_util.put_data(11, "/root")
  const s2 = icepeakObj.subscribe("/root/sub1", val => { console.log("s2 received:", val); w2.done() })
  const s3 = icepeakObj.subscribe("/root/sub1", val => { console.log("s3 received:", val); w2.done() })

  // await test_util.put_data(12, "/root/sub1/sub2")
  await test_util.put_data({"sub1": { "sub2": 11 }}, "/root")
  await test_util.put_data({"sub1": { "sub2": 11 }}, "/root")
  await test_util.put_data({"sub1": { "sub2": 11 }}, "/root")
  // s1.unsubscribe()
  // s2.unsubscribe()
  // s3.unsubscribe()

  await w1.wait
  await w2.wait

}


