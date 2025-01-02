import * as test_util from "./test-util.mjs"
import WebSocket from 'ws'

import * as icepeak from "../lib/icepeak.mjs"
import * as icepeak_core from "../lib/icepeak-core.mjs"

test_util.fill_node_event_queue()
test_util.handle_test_result(icepeakCoreTest())


async function icepeakCoreTest() {
  console.log("Starting Test")
  const config = {
    websocketUrl: "ws://localhost:3000/?method=reusable",
    websocketConstructor: (url: string) => new WebSocket(url),
    fetchTokenFn: test_util.fetch_dummy_token,
    calculateRetry: () => null,
    logger: test_util.log_everything
  };

  await test_util.put_data({}, "")
  const w1 = new test_util.Wait()
  const w2 = new test_util.Wait()
  const icepeakCore  = icepeak_core.createIcepeakCore(config);
  const s1 = icepeakCore.createSubscriptionRef("root/sub1")
  const s2 = icepeakCore.createSubscriptionRef("root")
  s2.onUpdate = u => {console.log("S2 updated:", u)}
  s2.subscribe(null)
  s1.onSuccess = _ => {console.log("S1 subscribed"); w1.done()}
  s1.onUpdate = u => {console.log("S1 updated:", u); w2.done()}
  s1.subscribe(null)
  await w1.wait
  test_util.put_data(3, "/root/sub1")
  await w2.wait
}
