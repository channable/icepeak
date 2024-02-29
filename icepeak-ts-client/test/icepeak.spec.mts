import * as test_util from "./test-util.mjs"

test_util.fill_node_event_queue()
test_util.handle_test_result(icepeakTest())

async function icepeakTest() {
  console.log("Starting Test")
  await test_util.put_data(JSON.stringify({}), "")

}


