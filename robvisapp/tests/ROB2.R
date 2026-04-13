app <- ShinyDriver$new("../", seed = 42, loadTimeout = 1000000)
app$snapshotInit("ROB2")
Sys.sleep(1.5)
app$setInputs(gotodata = "click")
app$setInputs(tool = "ROB2")
app$uploadFile(data = "test-data/ROB2_example (5).xlsx") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(gen_plots = "click")
app$waitFor("trafficplotUI")
app$snapshot(list(output = "trafficplotUI"))
app$snapshot(list(output = "trafficlightplot"))

# app$snapshot()
