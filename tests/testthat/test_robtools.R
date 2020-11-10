context("rob_tools()")

test_that("Length of rob_tools() output", {
  expect_equal(length(suppressMessages(rob_tools())), 7)
})
