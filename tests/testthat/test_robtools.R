context("rob_tools()")

test_that("Length of rob_tools() output", {
  # expect_error(rob_summary(data_rob2))
  expect_equal(length(rob_tools()),6)
})
