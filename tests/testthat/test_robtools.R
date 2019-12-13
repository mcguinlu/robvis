context("rob tools output")

test_that("ROB Tools output",
  expect_equal(rob_tools(), "ROBINS-I Online")
)
