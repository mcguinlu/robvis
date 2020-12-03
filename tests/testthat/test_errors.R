context("Check error messages")

# Tools ----
test_that("No tool specified", {
  expect_error(rob_summary(data_rob2))
  expect_error(rob_traffic_light(data_rob2))
})

test_that("Tool specified incorrectly", {
  expect_error(rob_summary(data_rob2, "Rob2"))
  expect_error(rob_traffic_light(data_rob2, "Rob2"))
})

test_that("ROB1 gives message", {
  expect_message(rob_summary(data_rob1, "ROB1"))
  expect_message(rob_traffic_light(data_rob1, "ROB1"))
})

# Colours ----

test_that("Colour errors", {
          expect_error(rob_summary(data_rob2, "ROB2", colour = c("#FFFFFF")))
          expect_error(rob_traffic_light(data_robins,"ROBINS-I", colour = c("#FFFFFF")))
})

# Columns ----

test_sum <- data_rob2[, c(1:6)]

test_that("Wrong number of columns - summary", {
  expect_error(
    rob_summary(test_sum, "ROB2", overall = TRUE, weighted = FALSE),
    "an \"Overall\" column."
  )
  expect_error(
    rob_summary(test_sum, "ROB2", overall = FALSE, weighted = TRUE),
    "columns, and \"Weight\" column"
  )
  expect_error(
    rob_summary(test_sum, "ROB2", overall = TRUE, weighted = TRUE),
    "an \"Overall\" column and a \"Weight\" column."
  )

  expect_error(
    rob_summary(data_rob2[, c(1:5)], "ROB2", overall = FALSE, weighted = FALSE),
    "\"Domain\" columns."
  )
})

test_that("Last column not weights - summary", {
  expect_error(
    rob_summary(data_rob2[, c(1:7)], "ROB2", overall = FALSE, weighted = TRUE),
    "expected for weighted = TRUE"
  )
  expect_error(
    rob_summary(data_rob2[, c(1:7)], "Generic", overall = FALSE, weighted = TRUE),
    "expected for weighted = TRUE"
  )
})

test_tf <- data_rob2[, c(1:6)]

test_that("Too few columns - traffic light", {
  expect_error(rob_traffic_light(test_tf, "ROB2"))
})

# Headers ----

data_header <- data_rob2
data_header[1,] <- names(data_header)

test_that("Data contains headers", {
  expect_error(rob_summary(data_header, "ROB2"))
  expect_error(rob_traffic_light(data_header,"ROB2"))
})
