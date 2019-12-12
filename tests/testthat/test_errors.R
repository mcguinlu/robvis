context("Check error messages")

# Number of columns




# Number of judgements
# Tool must be specified

test_that("Tool specified", {
          # expect_error(rob_summary(data_rob2))
          expect_error(rob_traffic_light(data_rob2))
          })

test_that("Tool case", {
  expect_error(rob_summary(data_rob2, "Rob2"))
  expect_error(rob_traffic_light(data_rob2, "Rob2"))
})


test_that("Colour errors", {
          expect_error(rob_summary(data_rob2, "ROB2", colour = c("#FFFFFF")))
          expect_error(rob_traffic_light(data_rob2,"ROB2", colour = c("#FFFFFF")))
})

test_sum <- data_rob2[, c(1:7)]
test_tf <- data_rob2[, c(1:6)]

test_that("Too few columns", {
  expect_error(rob_summary(test, "ROB2"))
  expect_error(rob_traffic_light(test, "ROB2"))
})

test_sum <- data_robins[, c(1:9)]
test_tf <- data_robins[, c(1:8)]

test_that("Too few columns", {
  expect_error(rob_summary(test, "ROBINS-I"))
  expect_error(rob_traffic_light(test, "ROBINS-I"))
})





