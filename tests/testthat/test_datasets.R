# For all 4 datasets

# Dimensions
test_that("Data dimensions correct - data_rob2", {
  expect_equal(ncol(data_rob2), 7)
  expect_equal(nrow(data_rob2), 9)
})

test_that("Data dimensions correct - data_rob1", {
  expect_equal(ncol(data_rob1), 9)
  expect_equal(nrow(data_rob1), 9)
})

test_that("Data dimensions correct - data_robins", {
  expect_equal(ncol(data_robins), 9)
  expect_equal(nrow(data_robins), 12)
})

test_that("Data dimensions correct - data_quadas", {
  expect_equal(ncol(data_quadas), 6)
  expect_equal(nrow(data_quadas), 12)
})
