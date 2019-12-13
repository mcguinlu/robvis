context("Check included datasets")

# For all 4 datasets

test_that("Size of data_rob2", {
          expect_equal(length(colnames(data_rob2)),8)
          expect_equal(length(rownames(data_rob2)),9)
          }
          )

test_that("Size of data_robins", {
  expect_equal(length(colnames(data_robins)),10)
  expect_equal(length(rownames(data_robins)),12)
}
)

test_that("Size of data_quadas", {
  expect_equal(length(colnames(data_quadas)),7)
  expect_equal(length(rownames(data_quadas)),12)
}
)

test_that("Size of rob1", {
  expect_equal(length(colnames(data_rob1)),10)
  expect_equal(length(rownames(data_rob1)),9)
}
)
