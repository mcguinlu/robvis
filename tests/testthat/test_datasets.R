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


test_that("Data crosscheck", {
  expect_identical(data_robins, read.csv("data_raw/data_robins.csv"))
  expect_identical(data_rob2, read.csv("data_raw/data_rob2.csv"))
  expect_identical(data_quadas, read.csv("data_raw/data_quadas.csv"))
  expect_identical(data_rob1, read.csv("data_raw/data_rob1.csv"))
})

