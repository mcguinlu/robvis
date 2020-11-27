context("Check saving")

rob_fig <- rob_traffic_light(data_rob2, "ROB2")

rob_save(rob_fig,"test.png")

test_that("Test PNG download", {
  expect_equal(file.exists("test.png"),TRUE)
})

if (file.exists("test.png") == TRUE) {
  unlink("test.png", recursive = TRUE)
}

rob_save(rob_fig,"test.jpeg")

test_that("Test jpeg download", {
  expect_equal(file.exists("test.jpeg"),TRUE)
})

if (file.exists("test.jpeg") == TRUE) {
  unlink("test.jpeg", recursive = TRUE)
}

rob_save(rob_fig,"test.tiff")

test_that("Test tiff download", {
  expect_equal(file.exists("test.tiff"),TRUE)
})

if (file.exists("test.tiff") == TRUE) {
  unlink("test.tiff", recursive = TRUE)
}

rob_save(rob_fig,"test.eps")

test_that("Test eps download", {
  expect_equal(file.exists("test.eps"),TRUE)
})

if (file.exists("test.eps") == TRUE) {
  unlink("test.eps", recursive = TRUE)
}
