testthat::local_edition(3)

save_png <- function(code, width = 1200, height = 800) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code

  return(path)
}

dat <- robvis::data_bias_direction_raw %>%
  triangulate::tri_to_long() %>%
  triangulate::tri_absolute_direction() %>%
  triangulate::tri_to_wide()

test_that("Test basic bias direction plots",{

  expect_snapshot_file(save_png({

      rob_direction(dat, vi = dat$vi)

  }), "paried_basic.png")

})
