dat <- data_bias_direction %>%
  triangulate::tri_to_long() %>%
  triangulate::tri_absolute_direction() %>%
  triangulate::tri_to_wide()

test_that("Test basic bias direction plots",{

  expect_snapshot_file(save_png({

      rob_direction(dat, vi = dat$vi)

  }), "paried_basic.png")

})
