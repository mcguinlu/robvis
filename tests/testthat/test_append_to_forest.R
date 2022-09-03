# Set-up ----

# Perform meta-analysis
dat.bcg <- metadat::dat.bcg

dat <-
  metafor::escalc(
    measure = "RR",
    ai = tpos,
    bi = tneg,
    ci = cpos,
    di = cneg,
    data = dat.bcg,
    slab = paste(author, year, sep = ", ")
  ) %>%
  dplyr::mutate(Study = paste(author, year))


# Prep bias datasets
dat.rob2 <- rbind(data_rob2, data_rob2[1:4, ])
dat.rob2$Study <- paste(dat$author, dat$year)
dat.rob2 <- dplyr::left_join(dat, dat.rob2, by=c("Study" = "Study"))
res.rob2 <- metafor::rma(yi, vi, data = dat.rob2, slab = paste(author, year))

dat.robins <- rbind(data_robins, data_robins[1, ])
dat.robins$Study <- paste(dat$author, dat$year)
dat.robins <- dplyr::left_join(dat, dat.robins, by=c("Study" = "Study"))
res.robins <- metafor::rma(yi, vi, data = dat.robins, slab = paste(author, year))

# Tests ----

# test_that("ROB - Append forest - Errors", {
#   vector <- c()
#
#   expect_error(rob_append_to_forest(vector, dat.rob2))
#
#   dat.rob.misnamed <- dat.rob2
#   dat.rob.misnamed[3, 1] <- "Blarg"
#
#   expect_error(rob_append_to_forest(res, dat.rob.misnamed))
# })
#
# test_that("ROB - Append ROB2 - Message?", {
#   skip_on_cran()
#
#   expect_message(rob_append_to_forest(res,
#                                       dat.rob2,
#                                       rob_caption = TRUE), )
#
#   dev.off()
#
# })

testthat::local_edition(3)

save_png <- function(code,
                     width = 1200,
                     height = 800) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code

  return(path)
}

test_that("Check paired plots", {
  expect_snapshot_file(save_png({
    rob_forest(res.rob2)

  }), "paired_rob2.png")


  # expect_snapshot_file(save_png({
  #   rob_forest(
  #     res.rob2,
  #     atransf = exp,
  #     xlim = c(-16, 7),
  #     at = log(c(.05, .25, 1, 4)),
  #     ilab = cbind(dat.bcg$tpos,
  #                  dat.bcg$tneg,
  #                  dat.bcg$cpos,
  #                  dat.bcg$cneg),
  #     ilab.xpos = c(-9.5, -8, -6, -4.5),
  #     header = "Author(s) and Year",
  #     textpos = c(-16, 6),
  #     cex = 1,
  #     mlab = paste0(
  #       "RE Model (Q=",
  #       formatC(res.rob2$QE, digits = 2, format =
  #                 "f"),
  #       ", df=",
  #       res.rob2$k - res.rob2$p,
  #       ", p=",
  #       formatC(res.rob2$QEp, digits = 2, format = "f"),
  #       ";  I^2=",
  #       formatC(res.rob2$I2, digits = 1, format =
  #                 "f"),
  #       "%)"
  #     ),
  #     rob_caption = FALSE
  #   )
  # }), "paired_rob2_complex.png")



  expect_snapshot_file(save_png({
    rob_forest(res.robins,
                         rob_tool = "ROBINS-I")

  }), "paired_robinsi.png")


  # expect_snapshot_file(save_png({
  #   rob_forest(
  #     res.robins,
  #     rob_tool = "ROBINS-I",
  #     atransf = exp,
  #     xlim = c(-16, 7),
  #     at = log(c(.05, .25, 1, 4)),
  #     ilab = cbind(dat.bcg$tpos,
  #                  dat.bcg$tneg,
  #                  dat.bcg$cpos,
  #                  dat.bcg$cneg),
  #     ilab.xpos = c(-9.5, -8, -6, -4.5),
  #     header = "Author(s) and Year",
  #     textpos = c(-16, 6),
  #     cex = 1,
  #     mlab = paste0(
  #       "RE Model (Q=",
  #       formatC(res.robins$QE, digits = 2, format =
  #                 "f"),
  #       ", df=",
  #       res.robins$k - res.robins$p,
  #       ", p=",
  #       formatC(res.robins$QEp, digits = 2, format = "f"),
  #       ";  I^2=",
  #       formatC(res.robins$I2, digits = 1, format =
  #                 "f"),
  #       "%)"
  #     ),
  #     rob_caption = FALSE
  #   )
  # }), "paired_robinsi_complex.png")

})
