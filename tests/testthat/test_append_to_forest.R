context("Check Blobbograms")

# Perform meta-analysis
  dat.bcg <- metafor::dat.bcg

  dat <-
    metafor::escalc(
      measure = "RR",
      ai = tpos,
      bi = tneg,
      ci = cpos,
      di = cneg,
      data = dat.bcg,
      slab = paste(author, year, sep = ", ")
    )

  res <- metafor::rma(yi, vi, data = dat)

# Define new SVG writing function
  # svglite can't handle saving the output of metafor::forest, as it is >1 page
  # This new function wraps the svg() file saving function, and orders the
  # arguments so that it can be used as the writer argument of
  # vdiffr::expect_doppleganger()
  svg_ordered <- function(fig, testcase, title = ""){
    grDevices::svg(filename = testcase, width = 10, height = 7)
    fig
    grDevices::dev.off()
  }

# Check basic resulting figures ----

  # ROB2
  dat.rob2 <- rbind(data_rob2, data_rob2[1:4,])
  dat.rob2$Study <- paste(dat$author,dat$year)

  dat.robins <- rbind(data_robins, data_robins[1,])
  dat.robins$Study <- paste(dat$author,dat$year)

  test_that("ROB - Append forest - Errors", {

    vector <- c()

    expect_error(rob_append_to_forest(vector, dat.rob2))

    dat.rob.misnamed <- dat.rob2
    dat.rob.misnamed[3,1] <- "Blarg"

    expect_error(rob_append_to_forest(res, dat.rob.misnamed))
  })

  test_that("ROB - Append ROB2 - Drawn?",{

    expect_equivalent(TRUE, TRUE) # avoid 'Empty test' message

    skip_on_cran()

    rob_append_to_forest(res,
                         dat.rob2,
                         rob_caption = FALSE)

    rob_append_to_forest(
      res,
      dat.rob2,
      atransf = exp,
      xlim = c(-16, 7),
      at = log(c(.05, .25, 1, 4)),
      ilab = cbind(dat.bcg$tpos,
                   dat.bcg$tneg,
                   dat.bcg$cpos,
                   dat.bcg$cneg),
      ilab.xpos = c(-9.5,-8,-6,-4.5),
      header = "Author(s) and Year",
      textpos = c(-16, 6),
      cex = 1,
      mlab = paste0(
        "RE Model (Q=",
        formatC(res$QE, digits = 2, format =
                  "f"),
        ", df=",
        res$k - res$p,
        ", p=",
        formatC(res$QEp, digits = 2, format = "f"),
        ";  I^2=",
        formatC(res$I2, digits = 1, format =
                  "f"),
        "%)"
      ),
      rob_caption = FALSE
    )
  })


  test_that("ROB - Append ROBINS-I - Drawn?",{

    expect_equivalent(TRUE, TRUE) # avoid 'Empty test' message

    skip_on_cran()

    rob_append_to_forest(res,
                         dat.robins,
                         rob_tool = "ROBINS-I",
                         rob_caption = FALSE)

    rob_append_to_forest(
      res,
      dat.robins,
      rob_tool = "ROBINS-I",
      atransf = exp,
      xlim = c(-16, 7),
      at = log(c(.05, .25, 1, 4)),
      ilab = cbind(dat.bcg$tpos,
                   dat.bcg$tneg,
                   dat.bcg$cpos,
                   dat.bcg$cneg),
      ilab.xpos = c(-9.5,-8,-6,-4.5),
      header = "Author(s) and Year",
      textpos = c(-16, 6),
      cex = 1,
      mlab = paste0(
        "RE Model (Q=",
        formatC(res$QE, digits = 2, format =
                  "f"),
        ", df=",
        res$k - res$p,
        ", p=",
        formatC(res$QEp, digits = 2, format = "f"),
        ";  I^2=",
        formatC(res$I2, digits = 1, format =
                  "f"),
        "%)"
      ),
      rob_caption = FALSE
    )
  })

# Tests

# png("test.png", width = 2000,height = 1200,res = 200)
#
# rob_append_to_forest(res,
#                      dat.robins,
#                      rob_tool = "ROBINS-I",
#                      rob_caption = FALSE)
#
# dev.off()

