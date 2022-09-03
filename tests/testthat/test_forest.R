# Set-up ----

# Perform meta-analysis
set.seed(1)

test_that("Check paired plots", {
  expect_snapshot_file(save_png({
    rob_forest(get_res("ROB2"),"ROB2")

  }), "paired_rob2.png")


    expect_snapshot_file(save_png({
      rob_forest(get_res("ROBINS-I"),"ROBINS-I")

    }), "paired_robinsi.png")

    expect_snapshot_file(save_png({
      rob_forest(get_res("ROBINS-E"),"ROBINS-E")

    }), "paired_robinse.png")


  expect_snapshot_file(save_png({
    rob_forest(
      get_res("ROB2"),
      atransf = exp,
      xlim = c(-16, 7),
      at = log(c(.05, .25, 1, 4)),
      ilab = cbind(metadat::dat.bcg$tpos,
                   metadat::dat.bcg$tneg,
                   metadat::dat.bcg$cpos,
                   metadat::dat.bcg$cneg),
      ilab.xpos = c(-9.5, -8, -6, -4.5),
      header = "Author(s) and Year",
      textpos = c(-16, 6),
      cex = 1,
      mlab = "RE Model (Q=",
      rob_caption = FALSE
    )
  }), "paired_rob2_complex.png")

  expect_snapshot_file(save_png({
    rob_forest(
      get_res("ROBINS-I"),
      rob_tool = "ROBINS-I",
      atransf = exp,
      xlim = c(-16, 7),
      at = log(c(.05, .25, 1, 4)),
      ilab = cbind(metadat::dat.bcg$tpos,
                   metadat::dat.bcg$tneg,
                   metadat::dat.bcg$cpos,
                   metadat::dat.bcg$cneg),
      ilab.xpos = c(-9.5, -8, -6, -4.5),
      header = "Author(s) and Year",
      textpos = c(-16, 6),
      cex = 1,
      mlab = "RE Model (Q=",
      rob_caption = FALSE
    )
  }), "paired_robinsi_complex.png")

})
