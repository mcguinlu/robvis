context("rob_summary()")

test_that("ROB - Summary", {

  vdiffr::expect_doppelganger("ROB2 - Basic", rob_summary(data_rob2, "ROB2"))
  vdiffr::expect_doppelganger("ROB2 - Weighted", rob_summary(data_rob2, "ROB2", weighted = FALSE))
  vdiffr::expect_doppelganger("ROB2 - Overall", rob_summary(data_rob2, "ROB2", overall = TRUE))
  vdiffr::expect_doppelganger("ROB2 - Colour - cochrane", rob_summary(data_rob2, "ROB2", colour = "cochrane"))
  vdiffr::expect_doppelganger("ROB2 - Colour - colourblind", rob_summary(data_rob2, "ROB2", colour = "colourblind"))
  vdiffr::expect_doppelganger("ROB2 - Colour - custom", rob_summary(data_rob2, "ROB2", colour = c("#f442c8", "#bef441", "#000000", "#bef441")))

  vdiffr::expect_doppelganger("ROB1 - Basic", rob_summary(data_rob1, "Generic"))
  vdiffr::expect_doppelganger("ROB1 - Weighted", rob_summary(data_rob1, "Generic", weighted = FALSE))
  vdiffr::expect_doppelganger("ROB1 - Overall", rob_summary(data_rob1, "Generic", overall = TRUE))
  vdiffr::expect_doppelganger("ROB1 - Colour - cochrane", rob_summary(data_rob1, "Generic", colour = "cochrane"))
  vdiffr::expect_doppelganger("ROB1 - Colour - colourblind", rob_summary(data_rob1, "Generic", colour = "colourblind"))
  vdiffr::expect_doppelganger("ROB1 - Colour - custom", rob_summary(data_rob1, "Generic", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7")))

  vdiffr::expect_doppelganger("ROBINS-I - Basic", rob_summary(data_robins, "ROBINS-I"))
  vdiffr::expect_doppelganger("ROBINS-I - Weighted", rob_summary(data_robins, "ROBINS-I", weighted = FALSE))
  vdiffr::expect_doppelganger("ROBINS-I - Overall", rob_summary(data_robins, "ROBINS-I", overall = TRUE))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - cochrane", rob_summary(data_robins, "ROBINS-I", colour = "cochrane"))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - colourblind", rob_summary(data_robins, "ROBINS-I", colour = "colourblind"))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - custom", rob_summary(data_robins, "ROBINS-I", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7")))

  vdiffr::expect_doppelganger("QUADAS - Basic", rob_summary(data_quadas, "QUADAS-2"))
  vdiffr::expect_doppelganger("QUADAS - Weighted", rob_summary(data_quadas, "QUADAS-2", weighted = FALSE))
  vdiffr::expect_doppelganger("QUADAS - Overall", rob_summary(data_quadas, "QUADAS-2", overall = TRUE))
  vdiffr::expect_doppelganger("QUADAS - Colour - cochrane", rob_summary(data_quadas, "QUADAS-2", colour = "cochrane"))
  vdiffr::expect_doppelganger("QUADAS - Colour - colourblind", rob_summary(data_quadas, "QUADAS-2", colour = "colourblind"))
  vdiffr::expect_doppelganger("QUADAS - Colour - custom", rob_summary(data_quadas, "QUADAS-2", colour = c("#f442c8", "#bef441", "#000000", "#bef441")))
})
