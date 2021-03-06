context("Check summary plots")

test_that("ROB - Summary", {
  vdiffr::expect_doppelganger("ROB2 - Basic", rob_summary(data_rob2, "ROB2"))
  vdiffr::expect_doppelganger("ROB2 - Overall", rob_summary(data_rob2[1:6], "ROB2", overall = FALSE))
  vdiffr::expect_doppelganger("ROB2 - Colour - cochrane", rob_summary(data_rob2, "ROB2", colour = "cochrane"))
  vdiffr::expect_doppelganger("ROB2 - Colour - colourblind", rob_summary(data_rob2, "ROB2", colour = "colourblind"))
  vdiffr::expect_doppelganger("ROB2 - Colour - custom", rob_summary(data_rob2, "ROB2", colour = c("#f442c8", "#bef441", "#000000", "#bef441")))

  vdiffr::expect_doppelganger("ROB1 - Basic", rob_summary(data_rob1, "Generic"))
  vdiffr::expect_doppelganger("ROB1 - Overall", rob_summary(data_rob1[1:8], "Generic", overall = FALSE))
  vdiffr::expect_doppelganger("ROB1 - Colour - cochrane", rob_summary(data_rob1, "Generic", colour = "cochrane"))
  vdiffr::expect_doppelganger("ROB1 - Colour - colourblind", rob_summary(data_rob1, "Generic", colour = "colourblind"))
  vdiffr::expect_doppelganger("ROB1 - Colour - custom", rob_summary(data_rob1, "Generic", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7")))
  vdiffr::expect_doppelganger("ROB1 - Judgement Labels", rob_summary(data_rob1, "Generic", judgement_labels = c("Test1","Test2","Test3","Test4","NI")))
  vdiffr::expect_doppelganger("ROB1 - ROBINS-I Judgement Labels", rob_summary(data_robins, "Generic", judgement_labels = c("Test1","Test2","Test3","Test4","NI"), overall = TRUE))

  vdiffr::expect_doppelganger("ROBINS-I - Basic", rob_summary(data_robins, "ROBINS-I"))
  vdiffr::expect_doppelganger("ROBINS-I - Overall", rob_summary(data_robins[1:8], "ROBINS-I", overall = FALSE))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - cochrane", rob_summary(data_robins, "ROBINS-I", colour = "cochrane"))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - colourblind", rob_summary(data_robins, "ROBINS-I", colour = "colourblind"))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - custom", rob_summary(data_robins, "ROBINS-I", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7")))

  vdiffr::expect_doppelganger("QUIPS - Basic", rob_summary(data_quips, "QUIPS"))
  vdiffr::expect_doppelganger("QUIPS - Overall", rob_summary(data_quips[1:7], "QUIPS", overall = FALSE))
  vdiffr::expect_doppelganger("QUIPS - Colour - cochrane", rob_summary(data_quips, "QUIPS", colour = "cochrane"))
  vdiffr::expect_doppelganger("QUIPS - Colour - colourblind", rob_summary(data_quips, "QUIPS", colour = "colourblind"))
  vdiffr::expect_doppelganger("QUIPS - Colour - custom", rob_summary(data_quips, "QUIPS", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7")))

  vdiffr::expect_doppelganger("QUADAS - Basic", rob_summary(data_quadas, "QUADAS-2"))
  vdiffr::expect_doppelganger("QUADAS - Overall", rob_summary(data_quadas[1:5], "QUADAS-2", overall = FALSE))
  vdiffr::expect_doppelganger("QUADAS - Colour - cochrane", rob_summary(data_quadas, "QUADAS-2", colour = "cochrane"))
  vdiffr::expect_doppelganger("QUADAS - Colour - colourblind", rob_summary(data_quadas, "QUADAS-2", colour = "colourblind"))
  vdiffr::expect_doppelganger("QUADAS - Colour - custom", rob_summary(data_quadas, "QUADAS-2", colour = c("#f442c8", "#bef441", "#000000", "#bef441")))
})
