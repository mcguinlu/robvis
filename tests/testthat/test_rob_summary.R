context("Check summary plots")

test_that("ROB - Summary", {
  vdiffr::expect_doppelganger("ROB2 - Basic", rob_summary(data_rob2, "ROB2", overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB2 - Weighted", rob_summary(data_rob2, "ROB2", weighted = FALSE, overall = FALSE))
  vdiffr::expect_doppelganger("ROB2 - Overall", rob_summary(data_rob2, "ROB2", weighted = TRUE))
  vdiffr::expect_doppelganger("ROB2 - Colour - cochrane", rob_summary(data_rob2, "ROB2", colour = "cochrane", overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB2 - Colour - colourblind", rob_summary(data_rob2, "ROB2", colour = "colourblind", overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB2 - Colour - custom", rob_summary(data_rob2, "ROB2", colour = c("#f442c8", "#bef441", "#000000", "#bef441"), overall = FALSE, weighted = TRUE))

  vdiffr::expect_doppelganger("ROB1 - Basic", rob_summary(data_rob1, "Generic", overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB1 - Weighted", rob_summary(data_rob1, "Generic", weighted = FALSE, overall = FALSE))
  vdiffr::expect_doppelganger("ROB1 - Overall", rob_summary(data_rob1, "Generic", weighted = TRUE))
  vdiffr::expect_doppelganger("ROB1 - Colour - cochrane", rob_summary(data_rob1, "Generic", colour = "cochrane", overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB1 - Colour - colourblind", rob_summary(data_rob1, "Generic", colour = "colourblind", overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB1 - Colour - custom", rob_summary(data_rob1, "Generic", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7"), overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB1 - Judgement Labels", rob_summary(data_rob1, "Generic", judgement_labels = c("Test1","Test2","Test3","Test4","NI"), overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROB1 - ROBINS-I Judgement Labels", rob_summary(data_robins, "Generic", judgement_labels = c("Test1","Test2","Test3","Test4","NI"), overall = TRUE, weighted = TRUE))



  vdiffr::expect_doppelganger("ROBINS-I - Basic", rob_summary(data_robins, "ROBINS-I", overall = FALSE, weighted = TRUE))
  vdiffr::expect_doppelganger("ROBINS-I - Weighted", rob_summary(data_robins, "ROBINS-I", weighted = FALSE, overall = FALSE))
  vdiffr::expect_doppelganger("ROBINS-I - Overall", rob_summary(data_robins, "ROBINS-I",weighted = TRUE))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - cochrane", rob_summary(data_robins, "ROBINS-I", colour = "cochrane", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - colourblind", rob_summary(data_robins, "ROBINS-I", colour = "colourblind", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("ROBINS-I - Colour - custom", rob_summary(data_robins, "ROBINS-I", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7"), overall = FALSE,weighted = TRUE))

  vdiffr::expect_doppelganger("QUIPS - Basic", rob_summary(data_quips, "QUIPS", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("QUIPS - Weighted", rob_summary(data_quips, "QUIPS", weighted = FALSE, overall = FALSE))
  vdiffr::expect_doppelganger("QUIPS - Overall", rob_summary(data_quips, "QUIPS",weighted = TRUE))
  vdiffr::expect_doppelganger("QUIPS - Colour - cochrane", rob_summary(data_quips, "QUIPS", colour = "cochrane", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("QUIPS - Colour - colourblind", rob_summary(data_quips, "QUIPS", colour = "colourblind", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("QUIPS - Colour - custom", rob_summary(data_quips, "QUIPS", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#4EA1F7"), overall = FALSE,weighted = TRUE))

  vdiffr::expect_doppelganger("QUADAS - Basic", rob_summary(data_quadas, "QUADAS-2", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("QUADAS - Weighted", rob_summary(data_quadas, "QUADAS-2", weighted = FALSE, overall = FALSE))
  vdiffr::expect_doppelganger("QUADAS - Overall", rob_summary(data_quadas, "QUADAS-2",weighted = TRUE))
  vdiffr::expect_doppelganger("QUADAS - Colour - cochrane", rob_summary(data_quadas, "QUADAS-2", colour = "cochrane", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("QUADAS - Colour - colourblind", rob_summary(data_quadas, "QUADAS-2", colour = "colourblind", overall = FALSE,weighted = TRUE))
  vdiffr::expect_doppelganger("QUADAS - Colour - custom", rob_summary(data_quadas, "QUADAS-2", colour = c("#f442c8", "#bef441", "#000000", "#bef441"), overall = FALSE,weighted = TRUE))
})
