
language_dat <- data_rob2
names(language_dat)[7] <- "Globale"

test_that("ROB - Traffic light", {
  expect_snapshot_file(name = "TF - ROB2 - Basic.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, "ROB2")))
  expect_snapshot_file(name = "TF - ROB2 - Point Size.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, "ROB2", psize = 10)))
  expect_snapshot_file(name = "TF - ROB2 - Colour - cochrane.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, "ROB2", colour = "cochrane")))
  expect_snapshot_file(name = "TF - ROB2 - Colour - colourblind.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, "ROB2", colour = "colourblind")))
  expect_snapshot_file(name = "TF - ROB2 - Colour - custom.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, "ROB2", colour = c("#f442c8", "#bef441", "#000000", "#333333"))))
  expect_snapshot_file(name = "TF - ROB2 - Overall.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2[1:6], "ROB2", overall = FALSE)))

  expect_snapshot_file(name = "TF - ROB2C - Basic.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2_cluster, "ROB2-Cluster")))
  expect_snapshot_file(name = "TF - ROB2C - Point Size.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2_cluster, "ROB2-Cluster", psize = 10)))
  expect_snapshot_file(name = "TF - ROB2C - Colour - cochrane.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2_cluster, "ROB2-Cluster", colour = "cochrane")))
  expect_snapshot_file(name = "TF - ROB2C - Colour - colourblind.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2_cluster, "ROB2-Cluster", colour = "colourblind")))
  expect_snapshot_file(name = "TF - ROB2C - Colour - custom.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2_cluster, "ROB2-Cluster", colour = c("#f442c8", "#bef441", "#000000", "#333333"))))
  expect_snapshot_file(name = "TF - ROB2C - Overall.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2_cluster[1:7], "ROB2-Cluster", overall = FALSE)))

  expect_snapshot_file(name = "TF - ROBINS-I - Basic.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_robins, "ROBINS-I")))
  expect_snapshot_file(name = "TF - ROBINS-I - Point Size.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_robins, "ROBINS-I", psize = 10)))
  expect_snapshot_file(name = "TF - ROBINS-I - Colour - cochrane.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_robins, "ROBINS-I", colour = "cochrane")))
  expect_snapshot_file(name = "TF - ROBINS-I - Colour - colourblind.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_robins, "ROBINS-I", colour = "colourblind")))
  expect_snapshot_file(name = "TF - ROBINS-I - Colour - custom.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_robins, "ROBINS-I", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#333333"))))
  expect_snapshot_file(name = "TF - ROBINS-I - Overall.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_robins[1:8], "ROBINS-I", overall = FALSE)))

  expect_snapshot_file(name = "TF - QUADAS - Basic.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quadas, "QUADAS-2")))
  expect_snapshot_file(name = "TF - QUADAS - Point size.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quadas, "QUADAS-2", psize = 10)))
  expect_snapshot_file(name = "TF - QUADAS - Colour - cochrane.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quadas, "QUADAS-2", colour = "cochrane")))
  expect_snapshot_file(name = "TF - QUADAS - Colour - colourblind.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quadas, "QUADAS-2", colour = "colourblind")))
  expect_snapshot_file(name = "TF - QUADAS - Colour - custom.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quadas, "QUADAS-2", colour = c("#f442c8", "#bef441", "#000000", "#333333"))))
  expect_snapshot_file(name = "TF - QUADAS - Overall.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quadas[1:5], "QUADAS-2", overall = FALSE)))

  expect_snapshot_file(name = "TF - QUIPS - Basic.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quips, "QUIPS")))
  expect_snapshot_file(name = "TF - QUIPS - Point Size.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quips, "QUIPS", psize = 10)))
  expect_snapshot_file(name = "TF - QUIPS - Colour - cochrane.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quips, "QUIPS", colour = "cochrane")))
  expect_snapshot_file(name = "TF - QUIPS - Colour - colourblind.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quips, "QUIPS", colour = "colourblind")))
  expect_snapshot_file(name = "TF - QUIPS - Colour - custom.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quips, "QUIPS", colour = c("#f442c8", "#bef441", "#000000", "#bef441", "#333333"))))
  expect_snapshot_file(name = "TF - QUIPS - Overall.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_quips[1:7], "QUIPS", overall = FALSE)))

  expect_snapshot_file(name = "TF - ROBG - Basic.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, tool = "Generic")))
  expect_snapshot_file(name = "TF - ROBG - Point size.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, tool = "Generic", psize = 15)))
  expect_snapshot_file(name = "TF - ROBG - Judgement labels.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, tool = "Generic", judgement_labels = c("Very bad", "Pretty bad", "Not sure", "Good", "No info", "NA"))))
  expect_snapshot_file(name = "TF - ROBG - Judgement title.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, tool = "Generic", judgement_title = "Assessment")))
  expect_snapshot_file(name = "TF - ROBG - Overall domain.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(language_dat, tool = "Generic")))
  expect_snapshot_file(name = "TF - ROBG - Label x axis.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, tool = "Generic", x_title = "ROB domains")))
  expect_snapshot_file(name = "TF - ROBG - Label y axis.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2, tool = "Generic", y_title = "Trial")))
  expect_snapshot_file(name = "TF - ROBG - Overall.png", rob_save(file = tempfile(fileext = ".png"), rob_traffic_light(data_rob2[1:6], "Generic", overall = FALSE)))

})
