context("rob_traffic_light()")

test_that("ROB - Traffic light",{

skip_on_cran()
# skip_on_travis()
# skip_on_appveyor()

vdiffr::expect_doppelganger("TF - ROB2 - Basic", rob_traffic_light(data_rob2,"ROB2"))
vdiffr::expect_doppelganger("TF - ROB2 - Point Size", rob_traffic_light(data_rob2,"ROB2", psize = 10))
vdiffr::expect_doppelganger("TF - ROB2 - Colour - cochrane", rob_traffic_light(data_rob2,"ROB2", colour = "cochrane"))
vdiffr::expect_doppelganger("TF - ROB2 - Colour - colourblind", rob_traffic_light(data_rob2,"ROB2", colour = "colourblind"))
vdiffr::expect_doppelganger("TF - ROB2 - Colour - custom", rob_traffic_light(data_rob2,"ROB2", colour = c("#f442c8","#bef441","#000000","#333333")))

vdiffr::expect_doppelganger("TF - ROB2C - Basic", rob_traffic_light(data_rob2_cluster,"ROB2-Cluster"))
vdiffr::expect_doppelganger("TF - ROB2C - Point Size", rob_traffic_light(data_rob2_cluster,"ROB2-Cluster", psize = 10))
vdiffr::expect_doppelganger("TF - ROB2C - Colour - cochrane", rob_traffic_light(data_rob2_cluster,"ROB2-Cluster", colour = "cochrane"))
vdiffr::expect_doppelganger("TF - ROB2C - Colour - colourblind", rob_traffic_light(data_rob2_cluster,"ROB2-Cluster", colour = "colourblind"))
vdiffr::expect_doppelganger("TF - ROB2C - Colour - custom", rob_traffic_light(data_rob2_cluster,"ROB2-Cluster", colour = c("#f442c8","#bef441","#000000","#333333")))

vdiffr::expect_doppelganger("TF - ROB1 - Basic", rob_traffic_light(data_rob1,"Generic"))
vdiffr::expect_doppelganger("TF - ROB1 - Point Size", rob_traffic_light(data_rob1,"Generic", psize = 10))
vdiffr::expect_doppelganger("TF - ROB1 - Colour - cochrane", rob_traffic_light(data_rob1,"Generic", colour = "cochrane"))
vdiffr::expect_doppelganger("TF - ROB1 - Colour - colourblind", rob_traffic_light(data_rob1,"Generic", colour = "colourblind"))
vdiffr::expect_doppelganger("TF - ROB1 - Colour - custom", rob_traffic_light(data_rob1,"Generic", colour = c("#f442c8","#bef441","#000000","#333333","#4EA1F7")))

vdiffr::expect_doppelganger("TF - ROBINS-I - Basic", rob_traffic_light(data_robins,"ROBINS-I"))
vdiffr::expect_doppelganger("TF - ROBINS-I - Point Size", rob_traffic_light(data_robins,"ROBINS-I", psize = 10))
vdiffr::expect_doppelganger("TF - ROBINS-I - Colour - cochrane", rob_traffic_light(data_robins,"ROBINS-I", colour = "cochrane"))
vdiffr::expect_doppelganger("TF - ROBINS-I - Colour - colourblind", rob_traffic_light(data_robins,"ROBINS-I", colour = "colourblind"))
vdiffr::expect_doppelganger("TF - ROBINS-I - Colour - custom", rob_traffic_light(data_robins,"ROBINS-I", colour = c("#f442c8","#bef441","#000000","#bef441","#333333")))

vdiffr::expect_doppelganger("TF - QUADAS - Basic", rob_traffic_light(data_quadas,"QUADAS-2"))
vdiffr::expect_doppelganger("TF - QUADAS - Point size", rob_traffic_light(data_quadas,"QUADAS-2", psize = 10))
vdiffr::expect_doppelganger("TF - QUADAS - Colour - cochrane", rob_traffic_light(data_quadas,"QUADAS-2", colour = "cochrane"))
vdiffr::expect_doppelganger("TF - QUADAS - Colour - colourblind", rob_traffic_light(data_quadas,"QUADAS-2", colour = "colourblind"))
vdiffr::expect_doppelganger("TF - QUADAS - Colour - custom", rob_traffic_light(data_quadas,"QUADAS-2", colour = c("#f442c8","#bef441","#000000","#333333")))

})