## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = 8,
  fig.height = 2.41)
  library(ggplot2)
  library(robvis)

## ----headrob-------------------------------------------------------------
knitr::kable(data_rob)

## ----rob2summary---------------------------------------------------------
rob_summary(data_rob, tool = "ROB2")

## ----robinssummary-------------------------------------------------------
rob_summary(data_robins, tool = "ROBINS-I")

## ----quadassummary-------------------------------------------------------
rob_summary(data_quadas, tool = "QUADAS-2")

