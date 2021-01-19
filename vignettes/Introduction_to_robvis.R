## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = 8,
  fig.height = 2.41)

set.seed(42)

library(robvis)


## ---- eval = TRUE-------------------------------------------------------------
rob_tools()

## ---- eval=FALSE--------------------------------------------------------------
#  data <- read.csv("path/to/summary_table.csv", header = TRUE)

## ----headrob, echo = FALSE----------------------------------------------------
knitr::kable(data_rob2)

## -----------------------------------------------------------------------------
rob_summary(data_rob2, tool = "ROB2")

## -----------------------------------------------------------------------------
rob_summary(data_robins, tool = "ROBINS-I")

## -----------------------------------------------------------------------------
rob_summary(data_quadas, tool = "QUADAS-2")

## -----------------------------------------------------------------------------
rob_summary(data_rob2, tool = "ROB2", overall = TRUE)

## -----------------------------------------------------------------------------
data_rob2_weighted <- data_rob2

# Assign a random number between 1 and 10 as the weight for each study
data_rob2_weighted$Weights <- sample(1:10,9)

# Produce a weighted barplot
rob_summary(data_rob2_weighted, tool = "ROB2", weighted = TRUE)

## -----------------------------------------------------------------------------
rob_summary(data = data_rob2, tool = "ROB2", colour = "colourblind")

## -----------------------------------------------------------------------------
rob_summary(data = data_rob2, tool = "ROB2", colour = c("#f442c8","#bef441","#000000","#557925"))

## ---- fig.width = 6, fig.height = 9, fig.align="center"-----------------------
rob_traffic_light(data_rob2, tool = "ROB2")

## ---- fig.width = 7.5, fig.height = 10.5, fig.align="center"------------------
rob_traffic_light(data_robins, tool = "ROBINS-I")

## ---- fig.width = 7.5, fig.height = 10.5, fig.align="center"------------------
rob_traffic_light(data_quadas, tool = "QUADAS-2")

## ---- fig.width = 6, fig.height = 9, fig.align="center"-----------------------
# Generate larger dataset
data <- rbind(data_rob2, data_rob2)
data$Study <- paste("Study",seq(1,18))

# Plot with reduced point size
rob_traffic_light(data, tool = "ROB2", psize = 10)

## ---- fig.width = 6, fig.height = 9, fig.align="center"-----------------------
rob_traffic_light(data = data_rob2, tool = "ROB2", colour = "colourblind")

## ---- fig.width = 6, fig.height = 9, fig.align="center"-----------------------
rob_traffic_light(data = data_rob2, tool = "ROB2", colour = c("#f442c8","#bef441","#000000","#557925"))

## -----------------------------------------------------------------------------
colnames(data_rob2)

colnames(data_rob1)

## ---- echo = FALSE------------------------------------------------------------
colnames(data_rob1)[2] <- "This is a test"
rob_summary(data_rob1, tool = "Generic")

## ---- fig.width = 7, fig.height = 9, fig.align="center", echo = FALSE---------
rob_traffic_light(data_rob1, tool = "Generic")

