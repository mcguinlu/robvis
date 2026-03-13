## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(robvis)
library(metafor)
library(dplyr)

# Define your studies, using the BCG dataset included in the metadat package
dat_bcg <- metadat::dat.bcg

glimpse(dat_bcg)

# Create some example data for ROB2 using rob_dummy(), and add it to the BCG
# data.
# We don't need a "Study" column for this example, so we set `study = FALSE`

dat_rob <- rob_dummy(13, 
                     "ROB2",
                     study = FALSE)

dat_analysis <- cbind(dat_bcg, dat_rob)

glimpse(dat_analysis)

## -----------------------------------------------------------------------------
# Calculate effect estimates and sampling variances for each study
dat_analysis <-
  metafor::escalc(
    measure = "RR",
    ai = tpos,
    bi = tneg,
    ci = cpos,
    di = cneg,
    data = dat_analysis
  )

# Perform the meta-analysis
res <- metafor::rma(yi,
                    vi,
                    data = dat_analysis,
                    slab = paste(author, year))

# Explore the results
res

## ---- fig.width=10------------------------------------------------------------
rob_forest(res, rob_tool = "ROB2")

