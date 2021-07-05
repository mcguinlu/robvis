context("Check blobbograms")

# Set-up ----

dat.bcg <- metafor::dat.bcg

dat <-
  metafor::escalc(
    measure = "RR",
    ai = tpos,
    bi = tneg,
    ci = cpos,
    di = cneg,
    data = dat.bcg,
    slab = paste(author, year)
  )

rma <- metafor::rma(yi, vi, data = dat)

dat.rob2 <- rbind(data_rob2, data_rob2[1:4,])
dat.rob2$Study <- paste(dat$author,dat$year)

# Run tests ----

test_that("RoB2 Blobbogram", {
  local_edition(3)

  fp <- "blobbogram_rob2.png"

  suppressMessages(suppressWarnings(rob_blobbogram(
    rma, dat.rob2, display = F, file_path = fp
  )))

  expect_snapshot_file(fp)
})
