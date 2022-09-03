dat.bcg <- metadat::dat.bcg[c(1:9),]

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

res <- metafor::rma(yi, vi, data = dat)

data_rob_noweights <- data_rob2[,c(1:7)]
data_rob_noweights$Study <- paste(dat$author,dat$year)

test_that("Check weight column is added without error", {
  expect_equal(ncol(rob_append_weights(data_rob_noweights, res)),8)
})

data_rob_noweights[3,1] <- ""

test_that("Check mismatched study names gives error", {
  expect_error(rob_append_weights(data_rob_noweights, res))
})
