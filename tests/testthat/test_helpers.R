test_that("Correct number of colours returned", {
  expect_equal(length(get_colour("ROB2", "cochrane")), 5)
  expect_equal(length(get_colour("ROB2", "colourblind")), 5)
})

test_that("Convert weird spelling of colour", {
  expect_equal(weird_spelling("colorblind"), "colourblind")
})
