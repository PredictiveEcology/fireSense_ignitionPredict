test_that("a fitted object without a list of fold models stops", {
  ins <- toyIgInputs(list(constant(1)), list(constant(1)))
  ins$fireSense_IgnitionFitted$modelList$model <- NULL
  expect_error(toyIgRun(ins), "Not tested anymore")
})

test_that("an ignition placed in a coarse cell with ONE flammable fine cell lands in that cell", {
  skip(paste("known defect: sample(x, size) with length-1 x samples from 1:x, so the ignition",
             "can land in any lower-numbered fine cell, in another coarse cell"))
  ## coarse 4 = fine rows 5-8, cols 5-8; leave only fine cell 64 flammable in it
  nf <- setdiff(as.integer(outer(5:8, (4:7) * 8, `+`)), 64L)
  for (seed in 1:8) {
    ins <- toyIgInputs(list(byPixel(c(0, 0, 0, 0.7))), list(constant(1)),
                       flammableRTM = toyFlammable(nf))
    ig <- igTable(toyIgRun(ins, seed = seed))
    expect_true(all(ig$pixelID == 64L))
  }
})

test_that("more ignitions than flammable fine cells does not stop the simulation", {
  skip(paste("known defect: sample() without replacement errors with 'cannot take a sample",
             "larger than the population' when ignitions > flammable fine cells in the coarse cell"))
  nf <- setdiff(as.integer(outer(5:8, (4:7) * 8, `+`)), 64L)
  ins <- toyIgInputs(list(byPixel(c(0, 0, 0, 20))), list(constant(1)),
                     flammableRTM = toyFlammable(nf))
  ig <- igTable(toyIgRun(ins))
  expect_true(all(ig$pixelID == 64L))
})
