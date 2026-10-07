## What the run event predicts and draws, on a 2 x 2 coarse grid over an 8 x 8 fine grid.
## Default toy: two ignition folds that predict 0, 2, 3, 0 and 0, 4, 3, 0 expected
## ignitions for coarse cells 1..4, so only coarse cells 2 and 3 can ignite.

twoFolds <- function(escFuns = list(constant(1)), ...) {
  toyIgInputs(list(byPixel(c(0, 2, 3, 0)), byPixel(c(0, 4, 3, 0))), escFuns, ...)
}

test_that("ignition probability is the mean of the per-fold predictions, on the coarse grid", {
  sim <- toyIgRun(twoFolds())
  ras <- sim$fireSense_IgAndEscapeProbRas
  expect_identical(names(ras), c("ignitionProb", "escapeProb"))
  expect_true(terra::compareGeom(ras, toyCoarse()))
  ## fold means: (0+0)/2, (2+4)/2, (3+3)/2, (0+0)/2
  expect_equal(probVals(sim)[, "ignitionProb"], c(0, 3, 3, 0))
})

test_that("escape probability is the fold mean, clamped to [0, 1] per fold, only where there are ignitions", {
  ## fold 1 predicts 1.7 (clamped to 1), fold 2 predicts 0.5: mean 0.75
  sim <- toyIgRun(twoFolds(list(constant(1.7), constant(0.5))))
  expect_equal(probVals(sim)[, "escapeProb"], c(NA, 0.75, 0.75, NA))
  ## fold 1 predicts -0.5 (clamped to 0), fold 2 predicts 0.1: mean 0.05
  sim <- toyIgRun(twoFolds(list(constant(-0.5), constant(0.1))))
  expect_equal(probVals(sim)[, "escapeProb"], c(NA, 0.05, 0.05, NA))
  expect_equal(unique(igTable(sim)$escapeProb), 0.05)
})

test_that("escape models only see the coarse cells that ignited", {
  sim <- toyIgRun(twoFolds())
  escCalls <- toyCalls("esc1")
  expect_length(escCalls, 1L)
  expect_identical(escCalls[[1]]$pixelID, 2:3)
  ## the ignition models see all four
  expect_identical(toyCalls("ign1")[[1]]$pixelID, 1:4)
  expect_identical(toyCalls("ign2")[[1]]$pixelID, 1:4)
})

test_that("only models whose name contains 'Fold' are used", {
  ins <- toyIgInputs(list(byPixel(c(0, 2, 3, 0)), byPixel(c(0, 4, 3, 0)), constant(1000)),
                     list(constant(1)), foldNames = c("Fold1", "Fold2", "full"))
  sim <- toyIgRun(ins)
  expect_equal(probVals(sim)[, "ignitionProb"], c(0, 3, 3, 0)) # not (…+1000)/3
  expect_length(toyCalls("ign3"), 0L)
})

test_that("each ignition is a distinct flammable fine cell inside its own coarse cell", {
  for (seed in 1:3) {
    sim <- toyIgRun(twoFolds(), seed = seed)
    ig <- igTable(sim)
    expect_identical(names(ig), c("pixelID", "igProb", "ignitions", "escapeProb", "escapes", "escaped"))
    expect_gt(nrow(ig), 0)
    expect_false(anyDuplicated(ig$pixelID) > 0)
    coarse <- coarseOfFine(ig$pixelID)
    expect_true(all(coarse %in% c(2, 3)))       # coarse 1 and 4 have 0 expected ignitions
    expect_false(any(ig$pixelID %in% c(5, 6)))  # not flammable, though inside coarse 2
    ## one row per ignition: a coarse cell with n ignitions has n rows, each saying n
    expect_identical(as.vector(table(coarse)[as.character(coarse)]), as.integer(ig$ignitions))
    expect_equal(ig$igProb, rep(3, nrow(ig)))
  }
})

test_that("escapes are certain with probability 1 and impossible with probability 0", {
  ig <- igTable(toyIgRun(twoFolds(list(constant(1)))))
  expect_identical(ig$escapes, ig$ignitions)
  expect_true(all(ig$ignitions >= 1L))
  ig <- igTable(toyIgRun(twoFolds(list(constant(0)))))
  expect_identical(ig$escapes, rep(0L, nrow(ig)))
  expect_gt(nrow(ig), 0)
})

test_that("the draws are reproducible under set.seed", {
  a <- igTable(toyIgRun(twoFolds(list(constant(0.5))), seed = 7))
  b <- igTable(toyIgRun(twoFolds(list(constant(0.5))), seed = 7))
  expect_identical(a, b)
  d <- igTable(toyIgRun(twoFolds(list(constant(0.5))), seed = 8))
  expect_false(identical(a, d))
})

test_that("no expected ignitions anywhere gives no ignitions", {
  sim <- toyIgRun(toyIgInputs(list(constant(0)), list(constant(1))))
  expect_identical(nrow(sim$ignitionsAndEscapes), 0L)
  expect_equal(probVals(sim)[, "ignitionProb"], c(0, 0, 0, 0))
  expect_true(all(is.na(probVals(sim)[, "escapeProb"])))
  expect_length(toyCalls("esc1")[[1]]$pixelID, 0L)
})

test_that("coarse cells with an NA covariate are dropped: no prediction, no ignition", {
  covs <- toyIgCovariates()
  data.table::set(covs, 2L, "MDC", NA_real_)
  ## fold predicts 3 wherever it is asked
  sim <- toyIgRun(toyIgInputs(list(constant(3)), list(constant(1)), covs = covs))
  expect_identical(toyCalls("ign1")[[1]]$pixelID, c(1L, 3L, 4L))
  expect_equal(probVals(sim)[, "ignitionProb"], c(3, NA, 3, 3))
  expect_false(any(coarseOfFine(igTable(sim)$pixelID) == 2))
})

test_that("a fully non-flammable coarse cell gets no ignitions even if the model predicts them", {
  ## coarse 2 = fine rows 1-4, cols 5-8
  nf <- as.integer(outer(5:8, (0:3) * 8, `+`))
  expect_true(all(coarseOfFine(nf) == 2))
  sim <- toyIgRun(twoFolds(flammableRTM = toyFlammable(nf)))
  ig <- igTable(sim)
  expect_gt(nrow(ig), 0)
  expect_true(all(coarseOfFine(ig$pixelID) == 3))
  ## the probability raster still reports the prediction for coarse 2
  expect_equal(probVals(sim)[, "ignitionProb"], c(0, 3, 3, 0))
})

test_that("the covariate table in the simList is not modified", {
  sim <- toyIgRun(twoFolds())
  expect_equal(as.data.frame(sim$fireSense_igAndEscapePred_Covariates),
               as.data.frame(toyIgCovariates()))
})

test_that("draws with seed 42 match development", {
  ## Stochastic, so pinned: values from origin/development at 277672d (R 4.6.1).
  ## `ignitions` and `escapes` are the counts of the coarse cell, repeated on each of its rows.
  ig <- igTable(toyIgRun(twoFolds(list(constant(0.5))), seed = 42))
  ig <- ig[order(ig$pixelID), ]
  expect_identical(ig$pixelID, c(14L, 15L, 21L, 23L, 34L, 36L, 50L, 58L))
  ## cells 14..23 are in coarse 2 (4 ignitions, 2 escapes); 34..58 in coarse 3 (4 and 3)
  expect_identical(as.integer(ig$ignitions), rep(4L, 8))
  expect_identical(as.integer(ig$escapes), rep(c(2L, 3L), each = 4))
  expect_equal(ig$escapeProb, rep(0.5, 8))
})
