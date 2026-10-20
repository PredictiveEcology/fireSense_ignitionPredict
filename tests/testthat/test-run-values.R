## What the run event predicts, on a 2 x 2 coarse grid. Nothing is drawn: the module makes expected-value
## surfaces and fireSense_burn draws from them.
## Default toy: two ignition folds that predict 0, 2, 3, 0 and 0, 4, 3, 0 expected
## ignitions for coarse cells 1..4.

twoFolds <- function(escFuns = list(constant(1)), ...) {
  toyIgInputs(list(byPixel(c(0, 2, 3, 0)), byPixel(c(0, 4, 3, 0))), escFuns, ...)
}

test_that("ignition surface is the mean of the per-fold predictions, on the coarse grid", {
  sim <- toyIgRun(twoFolds())
  ras <- sim$fireSense_IgAndEscapeProbRas
  expect_identical(names(ras), c("ignitionProb", "escapeProb", "ignitionYearSD", "escapeYearSD"))
  expect_true(terra::compareGeom(ras, toyCoarse()))
  ## fold means: (0+0)/2, (2+4)/2, (3+3)/2, (0+0)/2
  expect_equal(probVals(sim)[, "ignitionProb"], c(0, 3, 3, 0))
})

test_that("the escape surface is the fold mean of the escape model for every pixel, not clamped to 1", {
  ## folds predict 1.7 and 0.5: mean 1.1, a count, so above 1
  sim <- toyIgRun(twoFolds(list(constant(1.7), constant(0.5))))
  expect_equal(probVals(sim)[, "escapeProb"], rep(1.1, 4))
  ## also where there are no expected ignitions (cells 1 and 4)
  expect_false(anyNA(probVals(sim)[, "escapeProb"]))
})

test_that("predictions below 0 are made 0 per fold", {
  ## fold 1 predicts -0.5 (made 0), fold 2 predicts 0.1: mean 0.05
  sim <- toyIgRun(twoFolds(list(constant(-0.5), constant(0.1))))
  expect_equal(probVals(sim)[, "escapeProb"], rep(0.05, 4))
})

test_that("escape models see every coarse cell, ignition models too", {
  toyIgRun(twoFolds())
  escCalls <- toyCalls("esc1")
  expect_length(escCalls, 1L)
  expect_identical(escCalls[[1]]$pixelID, 1:4)
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

test_that("no ignitionsAndEscapes is made: the draws are fireSense_burn's", {
  expect_null(toyIgRun(twoFolds())$ignitionsAndEscapes)
})

test_that("the distribution is read from the fitted models' objective", {
  sim <- toyIgRun(twoFolds())
  expect_identical(sim$fireSense_IgAndEscapeDistribution,
                   list(ignition = "count:poisson", escape = "count:poisson"))
  ins <- twoFolds()
  attr(ins$fireSense_EscapeFitted$modelList$model$Fold1, "params")$objective <- "reg:tweedie"
  expect_identical(toyIgRun(ins)$fireSense_IgAndEscapeDistribution,
                   list(ignition = "count:poisson", escape = "reg:tweedie"))
})

test_that("an unreadable objective stops", {
  ins <- twoFolds()
  attr(ins$fireSense_IgnitionFitted$modelList$model$Fold1, "params") <- NULL
  expect_error(toyIgRun(ins), "cannot read the objective of a fitted ignition model")
  ins <- twoFolds()
  attr(ins$fireSense_EscapeFitted$modelList$model$Fold1, "params")$objective <- NULL
  expect_error(toyIgRun(ins), "cannot read the objective of a fitted escape model")
})

test_that("folds with different objectives stop", {
  ins <- toyIgInputs(list(byPixel(c(0, 2, 3, 0)), byPixel(c(0, 4, 3, 0))), list(constant(1)))
  attr(ins$fireSense_IgnitionFitted$modelList$model$Fold2, "params")$objective <- "reg:tweedie"
  expect_error(toyIgRun(ins), "folds of the ignition model.*different objectives: count:poisson, reg:tweedie")
})

test_that("coarse cells with an NA covariate are dropped: no prediction", {
  covs <- toyIgCovariates()
  data.table::set(covs, 2L, "MDC", NA_real_)
  ## fold predicts 3 wherever it is asked
  sim <- toyIgRun(toyIgInputs(list(constant(3)), list(constant(1)), covs = covs))
  expect_identical(toyCalls("ign1")[[1]]$pixelID, c(1L, 3L, 4L))
  expect_equal(probVals(sim)[, "ignitionProb"], c(3, NA, 3, 3))
  expect_equal(probVals(sim)[, "escapeProb"], c(1, NA, 1, 1))
})

test_that("the covariate table in the simList is not modified", {
  sim <- toyIgRun(twoFolds())
  expect_equal(as.data.frame(sim$fireSense_igAndEscapePred_Covariates),
               as.data.frame(toyIgCovariates()))
})

test_that("each pixel gets its fits' year-effect sd; a fit stored without one gives 0", {
  ## the toy fits have no modelList$yearSD, as ledger rows written before it existed
  sim <- toyIgRun(twoFolds())
  expect_equal(probVals(sim)[, "ignitionYearSD"], rep(0, 4))
  expect_equal(probVals(sim)[, "escapeYearSD"], rep(0, 4))
  ins <- twoFolds()
  ins$fireSense_IgnitionFitted$modelList$yearSD <- 0.45
  ins$fireSense_EscapeFitted$modelList$yearSD <- 0.8
  sim <- toyIgRun(ins)
  expect_equal(probVals(sim)[, "ignitionYearSD"], rep(0.45, 4))
  expect_equal(probVals(sim)[, "escapeYearSD"], rep(0.8, 4))
  ## the means are unchanged: the year effect is drawn in fireSense_burn
  expect_equal(probVals(sim)[, c("ignitionProb", "escapeProb")], probVals(toyIgRun(twoFolds()))[, c("ignitionProb", "escapeProb")])
})
