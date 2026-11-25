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
  ## only Poisson fits can be stored fits that predict (fireSenseUtils::storedFitProblem())
  expect_error(toyIgRun(ins), "escape fit of ELF \\(one fit\\).*reg:tweedie, not count:poisson")
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

test_that("a fit's fitted year effect of this year becomes a layer; without one the raster is unchanged", {
  ins <- twoFolds()
  ins$fireSense_IgnitionFitted$modelList$yearEffects <- data.frame(year = c(2000, 2001), multiplier = c(3, 1.7),
                                                                    shape = c(10, 40))
  ins$fireSense_EscapeFitted$modelList$yearEffects <- data.frame(year = c(2000, 2001), multiplier = c(0.2, 0.6),
                                                                 shape = c(5, NA))
  sim <- toyIgRun(ins)   # time 2001
  expect_identical(names(sim$fireSense_IgAndEscapeProbRas),
                   c("ignitionProb", "escapeProb", "ignitionYearSD", "escapeYearSD", "ignitionYearEffect", "escapeYearEffect",
                     "ignitionYearEffectShape", "escapeYearEffectShape"))
  expect_equal(probVals(sim)[, "ignitionYearEffect"], rep(1.7, 4))
  expect_equal(probVals(sim)[, "escapeYearEffect"], rep(0.6, 4))
  expect_equal(probVals(sim)[, "ignitionYearEffectShape"], rep(40, 4))
  expect_equal(probVals(sim)[, "escapeYearEffectShape"], rep(NA_real_, 4))
  ## the means are unchanged: fireSense_burn applies the effect
  expect_equal(probVals(sim)[, c("ignitionProb", "escapeProb")], probVals(toyIgRun(twoFolds()))[, c("ignitionProb", "escapeProb")])
  ## a year the fits have no effect for: no layers
  ins$fireSense_IgnitionFitted$modelList$yearEffects$year <- c(1990, 1991)
  ins$fireSense_EscapeFitted$modelList$yearEffects$year <- c(1990, 1991)
  expect_identical(names(toyIgRun(ins)$fireSense_IgAndEscapeProbRas),
                   c("ignitionProb", "escapeProb", "ignitionYearSD", "escapeYearSD"))
})

## the year climate term of the ignitions (fireSense_ignitionFit's `ignitionYearClimate`)
withYearClimate <- function(ins, yc) {
  ins$fireSense_IgnitionFitted$modelList$yearClimate <- yc
  ins
}
toyClimate <- list(variable = "MDC", center = 130, scale = 20, beta = 0.5, norm = 1)

test_that("a fit without a year climate term gives a multiplier of 1", {
  expect_identical(yearClimateMultiplierOf(twoFolds()$fireSense_IgnitionFitted, toyIgCovariates()), 1)
  expect_equal(probVals(toyIgRun(twoFolds()))[, "ignitionProb"], c(0, 3, 3, 0))
})

test_that("a drier year raises the expected ignitions by exp(beta * z) / norm, escapes untouched", {
  ## the mean MDC of the toy year is 130 = the fit's centre: no change
  expect_equal(probVals(toyIgRun(withYearClimate(twoFolds(), toyClimate)))[, "ignitionProb"], c(0, 3, 3, 0))
  ## 20 drier: z = 1
  dry <- twoFolds(covs = toyIgCovariates(MDC = c(100, 120, 140, 160) + 20))
  sim <- toyIgRun(withYearClimate(dry, toyClimate))
  expect_equal(probVals(sim)[, "ignitionProb"], c(0, 3, 3, 0) * exp(0.5))
  expect_equal(probVals(sim)[, "escapeProb"], rep(1, 4))
  expect_equal(probVals(toyIgRun(withYearClimate(dry, modifyList(toyClimate, list(norm = 2)))))[, "ignitionProb"],
               c(0, 3, 3, 0) * exp(0.5) / 2)
  ## beta 0 is no term
  sim0 <- toyIgRun(withYearClimate(dry, modifyList(toyClimate, list(beta = 0))))
  expect_equal(probVals(sim0)[, "ignitionProb"], c(0, 3, 3, 0))
  ## the raw column is used whether or not the models see standardised covariates
  expect_equal(probVals(toyIgRun(withYearClimate(dry, toyClimate), params = list(rescaleVars = FALSE)))[, "ignitionProb"],
               c(0, 3, 3, 0) * exp(0.5))
})

test_that("the multiplier is the fit's definition: unweighted mean over the rows with core and no NA", {
  covs <- data.frame(year = 2001, MDC = c(100, 140, 1000, 120), coreFraction = c(1, 0.1, 0, 1), other = c(1, 1, 1, NA))
  ## rows 1 and 2 count (row 3 has no core, row 4 an NA): mean 120, whatever the coreFraction of the others
  expect_equal(yearClimateMultiplierOf(withYearClimate(twoFolds(), toyClimate)$fireSense_IgnitionFitted, covs),
               exp(0.5 * (120 - 130) / 20))
})

test_that("a stored variable missing from the covariates, or several years in them, stop", {
  expect_error(toyIgRun(withYearClimate(twoFolds(), modifyList(toyClimate, list(variable = "CMDsm")))),
               "not among the covariates")
  two <- twoFolds(covs = toyIgCovariates(year = c(2001, 2001, 2002, 2002)))
  expect_error(toyIgRun(withYearClimate(two, toyClimate)), "hold 2 years")
  ## no year column: all rows are one year
  noYear <- toyIgCovariates(); data.table::set(noYear, NULL, "year", NULL)
  expect_no_error(toyIgRun(withYearClimate(twoFolds(covs = noYear), toyClimate)))
})

test_that("the year mean is taken before na.omit: an NA in an unrelated column does not leave the pixel out of it", {
  covs <- toyIgCovariates(MDC = c(120, 140, 160, 180))     # mean 150 over the four pixels, 153.3 without pixel 2
  data.table::set(covs, NULL, "lightningDays", c(1, NA, 1, 1))
  sim <- toyIgRun(withYearClimate(twoFolds(covs = covs), toyClimate))
  expect_equal(probVals(sim)[c(1, 3, 4), "ignitionProb"], c(0, 3, 0) * exp(0.5 * (150 - 130) / 20))
})

test_that("a fit with a year climate term and covariates without coreFraction stops with a clear message", {
  noCore <- toyIgCovariates(); data.table::set(noCore, NULL, "coreFraction", NULL)
  expect_error(toyIgRun(withYearClimate(twoFolds(covs = noCore), toyClimate)), "no `coreFraction` column")
  ## a fit without the term does not need it
  expect_no_error(toyIgRun(twoFolds(covs = noCore)))
})

test_that("a year with no usable covariate gives a multiplier of 1 and a warning", {
  expect_warning(m <- yearClimateMultiplierOf(withYearClimate(twoFolds(), toyClimate)$fireSense_IgnitionFitted,
                                              toyIgCovariates()[integer(0), , drop = FALSE]), "it is 1")
  expect_identical(m, 1)
})

## the year climate term of the escapes (fireSense_ignitionFit's `escapeYearClimate`), with the same year mean
test_that("an escape fit's year climate term scales the expected escapes by the same z, ignitions untouched", {
  dry <- twoFolds(covs = toyIgCovariates(MDC = c(100, 120, 140, 160) + 20))   # z = 1
  dry$fireSense_EscapeFitted$modelList$yearClimate <- modifyList(toyClimate, list(beta = 0.3, norm = 2))
  sim <- toyIgRun(dry)
  expect_equal(probVals(sim)[, "escapeProb"], rep(1, 4) * exp(0.3) / 2)
  expect_equal(probVals(sim)[, "ignitionProb"], c(0, 3, 3, 0))
  ## both terms at once: each fit's own beta, one z
  both <- withYearClimate(dry, toyClimate)
  simB <- toyIgRun(both)
  expect_equal(probVals(simB)[, "ignitionProb"], c(0, 3, 3, 0) * exp(0.5))
  expect_equal(probVals(simB)[, "escapeProb"], rep(1, 4) * exp(0.3) / 2)
  ## the hinge: the dry half is steeper
  dry$fireSense_EscapeFitted$modelList$yearClimate$beta2 <- 0.4
  expect_equal(probVals(toyIgRun(dry))[, "escapeProb"], rep(1, 4) * exp(0.3 + 0.4) / 2)
  ## a wet year (z = -1) does not feel it
  wet <- twoFolds(covs = toyIgCovariates(MDC = c(100, 120, 140, 160) - 20))
  wet$fireSense_EscapeFitted$modelList$yearClimate <- dry$fireSense_EscapeFitted$modelList$yearClimate
  expect_equal(probVals(toyIgRun(wet))[, "escapeProb"], rep(1, 4) * exp(-0.3) / 2)
  ## no term (the default): escapes are untouched
  expect_equal(probVals(toyIgRun(twoFolds(covs = toyIgCovariates(MDC = c(100, 120, 140, 160) + 20))))[, "escapeProb"], rep(1, 4))
})

test_that("an escape fit with a term and covariates without coreFraction stops", {
  noCore <- toyIgCovariates(); data.table::set(noCore, NULL, "coreFraction", NULL)
  ins <- twoFolds(covs = noCore)
  ins$fireSense_EscapeFitted$modelList$yearClimate <- toyClimate
  expect_error(toyIgRun(ins), "no `coreFraction` column")
})
