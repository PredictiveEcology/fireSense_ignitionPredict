## Real xgboost boosters, as the ledger stores them: saved and read back, which makes them
## external pointers with no `$params` (xgboost >= 3).
realBooster <- function(objective = "count:poisson", seed = 1) {
  set.seed(seed)
  x <- matrix(rnorm(200), ncol = 2, dimnames = list(NULL, c("MDC", "youngAge")))
  y <- rpois(100, exp(0.3 * x[, 1]))
  b <- xgboost::xgboost(x, y, objective = objective, nrounds = 3, nthreads = 1,
                        max_depth = 2)
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f))
  saveRDS(b, f)
  readRDS(f)
}

ledgerFit <- function(b1, b2)
  list(elf = "A", ign = list(modelList = list(model = list(Fold1 = b1, Fold2 = b2),
                                              scaleData = toyScaleData())))

test_that("the objective of saved and reloaded xgboost models is read", {
  skip_if_not_installed("xgboost")
  fits <- list(ledgerFit(realBooster(), realBooster(seed = 2)))
  expect_identical(fitObjective(fits, "ign"), "count:poisson")
})

test_that("xgboost folds with different objectives stop", {
  skip_if_not_installed("xgboost")
  fits <- list(ledgerFit(realBooster(), realBooster("reg:squarederror")))
  expect_error(fitObjective(fits, "ign"), "different objectives: count:poisson, reg:squarederror")
})

test_that("real boosters predict covariates that carry coreFraction, which is not a feature, and the fit still stores it unscaled", {
  skip_if_not_installed("xgboost")
  fit <- ledgerFit(realBooster(), realBooster(seed = 2))$ign
  covs <- data.frame(pixelID = 1:3, MDC = c(-1, 0, 1), youngAge = c(0, 1, 0), year = 2001)
  withCore <- cbind(covs, coreFraction = c(1, 0.5, 0))
  expect_equal(fireSenseUtils::foldMeanPrediction(fit, withCore), fireSenseUtils::foldMeanPrediction(fit, covs))
  expect_null(fireSenseUtils::storedFitProblem(fit, withCore))
  ## scaling the covariates as the fit did leaves coreFraction alone
  sd <- toyScaleData()
  expect_identical(scaleAsFit(data.table::as.data.table(withCore), sd, "fireSense_IgnitionFitted")$coreFraction, withCore$coreFraction)
})
