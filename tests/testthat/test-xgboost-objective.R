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
