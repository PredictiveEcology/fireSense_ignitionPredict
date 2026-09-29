## The models were fitted on covariates standardised once, over all fitting years. Prediction
## must use that same centre and scale, which fireSense_ignitionFit stores as `scaleData`.

oneFold <- function(covs, ...) toyIgInputs(list(constant(1)), list(constant(0)), covs = covs, ...)

test_that("covariates are standardised with the FIT's centre and scale, whatever the year's own mean", {
  ## fit: MDC centre 100, scale 50. A cool year, and one 100 MDC units hotter everywhere:
  ##   cool: (c(100, 120, 140, 160) - 100) / 50 = 0, 0.4, 0.8, 1.2
  ##   hot:  (c(200, 220, 240, 260) - 100) / 50 = 2, 2.4, 2.8, 3.2
  ## Standardised with each year's own mean and sd, both would be -1.162, -0.387, 0.387, 1.162.
  seen <- lapply(list(cool = 0, hot = 100), function(add) {
    toyIgRun(oneFold(toyIgCovariates(MDC = c(100, 120, 140, 160) + add)))
    toyCalls("ign1")[[1]]
  })
  expect_equal(seen$cool$MDC, c(0, 0.4, 0.8, 1.2))
  expect_equal(seen$hot$MDC, c(2, 2.4, 2.8, 3.2))
  expect_equal(seen$cool$youngAge, c(-1, -0.5, 0, 1))
  expect_equal(seen$cool$year, rep(0.1, 4))
  ## pixelID is in the fit's scaleData too, but it is the cell number
  expect_identical(seen$cool$pixelID, 1:4)
})

test_that("an integer covariate is standardised, not truncated", {
  covs <- toyIgCovariates()
  covs$year <- 2001L
  toyIgRun(oneFold(covs))
  expect_equal(toyCalls("ign1")[[1]]$year, rep(0.1, 4))
})

test_that("the escape models get the escape fit's centre and scale", {
  esc <- toyScaleData(center = c(MDC = 120), scale = c(MDC = 20))
  toyIgRun(oneFold(toyIgCovariates(), escScaleData = esc))
  ## every coarse pixel ignites (lambda = 1 with seed 42 gives zeros too, so match on pixelID)
  ign <- toyCalls("ign1")[[1]]
  escSeen <- toyCalls("esc1")[[1]]
  expect_gt(nrow(escSeen), 0)
  expect_equal(ign$MDC, c(0, 0.4, 0.8, 1.2))
  expect_equal(escSeen$MDC, (c(100, 120, 140, 160)[escSeen$pixelID] - 120) / 20)
  ## not in the escape fit's scaleData, so left alone
  expect_equal(escSeen$youngAge, c(0, 0.25, 0.5, 1)[escSeen$pixelID])
})

test_that("with rescaleVars = FALSE the models get the covariates exactly as supplied", {
  toyIgRun(oneFold(toyIgCovariates()), params = list(rescaleVars = FALSE))
  nd <- toyCalls("ign1")[[1]]
  expect_identical(nd$MDC, c(100, 120, 140, 160))
  expect_identical(nd$youngAge, c(0, 0.25, 0.5, 1))
})

test_that("rescaleVars = TRUE with a fit that carries no scaleData is an error", {
  expect_error(toyIgRun(oneFold(toyIgCovariates(), ignScaleData = NULL)), "scaleData")
})
