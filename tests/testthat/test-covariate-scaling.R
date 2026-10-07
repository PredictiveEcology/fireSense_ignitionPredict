## What the run event hands to the fitted models.

oneFold <- function(covs, ...) toyIgInputs(list(constant(0)), list(constant(0)), covs = covs, ...)

test_that("with rescaleVars = FALSE the models get the covariates exactly as supplied", {
  covs <- toyIgCovariates()
  sim <- toyIgRun(oneFold(covs), params = list(rescaleVars = FALSE))
  nd <- toyCalls("ign1")[[1]]
  expect_setequal(names(nd), c("pixelID", "MDC", "youngAge", "year"))
  expect_identical(nd$MDC, c(100, 120, 140, 160))
  expect_identical(nd$youngAge, c(0, 0.25, 0.5, 1))
  expect_identical(nd$year, rep(2001, 4))
  expect_identical(nd$pixelID, 1:4)
})

test_that("pixelID reaches the models as the cell number, never rescaled", {
  sim <- toyIgRun(oneFold(toyIgCovariates()), params = list(rescaleVars = TRUE))
  expect_identical(toyCalls("ign1")[[1]]$pixelID, 1:4)
})

test_that("covariates are standardised with the FIT's centre and scale", {
  ## The fit standardised MDC with centre 100 and scale 50, and stored that in `scaleData`
  ## (as fireSense_ignitionFit does). A cool year and a year that is 100 MDC units hotter
  ## everywhere must therefore look different to the model:
  ##   cool: (c(100, 120, 140, 160) - 100) / 50 = 0, 0.4, 0.8, 1.2
  ##   hot:  (c(200, 220, 240, 260) - 100) / 50 = 2, 2.4, 2.8, 3.2
  ## Before fireSense_ignitionPredict#21 both years reached the model as -1.162, -0.387, 0.387, 1.162, and `year`
  ## (constant within a year) as NaN.
  scaleData <- list(`scaled:center` = c(MDC = 100, youngAge = 0.5, year = 2000),
                    `scaled:scale`  = c(MDC = 50,  youngAge = 0.5, year = 10))
  seen <- lapply(list(cool = 0, hot = 100), function(add) {
    ins <- oneFold(toyIgCovariates(MDC = c(100, 120, 140, 160) + add))
    ins$fireSense_IgnitionFitted$scaleData <- scaleData
    toyIgRun(ins, params = list(rescaleVars = TRUE))
    toyCalls("ign1")[[1]]
  })
  expect_equal(seen$cool$MDC, c(0, 0.4, 0.8, 1.2))
  expect_equal(seen$hot$MDC, c(2, 2.4, 2.8, 3.2))
  expect_equal(seen$cool$youngAge, c(-1, -0.5, 0, 1))
  expect_equal(seen$cool$year, rep(0.1, 4))
})
