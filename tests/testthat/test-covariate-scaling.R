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

test_that("a species the fit used and the vegetation lacks is 0 in raw units, then standardised with the fit's centre and scale", {
  ## the covariates have no Pinu_con column: the species is absent, so its biomass is 0. The fit standardised
  ## Pinu_con with centre 10 and scale 5, so the model must see (0 - 10) / 5 = -2, not a missing column.
  variable.names.toyFold <- function(object, ...) object$features
  registerS3method("variable.names", "toyFold", variable.names.toyFold)
  scaleData <- list(`scaled:center` = c(MDC = 100, Pinu_con = 10, youngAge = 0.5, year = 2000),
                    `scaled:scale`  = c(MDC = 50, Pinu_con = 5, youngAge = 0.5, year = 10))
  ins <- oneFold(toyIgCovariates())
  for (nm in c("fireSense_IgnitionFitted", "fireSense_EscapeFitted")) {
    ins[[nm]]$modelList$model$Fold1$features <- c("MDC", "Pinu_con", "youngAge")
    ins[[nm]]$scaleData <- scaleData
  }
  toyIgRun(ins, params = list(rescaleVars = TRUE))
  expect_equal(toyCalls("ign1")[[1]]$Pinu_con, rep(-2, 4))
  expect_equal(toyCalls("esc1")[[1]]$Pinu_con, rep(-2, 4))
  ## unscaled, it is the raw 0
  toyIgRun(ins, params = list(rescaleVars = FALSE))
  expect_equal(toyCalls("ign1")[[1]]$Pinu_con, rep(0, 4))
  ## a species column the fit never saw is a rename, not an absence: nothing is added
  ins2 <- ins
  ins2$fireSense_igAndEscapePred_Covariates <- toyIgCovariates()
  data.table::set(ins2$fireSense_igAndEscapePred_Covariates, NULL, "Pinu_new", 3)
  toyIgRun(ins2, params = list(rescaleVars = FALSE))
  expect_false("Pinu_con" %in% names(toyCalls("ign1")[[1]]))
})
