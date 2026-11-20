## Several fitted ELFs in one study area (the 2-ELF Mackenzie forecast, 2026-09). Each ELF's ignition and
## escape models, fitted in that ELF's own run, predict the coarse pixels of that ELF.
##
## Toy: ELF "A" is the left half of the grid (coarse cells 1 and 3), "B" the right half (2 and 4).
## A's ignition model predicts 2 everywhere it is asked, B's predicts 5.

fitPair <- function(ign, esc, tag) list(
  ign = structure(list(modelList = list(model = list(Fold1 = toyFold(ign, paste0("ign", tag))), fittingRes = 2),
                       scaleData = toyScaleData()), class = "fireSense_IgnitionFit"),
  esc = structure(list(modelList = list(model = list(Fold1 = toyFold(esc, paste0("esc", tag)))),
                       scaleData = toyScaleData()), class = "fireSense_EscapeFit"))

multiInputs <- function() {
  o <- toyIgInputs(list(constant(0)), list(constant(0)))      # the single fits are not used
  a <- fitPair(constant(2), constant(0.5), "A"); b <- fitPair(constant(5), constant(0.5), "B")
  o$fireSense_IgnitionFittedList <- list(A = a$ign, B = b$ign)
  o$fireSense_EscapeFittedList <- list(A = a$esc, B = b$esc)
  elf <- terra::rast(nrows = 8, ncols = 8, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:3005")
  terra::values(elf) <- rep(rep(1:2, each = 4), times = 8)    # fine cols 1-4 -> 1 (A), 5-8 -> 2 (B)
  levels(elf) <- data.frame(id = 1:2, ELFind = c("A", "B"))
  o$rasterToMatchLargeELF <- elf
  o
}

test_that("each ELF's ignition model predicts only its own coarse pixels", {
  sim <- toyIgRun(multiInputs())
  expect_equal(toyCalls("ignA")[[1]]$pixelID, c(1L, 3L))
  expect_equal(toyCalls("ignB")[[1]]$pixelID, c(2L, 4L))
  expect_equal(probVals(sim)[, "ignitionProb"], c(2, 5, 2, 5))
  ## escapes: the escape surface is complete, each ELF's escape model predicts its own pixels
  expect_equal(toyCalls("escA")[[1]]$pixelID, c(1L, 3L))
  expect_equal(toyCalls("escB")[[1]]$pixelID, c(2L, 4L))
  expect_equal(probVals(sim)[, "escapeProb"], c(0.5, 0.5, 0.5, 0.5))
  expect_length(toyCalls("ign1"), 0L)                         # the single fit is ignored
})

test_that("with several fits, a missing ELF raster or mismatched lists stop with a message", {
  o <- multiInputs(); o$rasterToMatchLargeELF <- NULL
  expect_error(toyIgRun(o), "rasterToMatchLargeELF")
  o <- multiInputs(); o$fireSense_EscapeFittedList <- o$fireSense_EscapeFittedList["A"]
  expect_error(toyIgRun(o), "same ELFs")
})

test_that("ELFs fitted with different objectives stop, naming the ELFs and objectives", {
  o <- multiInputs()
  attr(o$fireSense_IgnitionFittedList$B$modelList$model$Fold1, "params")$objective <- "reg:tweedie"
  expect_error(toyIgRun(o), "ELFs' ignition models have different objectives: A \\(count:poisson\\), B \\(reg:tweedie\\)")
  o <- multiInputs()
  attr(o$fireSense_EscapeFittedList$A$modelList$model$Fold1, "params")$objective <- "reg:squarederror"
  expect_error(toyIgRun(o), "ELFs' escape models have different objectives: A \\(reg:squarederror\\), B \\(count:poisson\\)")
})

test_that("with several ELFs sharing an objective, the distribution is that objective", {
  expect_identical(toyIgRun(multiInputs())$fireSense_IgAndEscapeDistribution,
                   list(ignition = "count:poisson", escape = "count:poisson"))
})

test_that("each ELF's pixels get that ELF's year-effect sd", {
  o <- multiInputs()
  o$fireSense_IgnitionFittedList$A$modelList$yearSD <- 0.4
  o$fireSense_EscapeFittedList$B$modelList$yearSD <- 1.1      # A's escape fit has none: 0
  sim <- toyIgRun(o)
  expect_equal(probVals(sim)[, "ignitionYearSD"], c(0.4, 0, 0.4, 0))
  expect_equal(probVals(sim)[, "escapeYearSD"], c(0, 1.1, 0, 1.1))
})

test_that("each ELF's pixels get that ELF's fitted year effect; an ELF without one is NA", {
  o <- multiInputs()
  o$fireSense_IgnitionFittedList$A$modelList$yearEffects <- data.frame(year = 2001, multiplier = 1.4, shape = 25)
  o$fireSense_EscapeFittedList$B$modelList$yearEffects <- data.frame(year = 2001, multiplier = 0.7)
  sim <- toyIgRun(o)
  expect_equal(probVals(sim)[, "ignitionYearEffect"], c(1.4, NA, 1.4, NA))
  expect_equal(probVals(sim)[, "escapeYearEffect"], c(NA, 0.7, NA, 0.7))
  expect_equal(probVals(sim)[, "ignitionYearEffectShape"], c(25, NA, 25, NA))
  expect_equal(probVals(sim)[, "escapeYearEffectShape"], rep(NA_real_, 4))   # B's fit has no shape column
})

test_that("a stored fit with features the covariates lack stops, naming its ELF, process and the features", {
  o <- multiInputs()
  o$fireSense_EscapeFittedList$B$modelList$model$Fold1$features <- c("MDC", "nonexistentCov")
  expect_error(toyIgRun(o), "escape fit of ELF B.*nonexistentCov.*doFit = TRUE.*ignitionFitFilename")
  o <- multiInputs()
  o$fireSense_IgnitionFittedList$A$modelList$model$Fold1$features <- "gone"
  expect_error(toyIgRun(o), "ignition fit of ELF A.*gone")
})

test_that("a stored fit whose features are all among the covariates predicts as before", {
  o <- multiInputs()
  o$fireSense_IgnitionFittedList$A$modelList$model$Fold1$features <- c("MDC", "youngAge")
  expect_equal(probVals(toyIgRun(o))[, "ignitionProb"], c(2, 5, 2, 5))
})

test_that("with one fit, a stored fit missing features stops, naming the fit", {
  o <- toyIgInputs(list(constant(1)), list(constant(1)))
  o$fireSense_IgnitionFitted$modelList$model$Fold1$features <- c("MDC", "nonexistentCov")
  expect_error(toyIgRun(o), "ignition fit of ELF \\(one fit\\).*nonexistentCov")
})

test_that("each ELF's fit applies its own year climate term, or none", {
  ## A: pixels 1, 3 (MDC 100, 140: mean 120); B: pixels 2, 4 (MDC 120, 160: mean 140)
  o <- multiInputs()
  o$fireSense_IgnitionFittedList$A$modelList$yearClimate <- list(variable = "MDC", center = 100, scale = 10, beta = 0.5, norm = 1)
  expect_equal(probVals(toyIgRun(o))[, "ignitionProb"], c(2 * exp(1), 5, 2 * exp(1), 5))     # B has none
  o$fireSense_IgnitionFittedList$B$modelList$yearClimate <- list(variable = "MDC", center = 140, scale = 10, beta = 1, norm = 2)
  expect_equal(probVals(toyIgRun(o))[, "ignitionProb"], c(2 * exp(1), 5 / 2, 2 * exp(1), 5 / 2))
})

test_that("with several ELFs the year mean is of each ELF's pixels, NA rows included", {
  o <- multiInputs()
  ## pixels 1, 3 are ELF A; 2, 4 are B. MDC 100, 140 (A) and 120, 160 (B); an NA in an unrelated column of pixel 3
  o$fireSense_igAndEscapePred_Covariates <- toyIgCovariates(MDC = c(100, 120, 140, 160))
  data.table::set(o$fireSense_igAndEscapePred_Covariates, NULL, "lightningDays", c(1, 1, NA, 1))
  o$fireSense_IgnitionFittedList$A$modelList$yearClimate <- list(variable = "MDC", center = 100, scale = 10, beta = 0.5, norm = 1)
  sim <- toyIgRun(o)
  ## A's mean is over pixels 1 and 3 (120), not pixel 1 only (100): z = 2; pixel 3 is dropped (NA), pixel 1 predicted 2
  expect_equal(unname(probVals(sim)[1, "ignitionProb"]), 2 * exp(0.5 * 2))
})


test_that("the ELF assignment (raster post-process) runs once per prediction, with or without a year climate term", {
  calls <- function(o) {
    n <- 0L
    orig <- reproducible::postProcess
    testthat::local_mocked_bindings(postProcess = function(...) { n <<- n + 1L; orig(...) }, .package = "reproducible")
    toyIgRun(o)
    n
  }
  o <- multiInputs()
  expect_identical(calls(o), 1L)
  o$fireSense_IgnitionFittedList$A$modelList$yearClimate <- list(variable = "MDC", center = 100, scale = 10, beta = 0.5, norm = 1)
  expect_identical(calls(o), 1L)
})
