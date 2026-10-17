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
