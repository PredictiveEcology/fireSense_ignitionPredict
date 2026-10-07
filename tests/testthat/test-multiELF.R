## Several fitted ELFs in one study area (the 2-ELF Mackenzie forecast, 2026-09). Each ELF's ignition and
## escape models, fitted in that ELF's own run, predict the coarse pixels of that ELF. Poisson and binomial
## draws are still made once over all pixels.
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
  elf <- toyFlammable(integer(0))
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
  ## escapes: each ELF's escape model sees only its own ignited pixels
  expect_true(all(toyCalls("escA")[[1]]$pixelID %in% c(1L, 3L)))
  expect_true(all(toyCalls("escB")[[1]]$pixelID %in% c(2L, 4L)))
  expect_length(toyCalls("ign1"), 0L)                         # the single fit is ignored
})

test_that("with several fits, a missing ELF raster or mismatched lists stop with a message", {
  o <- multiInputs(); o$rasterToMatchLargeELF <- NULL
  expect_error(toyIgRun(o), "rasterToMatchLargeELF")
  o <- multiInputs(); o$fireSense_EscapeFittedList <- o$fireSense_EscapeFittedList["A"]
  expect_error(toyIgRun(o), "same ELFs")
})
