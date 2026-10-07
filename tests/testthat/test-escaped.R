## `escapes` is the coarse pixel's number of escapes, repeated on each of its ignitions. fireSense spread
## `escapes` fires from EVERY ignition row, so a coarse pixel with 4 ignitions and 2 escapes gave 8 escaped
## fires. `escaped` marks which ignitions escaped: exactly `escapes` of each coarse pixel's rows.

## the toy of test-run-values.R: two ignition folds, coarse cells 2 and 3 can ignite
twoFolds <- function(escFuns = list(constant(1)), ...) {
  toyIgInputs(list(byPixel(c(0, 2, 3, 0)), byPixel(c(0, 4, 3, 0))), escFuns, ...)
}

test_that("exactly `escapes` of each coarse pixel's ignitions escaped", {
  for (seed in 1:5) {
    ig <- igTable(toyIgRun(twoFolds(list(constant(0.5))), seed = seed))
    expect_type(ig$escaped, "logical")
    coarse <- coarseOfFine(ig$pixelID)
    per <- tapply(ig$escaped, coarse, sum)
    esc <- tapply(ig$escapes, coarse, `[`, 1)
    expect_identical(as.integer(per), as.integer(esc))
    expect_identical(sum(ig$escaped), sum(tapply(ig$escapes, coarse, `[`, 1)))
  }
})

test_that("every ignition escapes with probability 1, none with probability 0", {
  ig1 <- igTable(toyIgRun(twoFolds(list(constant(1)))))
  ig0 <- igTable(toyIgRun(twoFolds(list(constant(0)))))
  expect_identical(ig1$escaped, rep(TRUE, nrow(ig1)))
  expect_identical(ig0$escaped, rep(FALSE, nrow(ig0)))
})

test_that("the seed-42 placements are unchanged, and 5 of the 8 ignitions escape (2 + 3), not 20", {
  ig <- igTable(toyIgRun(twoFolds(list(constant(0.5))), seed = 42))
  ig <- ig[order(ig$pixelID), ]
  expect_identical(ig$pixelID, c(14L, 15L, 21L, 23L, 34L, 36L, 50L, 58L))
  expect_identical(sum(ig$escaped), 5L)
})
