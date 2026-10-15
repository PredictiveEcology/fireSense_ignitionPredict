test_that("a fitted object without a list of fold models stops", {
  ins <- toyIgInputs(list(constant(1)), list(constant(1)))
  ins$fireSense_IgnitionFitted$modelList$model <- NULL
  expect_error(toyIgRun(ins), "Not tested anymore")
})
