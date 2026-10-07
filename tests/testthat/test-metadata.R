## The module's metadata is its public contract: a project using this module binds
## to these object names and classes. Renaming or retyping one breaks every caller,
## which is exactly the class of change the raster -> terra migration makes, so it is
## worth asserting here rather than discovering downstream.
##
## When a change is deliberate, update this file in the same commit and bump the
## module version to match: removed, renamed or retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  expect_identical(
    inputs[order(names(inputs))],
    c(fireSense_EscapeFitted               = "fireSense_EscapeFit",
      fireSense_EscapeFittedList           = "list",
      fireSense_igAndEscapePred_Covariates = "data.table",
      fireSense_IgnitionFitted             = "fireSense_IgnitionFit",
      fireSense_IgnitionFittedList         = "list",
      flammableRTM                         = "SpatRaster",
      rasterToMatchLargeELF                = "SpatRaster")
  )
})

test_that("outputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outputs <- stats::setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  expect_identical(
    outputs[order(names(outputs))],
    c(fireSense_IgAndEscapeProbRas = "SpatRaster",
      ignitionsAndEscapes          = "data.table")
  )
})

## deparsed defaults, so that a changed default fails as loudly as a renamed parameter
paramTable <- function(md) {
  p <- md$parameters
  out <- data.frame(
    class = as.character(unlist(p$paramClass)),
    default = vapply(p$default, function(d) paste(deparse(as.vector(d)), collapse = ""), ""),
    row.names = p$paramName
  )
  out[order(rownames(out), method = "radix"), ] # C order, whatever the locale
}

test_that("parameters have the expected names, classes and defaults", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expected <- data.frame(
    class   = c("numeric", "numeric", "numeric", "numeric", "character", "logical", "character", "logical"),
    ## .runInitialTime defaults to start(sim), which is 0 when only metadata is parsed
    default = c("0", "1", "NA", "NA", "NA", "FALSE", "\"xgboost\"", "TRUE"),
    row.names = c(".runInitialTime", ".runInterval", ".saveInitialTime", ".saveInterval", ".studyAreaName", ".useCache",
                  "modelAlgorithm", "rescaleVars")
  )
  expect_identical(paramTable(md), expected)
})

test_that("fireSenseUtils is a declared dependency, so CI installs it", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_true(any(grepl("^PredictiveEcology/fireSenseUtils@development", unlist(md$reqdPkgs))))
  expect_identical(md$timeunit, "year")
})
