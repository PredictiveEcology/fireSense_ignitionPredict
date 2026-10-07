## Toy inputs for the `run` event. A setup file rather than a helper, so that it shares an
## environment with `moduleName` and `testPaths` from setup.R.
##
## Tests run inside the namespace of the package rendition, which does not import
## data.table, so `dt[i, j]` there is NOT data.table-aware: tables are handled with base
## subsetting and data.table::set() only.
##
## The fitted models are stand-ins of class "toyFold". xgboost is not among the module's
## `reqdPkgs`, so CI does not have it; and a stand-in lets a test see exactly what the
## module hands to predict(). Each "model" is a function of `newdata`; every call is
## logged in `toyLog`.

## The run event calls postProcess() unqualified, but `reproducible` is not in the module's
## `reqdPkgs`: in a project some other module attaches it. It is always installed (SpaDES.core
## imports it), so attach it here; otherwise the event stops with
## 'could not find function "postProcess"'.
suppressPackageStartupMessages(library(reproducible))

toyLog <- new.env()
toyLogReset <- function() rm(list = ls(toyLog), envir = toyLog)

predict.toyFold <- function(object, newdata, ...) {
  key <- sprintf("%s_%03d", object$label, length(ls(toyLog)) + 1L)
  assign(key, as.data.frame(newdata), envir = toyLog)
  object$fun(as.data.frame(newdata))
}
registerS3method("predict", "toyFold", predict.toyFold)

toyFold <- function(fun, label) structure(list(fun = fun, label = label), class = "toyFold")

## calls logged for one model label, in call order
toyCalls <- function(label) {
  keys <- sort(grep(paste0("^", label, "_"), ls(toyLog), value = TRUE))
  lapply(keys, get, envir = toyLog)
}

## Grids, both 0..4 x 0..4. The fine (simulation) grid is 8 x 8 with 0.5-unit cells; the
## coarse (fitting) grid is 2 x 2 with 2-unit cells, so each coarse cell holds 16 fine cells:
## coarse 1 = fine rows 1-4, cols 1-4; coarse 2 = rows 1-4, cols 5-8;
## coarse 3 = rows 5-8, cols 1-4; coarse 4 = rows 5-8, cols 5-8.
## Fine cell number = (row - 1) * 8 + col.
coarseOfFine <- function(cell) {
  row <- (cell - 1) %/% 8 + 1
  col <- (cell - 1) %% 8 + 1
  (row > 4) * 2 + (col > 4) + 1
}

## By default fine cells 5 and 6 (in coarse 2) and 64 (in coarse 4) are not flammable.
toyFlammable <- function(nonFlammable = c(5L, 6L, 64L)) {
  r <- terra::rast(nrows = 8, ncols = 8, xmin = 0, xmax = 4, ymin = 0, ymax = 4, vals = 1,
                   crs = "EPSG:3005")
  r[nonFlammable] <- 0
  r
}

toyCoarse <- function() {
  terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 4, ymin = 0, ymax = 4, vals = 1,
              crs = "EPSG:3005")
}

## one row per coarse cell, as fireSense_dataPrepPredict makes them
toyIgCovariates <- function(MDC = c(100, 120, 140, 160), year = 2001) {
  data.table::data.table(pixelID = 1:4, MDC = MDC, youngAge = c(0, 0.25, 0.5, 1),
                         year = year)
}

## The centre and scale the fit standardised its covariates with, as fireSense_ignitionFit
## stores them (the attributes of a scale()d matrix; `pixelID` and `year` are in there too).
toyScaleData <- function(center = c(pixelID = 2.5, MDC = 100, youngAge = 0.5, year = 2000),
                         scale = c(pixelID = 1.3, MDC = 50, youngAge = 0.5, year = 10)) {
  list(`scaled:center` = center, `scaled:scale` = scale)
}

## `ignFuns` and `escFuns` are lists of functions of newdata, one per fold
toyIgInputs <- function(ignFuns, escFuns, covs = toyIgCovariates(),
                        foldNames = paste0("Fold", seq_along(ignFuns)),
                        flammableRTM = toyFlammable(),
                        ignScaleData = toyScaleData(), escScaleData = toyScaleData()) {
  ign <- Map(toyFold, ignFuns, paste0("ign", seq_along(ignFuns)))
  esc <- Map(toyFold, escFuns, paste0("esc", seq_along(escFuns)))
  names(ign) <- foldNames
  names(esc) <- paste0("Fold", seq_along(escFuns))
  list(
    fireSense_IgnitionFitted = structure(
      list(modelList = list(model = ign, fittingRes = 2), scaleData = ignScaleData),
      class = "fireSense_IgnitionFit"),
    fireSense_EscapeFitted = structure(
      list(modelList = list(model = esc), scaleData = escScaleData),
      class = "fireSense_EscapeFit"),
    fireSense_igAndEscapePred_Covariates = covs,
    flammableRTM = flammableRTM,
    ignitionFitRTM = toyCoarse()
  )
}

toyIgSim <- function(objects, params = list(), times = list(start = 2001, end = 2001)) {
  SpaDES.core::simInit(
    times = c(times, timeunit = "year"),
    modules = moduleName,
    params = stats::setNames(list(params), moduleName),
    objects = objects,
    paths = testPaths
  )
}

## fireSenseUtils::rescaleCovariates() warns on every call that there is no `ignitions`
## column to remove; that is noise here, so muffle that one warning only.
toyIgRun <- function(objects, ..., seed = 42) {
  toyLogReset()
  sim <- toyIgSim(objects, ...)
  set.seed(seed)
  withCallingHandlers(
    SpaDES.core::spades(sim, debug = FALSE),
    warning = function(w) {
      if (grepl("Tried to assign NULL to column 'ignitions'", conditionMessage(w)))
        invokeRestart("muffleWarning")
    })
}

igTable <- function(sim) as.data.frame(sim$ignitionsAndEscapes)
probVals <- function(sim) terra::values(sim$fireSense_IgAndEscapeProbRas)

## a constant prediction, whatever the covariates
constant <- function(x) function(nd) rep(x, nrow(nd))
## a prediction that depends on which coarse pixel it is: vals[pixelID]
byPixel <- function(vals) function(nd) vals[nd$pixelID]

evOf <- function(dt, type) {
  df <- as.data.frame(dt)
  df[df$moduleName == "fireSense_ignitionPredict" & df$eventType == type, , drop = FALSE]
}
