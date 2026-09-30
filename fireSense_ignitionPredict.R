defineModule(sim, list(
  name = "fireSense_ignitionPredict",
  description = paste(
    "Predicts annual ignition and escape probabilities from the models fitted by",
    "fireSense_ignitionFit and fireSense_EscapeFit, and draws the pixels that ignite and escape."),
  keywords = c("fire frequency", "additive property", "poisson", "negative binomial", "fireSense"),
  authors = c(
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = "aut"),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(),
  version = list(SpaDES.core = "0.1.0", fireSense_ignitionPredict = "1.1.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_ignitionPredict.Rmd"),
  reqdPkgs = list(
    "data.table", "magrittr", "terra",
    "PredictiveEcology/reproducible@development",
    "PredictiveEcology/fireSenseUtils@development (>=0.1.0)"
  ),
  loadOrder = list(after = "fireSense_dataPrepPredict"),
  parameters = bindrows(
    defineParameter("modelAlgorithm", "character", "xgboost", NA, NA,
                    paste("Algorithm used to fit the models; only `xgboost` is supported.",
                          "Must agree with the value in the other fireSense modules.")),
    defineParameter("rescaleVars", "logical", default = TRUE,
                    desc = paste("Rescale the covariates before predicting? With `xgboost` they are standardized",
                                 "with `scale()`. Must agree with the value in the other fireSense modules.")),
    defineParameter(".runInitialTime", "numeric", start(sim), NA, NA,
                    desc = "Time of the first prediction."
    ),
    defineParameter(".runInterval", "numeric", 1, NA, NA,
                    desc = "Interval between predictions, in years. `NA` predicts once."
    ),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    desc = "Time of the `save` event, which does nothing. `NA` means never."
    ),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    desc = "If not `NA`, the ignition probability raster is plotted each year."
    ),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    desc = paste(
                      "Should this entire module be run with caching activated?",
                      "This is generally intended for data-type modules,",
                      "where stochasticity and time are not relevant"
                    )
    )
  ),
  inputObjects = bindrows(
    expectsInput("fireSense_EscapeFitted", "fireSense_EscapeFit",
                 desc = "Fitted escape models (`$modelList$model`, one per fold), from `fireSense_EscapeFit`.",
                 sourceURL = NA
    ),
    expectsInput("fireSense_IgnitionFittedList", "list",
                 desc = paste("Only with several fitted ELFs: one `fireSense_IgnitionFitted` per ELF, named by `ELFind`.",
                              "Each ELF's model predicts the coarse pixels of that ELF.")),
    expectsInput("fireSense_EscapeFittedList", "list",
                 desc = "Only with several fitted ELFs: one `fireSense_EscapeFitted` per ELF, named as `fireSense_IgnitionFittedList`."),
    expectsInput("rasterToMatchLargeELF", "SpatRaster",
                 desc = "Only with several fitted ELFs: each pixel's ELF (`ELFind`), from `fireSense_ELFs` with a `studyAreaLarge`."),
    expectsInput("fireSense_IgnitionFitted", "fireSense_IgnitionFit",
                 desc = paste("Fitted ignition models (`$modelList$model`, one per fold) and `$modelList$fittingRes`,",
                              "from `fireSense_ignitionFit`."),
                 sourceURL = NA
    ),
    expectsInput("fireSense_igAndEscapePred_Covariates", "data.table",
                 desc = paste(
                   "This year's covariates, from `fireSense_dataPrepPredict`.",
                   "`pixelID` is the cell index of `ignitionFitRTM`."
                 )
    ),
    expectsInput("flammableRTM", "SpatRaster",
                 sourceURL = NA,
                 desc = "Binary raster, 1 where the pixel is flammable."
    ),
  ),
  outputObjects = bindrows(
    createsOutput("fireSense_IgAndEscapeProbRas", "SpatRaster",
                  desc = paste("Two layers, `ignitionProb` (expected ignitions per pixel) and `escapeProb`,",
                               "at the resolution of `ignitionFitRTM`.")
    ),
    createsOutput(
      "ignitionsAndEscapes", "data.table",
      paste("One row per ignited pixel, in random order: `pixelID` (cell index of `flammableRTM`),",
            "`igProb`, `ignitions`, `escapeProb`, `escapes` of the coarse pixel it was drawn from, and",
            "`escaped`, whether this ignition escaped: exactly `escapes` of a coarse pixel's rows are TRUE.")
    )
  )
))

#' Event dispatcher
#'
#' Events: `init`, `run` (predict, repeated every `.runInterval`), `save` (does nothing).
#'
#' @param sim A `simList`.
#' @param eventTime Time of the event.
#' @param eventType Name of the event.
#' @param debug Not used.
#'
#' @return The `simList`, invisibly.
doEvent.fireSense_ignitionPredict <- function(sim, eventTime, eventType, debug = FALSE) {
  moduleName <- currentModule(sim)

  switch(eventType,
         init = {
           sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run")

           if (!is.na(P(sim)$.saveInitialTime)) {
             sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
           }
         },
         run = {
           sim <- IgnitionPredictRun(sim)
           if (!is.na(P(sim)$.runInterval)) {
             sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, moduleName, "run")
           }
         },
         save = {
           message("fireSense_ignitionPredict: the `save` event does nothing.")
         },
         warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                       "' in module '", current(sim)[1, "moduleName", with = FALSE], "'",
                       sep = ""
         ))
  )
  return(invisible(sim))
}

#' Predict and draw this year's ignitions and escapes
#'
#' Averages the per-fold predictions of the ignition models, draws ignitions per coarse pixel
#' (Poisson), then does the same for escapes (binomial, given ignition). Each ignition is placed in
#' a random flammable `flammableRTM` pixel inside its coarse pixel.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly, with `fireSense_IgAndEscapeProbRas` and `ignitionsAndEscapes`.
IgnitionPredictRun <- function(sim) {
  igCov <- copy(sim$fireSense_igAndEscapePred_Covariates)

  igCov <- na.omit(igCov)
  
  paramCheckOtherMods(sim, "modelAlgorithm")
  rescaleVars <- paramCheckOtherMods(sim, "rescaleVars")
  # The models were fitted on covariates standardized once, over all fitting years; a year's
  #   own mean and sd would make every year look average.
  ## The fitted models: one ignition and one escape fit, or one of each per fitted ELF (named by ELF), each
  ## applied to the coarse pixels of its own ELF (sim$rasterToMatchLargeELF). The Poisson and binomial draws
  ## are made once over all pixels, as before, so one ELF gives exactly the old result.
  fits <- ignitionFitsByELF(sim, igCov$pixelID)

  # From here, it makes predictions from each KFold, then averages them to get the probabilities for
  #   each cell; same for Escape, which is conditional on having ignited.
  predsIgns <- rep(NA_real_, NROW(igCov))
  for (f in fits) {
    covsHere <- igCov[f$rows]
    # The models were fitted on covariates standardized once, over all fitting years; a year's
    #   own mean and sd would make every year look average.
    if (rescaleVars) covsHere <- scaleAsFit(covsHere, f$ign$scaleData, "fireSense_IgnitionFitted")
    predsIgns[f$rows] <- foldMeanPrediction(f$ign, covsHere)
  }
  igns <- rpois(NROW(predsIgns), lambda = predsIgns) # can't use rtweedie because don't know the dispersion parameter

  whHasIgns <- which(igns > 0)
  predsEscs <- rep(NA_real_, NROW(igCov))
  for (f in fits) {
    rowsEsc <- intersect(whHasIgns, f$rows)
    covsHere <- igCov[rowsEsc]
    if (rescaleVars) covsHere <- scaleAsFit(covsHere, f$esc$scaleData, "fireSense_EscapeFitted")
    # the escape model is tweedie, so can go above 1, below 0 rarely
    predsEscs[rowsEsc] <- foldMeanPrediction(f$esc, covsHere, clamp01 = TRUE)
  }
  predsEscs <- predsEscs[whHasIgns]
  escs <- rbinom(NROW(predsEscs), size = igns[whHasIgns], prob = predsEscs)

  set(igCov, NULL, c("igProb", "ignitions"), list(predsIgns, igns))
  set(igCov, whHasIgns, c("escapeProb", "escapes"), list(predsEscs, escs))
  # Ignite - accounting for spatial resolution of models

  # disaggregate the coarse raster to size of flammableRTM
  # draw ignitions/escapes from smaller pixel size
  igRas <- rast(sim$ignitionFitRTM)
  igRas[igCov$pixelID] <- igCov$pixelID

  # can't use disagg because it may not be round pixels
  igRas <- postProcess(igRas, to = sim$flammableRTM, method = "near")
  igRas[sim$flammableRTM[] != 1] <- NA

  igRasDT <- as.data.table(igRas, cells = TRUE)
  setnames(igRasDT, new = c("pixelID", "chunkyPixels"))
  # nomatch = NULL --> removes pixels that aren't in igCov, which are non flammable
  igs <- igRasDT[igCov, on = c("chunkyPixels" = "pixelID"), nomatch = NULL]
  # randomly sample
  # use unique because of multiple pixelID per chunkyPixel
  drawTable <- unique(igs[ignitions > 0, .(chunkyPixels, ignitions, escapes)])
  pixelID_igs <- lapply(drawTable$chunkyPixels,
                        FUN = function(n, draw = drawTable, ig = igs) {
                          sampledChunk <- draw[chunkyPixels == n]
                          pixelID_pop <- igs[chunkyPixels == sampledChunk$chunkyPixels]
                          drawn <- sample(pixelID_pop$pixelID, size = sampledChunk$ignitions)
                          igPixels <- try(ig[pixelID %in% drawn, .(pixelID, igProb, ignitions, escapeProb, escapes,
                                                                   chunkyPixels)])
                          return(igPixels)
                        }
  ) |>
    rbindlist() #will this fail with zero ignitions?

  #for posterior analysis
  igProbRas <- rast(sim$ignitionFitRTM)
  igProbRas[igCov$pixelID] <- igCov$igProb
  escapeProbRas <- igProbRas
  escapeProbRas[igCov$pixelID] <- igCov$escapeProb
  sim$fireSense_IgAndEscapeProbRas <- c(igProbRas, escapeProbRas)
  names(sim$fireSense_IgAndEscapeProbRas) <- c("ignitionProb", "escapeProb")
  
  if (!is.na(P(sim)$.saveInterval)) {
    #TODO: implement proper plotting control
    Plots(igProbRas, types = Par$.plots, filename = paste0("IgnitionProbability_yr", time(sim)))
  }
  ## `escapes` counts the coarse pixel's escapes and is repeated on each of its ignitions, so which of
  ## them escaped is drawn here: exactly `escapes` of the coarse pixel's rows. Drawn after every pixel
  ## is placed, so the placements do not change.
  if (NROW(pixelID_igs)) {
    pixelID_igs[, escaped := seq_len(.N) %in% sample.int(.N, min(escapes[1], .N)), by = "chunkyPixels"]
    set(pixelID_igs, NULL, "chunkyPixels", NULL)
  }
  randomOrder <- sample(1:nrow(pixelID_igs))
  sim$ignitionsAndEscapes <- pixelID_igs[randomOrder,] #Randomize order

  return(invisible(sim))
}

#' Supply default inputs
#'
#' There are none.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
.inputObjects <- function(sim) {
  return(invisible(sim))
}


# Standardize `covariates` with the center and scale the fit used (`scaleData`, the attributes of
#   the `scale()`d fitting covariates), not with their own. `pixelID` is a cell number, left alone.
scaleAsFit <- function(covariates, scaleData, fittedName) {
  if (is.null(scaleData)) {
    stop("rescaleVars is TRUE but sim$", fittedName, "$scaleData is NULL; ",
         "the covariates cannot be standardized as they were for the fit.")
  }
  covariates <- copy(covariates)
  center <- scaleData[["scaled:center"]]
  scale <- scaleData[["scaled:scale"]]
  for (v in setdiff(intersect(names(center), colnames(covariates)), "pixelID")) {
    set(covariates, NULL, v, (covariates[[v]] - center[[v]]) / scale[[v]])
  }
  covariates
}

#' The fitted models, per ELF
#'
#' With `sim$fireSense_IgnitionFittedList` and `sim$fireSense_EscapeFittedList` (named by `ELFind`), each
#' coarse pixel is assigned to its ELF from `sim$rasterToMatchLargeELF` (the value at the pixel's centre)
#' and gets that ELF's models. Otherwise all pixels get `sim$fireSense_IgnitionFitted` and
#' `sim$fireSense_EscapeFitted`.
#'
#' @param sim A `simList`.
#' @param pixelID the cells of `sim$ignitionFitRTM` to predict.
#' @return list, one element per fit, each with `ign`, `esc` and `rows` (indices into `pixelID`).
ignitionFitsByELF <- function(sim, pixelID) {
  ignL <- sim$fireSense_IgnitionFittedList
  if (length(ignL) > 1L) {
    escL <- sim$fireSense_EscapeFittedList
    if (!setequal(names(ignL), names(escL)))
      stop("fireSense_ignitionPredict: fireSense_IgnitionFittedList and fireSense_EscapeFittedList must name ",
           "the same ELFs")
    if (is.null(sim$rasterToMatchLargeELF))
      stop("fireSense_ignitionPredict: several ignition fits need sim$rasterToMatchLargeELF to say which ELF ",
           "each pixel is in")
    elfCoarse <- postProcess(sim$rasterToMatchLargeELF, to = sim$ignitionFitRTM, method = "near")
    v <- terra::values(elfCoarse[[1]], mat = FALSE)[pixelID]
    lv <- terra::levels(elfCoarse[[1]])[[1]]
    elf <- if (is.data.frame(lv) && NCOL(lv) >= 2) as.character(lv[[2]][match(v, lv[[1]])]) else as.character(v)
    noELF <- sum(!elf %in% names(ignL))
    if (noELF > 0)
      warning("fireSense_ignitionPredict: ", noELF, " coarse pixels are in no ELF with an ignition fit; ",
              "they get no ignitions", call. = FALSE)
    return(lapply(names(ignL), function(id)
      list(ign = ignL[[id]], esc = escL[[id]], rows = which(elf == id))))
  }
  list(list(ign = sim$fireSense_IgnitionFitted, esc = sim$fireSense_EscapeFitted,
            rows = seq_along(pixelID)))
}

## Mean over the K-fold models of one fit's predictions
foldMeanPrediction <- function(fit, newdata, clamp01 = FALSE) {
  mods <- fit$modelList$model
  modsOnly <- mods[grep("Fold", names(mods))]
  if (is.null(modsOnly) || !length(modsOnly)) stop("Not tested anymore")
  preds <- lapply(modsOnly, function(model) {
    p <- predict(model, newdata = newdata)
    if (clamp01) pmax(pmin(1, p), 0) else p
  })
  rowMeans(do.call(cbind, preds))
}
