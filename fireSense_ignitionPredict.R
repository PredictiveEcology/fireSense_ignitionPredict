defineModule(sim, list(
  name = "fireSense_ignitionPredict",
  description = paste(
    "Predicts the expected number of ignitions and of escapes per coarse pixel and year from the models fitted by",
    "fireSense_ignitionFit and fireSense_EscapeFit. It draws nothing: fireSense_burn draws the ignitions and escapes."),
  keywords = c("fire frequency", "additive property", "poisson", "negative binomial", "fireSense"),
  authors = c(
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = "aut"),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(),
  version = list(SpaDES.core = "0.1.0", fireSense_ignitionPredict = "1.2.0.9007"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_ignitionPredict.Rmd"),
  reqdPkgs = list(
    "data.table", "magrittr", "terra", "xgboost (>= 3.0.0)",
    "PredictiveEcology/SpaDES.core@development (>= 3.0.4)",
    "PredictiveEcology/reproducible@development",
    "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9168)" # 9168: foldModels(), foldMeanPrediction()
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
                 desc = "Only with several fitted ELFs: each pixel's ELF (`ELFind`), from `fireSense_ELFs` with a `studyArea_fireSense`."),
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
    expectsInput("ignitionFitRTM", "SpatRaster",
                 desc = paste("The coarse grid of `fireSense_igAndEscapePred_Covariates`: from `fireSense_dataPrepFit`,",
                              "else `fireSense_dataPrepPredict`. The probability rasters are made on it.")),
  ),
  outputObjects = bindrows(
    createsOutput("fireSense_IgAndEscapeProbRas", "SpatRaster",
                  desc = paste("Two layers at the resolution of `ignitionFitRTM`, for every pixel with covariates:",
                               "`ignitionProb`, the expected number of ignitions per pixel and year, and `escapeProb`,",
                               "the expected number of escapes per pixel and year, given at least one ignition, times the",
                               "escape fit's calibration (`modelList$escapeCalibration`; 1 if the fit has none).",
                               "Both are means of the fitted distributions in `fireSense_IgAndEscapeDistribution`,",
                               "not probabilities. And two more, `ignitionYearSD` and `escapeYearSD`: the sd of the year",
                               "effect of each pixel's ELF's ignition and escape fit (`modelList$yearSD`, 0 for a fit without",
                               "one), with which `fireSense_burn` scales both means each year. And, only when a fit has a",
                               "fitted effect for the year `floor(time(sim))` (`modelList$yearEffects` of fireSense_ignitionFit),",
                               "`ignitionYearEffect` and `escapeYearEffect`: that multiplier (`NA` in an ELF without one), and",
                               "`ignitionYearEffectShape` and `escapeYearEffectShape`: the shape of its gamma posterior (`NA`: no",
                               "uncertainty), which `fireSense_burn` uses instead of the draw with `yearEffects = \"fitted\"`.")
    ),
    createsOutput("fireSense_IgAndEscapeDistribution", "list",
                  desc = paste("`list(ignition = , escape = )`: the objective of the fitted models (e.g. `\"count:poisson\"`),",
                               "which says how `fireSense_burn` draws from the means in `fireSense_IgAndEscapeProbRas`.")
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

#' Predict this year's expected ignitions and escapes
#'
#' Averages the per-fold predictions of the ignition models and of the escape models, for every coarse
#' pixel with covariates (`fireSenseUtils::foldMeanPrediction()`; the escape mean times the escape fit's calibration,
#' `escapeCalibrationOf()`), and gives each pixel its fits' year-effect sd
#' (`yearSDOf()`) and, when they have one, their fitted effect of this year (`yearEffectOf()`). Nothing is
#' drawn: `fireSense_burn` draws ignitions and escapes from these means,
#' with the distribution named in `fireSense_IgAndEscapeDistribution`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly, with `fireSense_IgAndEscapeProbRas` and `fireSense_IgAndEscapeDistribution`.
IgnitionPredictRun <- function(sim) {
  igCov <- copy(sim$fireSense_igAndEscapePred_Covariates)

  igCov <- na.omit(igCov)
  
  paramCheckOtherMods(sim, "modelAlgorithm")
  rescaleVars <- paramCheckOtherMods(sim, "rescaleVars")
  # The models were fitted on covariates standardized once, over all fitting years; a year's
  #   own mean and sd would make every year look average.
  ## The fitted models: one ignition and one escape fit, or one of each per fitted ELF (named by ELF), each
  ## applied to the coarse pixels of its own ELF (sim$rasterToMatchLargeELF).
  fits <- ignitionFitsByELF(sim, igCov$pixelID)
  sim$fireSense_IgAndEscapeDistribution <- list(ignition = fitObjective(fits, "ign"),
                                                escape = fitObjective(fits, "esc"))

  # From here, it makes predictions from each KFold, then averages them to get the expected counts for
  #   each cell; same for Escape (expected escapes given an ignition), for every cell.
  predsIgns <- predsEscs <- sdIgns <- sdEscs <- effIgns <- effEscs <- shapeIgns <- shapeEscs <- rep(NA_real_, NROW(igCov))
  year <- floor(time(sim) + 1e-9)  # a hindcast's reps are decimal years
  for (f in fits) {
    covsHere <- igCov[f$rows]
    # The models were fitted on covariates standardized once, over all fitting years; a year's
    #   own mean and sd would make every year look average.
    covsIgn <- if (rescaleVars) scaleAsFit(covsHere, f$ign$scaleData, "fireSense_IgnitionFitted") else covsHere
    covsEsc <- if (rescaleVars) scaleAsFit(covsHere, f$esc$scaleData, "fireSense_EscapeFitted") else covsHere
    predsIgns[f$rows] <- fireSenseUtils::foldMeanPrediction(f$ign, covsIgn)
    predsEscs[f$rows] <- fireSenseUtils::foldMeanPrediction(f$esc, covsEsc) * escapeCalibrationOf(f$esc)
    sdIgns[f$rows] <- yearSDOf(f$ign)
    sdEscs[f$rows] <- yearSDOf(f$esc)
    effIgns[f$rows] <- yearEffectOf(f$ign, year)
    effEscs[f$rows] <- yearEffectOf(f$esc, year)
    shapeIgns[f$rows] <- yearEffectOf(f$ign, year, "shape")
    shapeEscs[f$rows] <- yearEffectOf(f$esc, year, "shape")
  }

  #for posterior analysis, and for fireSense_burn to draw from
  igProbRas <- rast(sim$ignitionFitRTM)
  igProbRas[igCov$pixelID] <- predsIgns
  escapeProbRas <- igProbRas
  escapeProbRas[igCov$pixelID] <- predsEscs
  sdIgnRas <- sdEscRas <- igProbRas
  sdIgnRas[igCov$pixelID] <- sdIgns
  sdEscRas[igCov$pixelID] <- sdEscs
  sim$fireSense_IgAndEscapeProbRas <- c(igProbRas, escapeProbRas, sdIgnRas, sdEscRas)
  names(sim$fireSense_IgAndEscapeProbRas) <- c("ignitionProb", "escapeProb", "ignitionYearSD", "escapeYearSD")
  ## the fitted effects of this year, only when a fit has one
  if (!all(is.na(c(effIgns, effEscs)))) {
    effLayers <- lapply(list(effIgns, effEscs, shapeIgns, shapeEscs), function(v) {
      r <- rast(sim$ignitionFitRTM)
      r[igCov$pixelID] <- v
      r
    })
    sim$fireSense_IgAndEscapeProbRas <- c(sim$fireSense_IgAndEscapeProbRas, rast(effLayers))
    names(sim$fireSense_IgAndEscapeProbRas)[5:8] <- c("ignitionYearEffect", "escapeYearEffect",
                                                      "ignitionYearEffectShape", "escapeYearEffectShape")
  }
  
  if (!is.na(P(sim)$.saveInterval)) {
    #TODO: implement proper plotting control
    Plots(igProbRas, types = Par$.plots, filename = paste0("IgnitionProbability_yr", time(sim)))
  }

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
#' @return list, one element per fit, each with `ign`, `esc`, `rows` (indices into `pixelID`) and `elf` (its name).
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
      list(ign = ignL[[id]], esc = escL[[id]], rows = which(elf == id), elf = id)))
  }
  list(list(ign = sim$fireSense_IgnitionFitted, esc = sim$fireSense_EscapeFitted,
            rows = seq_along(pixelID), elf = "(one fit)"))
}

#' The sd of a fit's year effect
#'
#' `fireSense_ignitionFit` stores it as `modelList$yearSD`; a fit stored before that has none, which is 0:
#' no year effect, as before.
#'
#' @param fit A `fireSense_IgnitionFitted` or `fireSense_EscapeFitted`.
#' @return One number.
yearSDOf <- function(fit) {
  sd <- fit$modelList$yearSD
  if (is.null(sd)) 0 else sd
}

#' The calibration of a fit's escape mean
#'
#' `fireSense_ignitionFit` stores, with both processes fitted, `modelList$escapeCalibration` in the escape fit: the
#' factor `c` at which the ignition model's own ignitions expect the observed escapes (`fittedYearEffects()`). The
#' escape model is fitted on the observed ignitions, but `fireSense_burn` draws ignitions where the ignition model
#' puts them. `c` multiplies the escape mean here, so `fireSense_burn`'s chance that an ignition escapes,
#' `fireSenseUtils::escapeProbPerIgnition(c * escapeMean, ignitionMean)`, is capped at 1 after it (`c` is applied
#' before the cap: where the cap binds, `c` adds nothing). A fit stored before it has none, which is 1.
#'
#' @param fit A `fireSense_EscapeFitted`.
#' @return One number.
escapeCalibrationOf <- function(fit) {
  cal <- fit$modelList$escapeCalibration
  if (is.null(cal)) 1 else cal
}

#' A fit's fitted year effect of one year
#'
#' `fireSense_ignitionFit` stores, with both processes fitted, `modelList$yearEffects`: one row per fit year,
#' `year` and `multiplier`, the effect a hindcast of that year may use instead of a draw (`fireSense_burn`'s
#' `yearEffects = "fitted"`).
#'
#' @param fit A `fireSense_IgnitionFitted` or `fireSense_EscapeFitted`.
#' @param year The fire year, e.g. `floor(time(sim))`.
#' @param column `"multiplier"` (the posterior mean) or `"shape"` (of its gamma posterior, which fireSense_burn
#'   draws each rep's multiplier from).
#' @return One number; `NA` for a year the fit has no effect for, or a fit stored without them.
yearEffectOf <- function(fit, year, column = "multiplier") {
  ye <- fit$modelList$yearEffects
  if (is.null(ye) || is.null(ye[[column]])) NA_real_ else ye[[column]][match(year, ye$year)]
}

#' The objective the fitted models were fitted with
#'
#' `fireSense_burn` draws from the predicted means with the distribution this implies, so every fold of
#' every ELF must have been fitted with the same one.
#'
#' @param fits The result of `ignitionFitsByELF()`.
#' @param process `"ign"` (ignition) or `"esc"` (escape).
#'
#' @return The objective, e.g. `"count:poisson"`.
fitObjective <- function(fits, process) {
  what <- c(ign = "ignition", esc = "escape")[[process]]
  perELF <- lapply(fits, function(f) {
    obj <- fireSenseUtils::xgbObjective(fireSenseUtils::foldModels(f[[process]]))
    if (anyNA(obj))
      stop("fireSense_ignitionPredict: cannot read the objective of a fitted ", what,
           " model (`fireSenseUtils::xgbObjective()` found none); fireSense_burn needs it to draw from the predictions")
    if (length(unique(obj)) > 1L)
      stop("fireSense_ignitionPredict: the folds of the ", what, " model of ELF ", f$elf,
           " have different objectives: ", paste(unique(obj), collapse = ", "))
    obj[[1L]]
  })
  if (length(unique(unlist(perELF))) > 1L)
    stop("fireSense_ignitionPredict: the ELFs' ", what, " models have different objectives: ",
         paste0(vapply(fits, `[[`, "", "elf"), " (", unlist(perELF), ")", collapse = ", "))
  perELF[[1L]]
}
