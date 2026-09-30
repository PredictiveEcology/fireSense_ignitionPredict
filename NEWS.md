# fireSense_ignitionPredict 1.1.1

- reqdPkgs now lists `data.table` and `reproducible`, which the module calls (`:=`, `copy`, `set`, `setnames`, `rbindlist`, `as.data.table`, `postProcess`) but did not list. Version 1.1.1.

# fireSense_ignitionPredict 1.1.0

- Renamed from `fireSense_IgnitionPredict` to `fireSense_ignitionPredict` (module naming convention `<model>_<camelCaseComponent>`); projects must rename the module and its `params` key. The class `fireSense_IgnitionFit` and the objects `fireSense_IgnitionFitted`, `fireSense_IgnitionFittedList` and `fireSense_IgnitionPredicted` keep their names. Version 1.1.0.

- `ignitionsAndEscapes` gains `escaped`: whether each ignition escaped, with exactly `escapes` of a coarse
  pixel's ignitions TRUE. `escapes` is the coarse pixel's count, repeated on each of its ignitions, and fireSense
  spread `escapes` fires from every one of them, so a coarse pixel with 4 ignitions and 2 escapes gave 8 escaped
  fires instead of 2. Version 1.0.0.9003.

- New parameter `.studyAreaName` (default `NA`), the name PredictiveEcology modules use for the study area. This module does not use it yet.
- Several fitted ELFs in one study area: with `fireSense_IgnitionFittedList` and `fireSense_EscapeFittedList` (one fit per ELF, named by `ELFind`) and `rasterToMatchLargeELF`, each ELF's models predict the coarse pixels of that ELF, with its own `scaleData`. The Poisson and binomial draws are still made once over all pixels, so one ELF gives exactly the previous result.

# fireSense_IgnitionPredict (development version)

## Bug fixes

- Covariates are now standardized with the center and scale stored by the fit (`fireSense_IgnitionFitted$scaleData`, `fireSense_EscapeFitted$scaleData`). They were standardized with each year's own mean and sd, so every year looked average to the models. With `rescaleVars = TRUE`, a fitted object without `scaleData` is now an error.

## Cleanup

- The `save` event now does nothing and says so. It used to fail, because it wrote `sim$fireSense_IgnitionPredicted`, which the module no longer creates. `IgnitionPredictSave()` is removed.

# fireSense_IgnitionPredict 1.0.0

First release from `development` since `master` was last updated (2021-03-11). Full history: https://github.com/PredictiveEcology/fireSense_IgnitionPredict/compare/5a242fa...v1.0.0

## Breaking changes

- Removed input `dataFireSense_IgnitionPredict` (data.frame, RasterLayer, RasterStack).
- Removed output `fireSense_IgnitionPredicted`.
- Removed parameters: `data`, `mapping`, `modelObjName`, `rescaleFactor`.

## New features

- New inputs: `fireSense_EscapeFitted`, `fireSense_igAndEscapePred_Covariates`, `flammableRTM`.
- New outputs: `fireSense_IgAndEscapeProbRas`, `ignitionsAndEscapes`.
- New parameters: `modelAlgorithm`, `rescaleVars`.

## Dependencies

- No longer depends on `raster`.
- Now depends on `fireSenseUtils`, `terra`.

## Testing

- testthat suite and CI (`testthat-module`), including a snapshot of the module's inputs, outputs and parameters in `tests/testthat/test-metadata.R`.
