---
title: "fireSense_ignitionPredict Manual"
subtitle: "v.1.1.1"
date: "Last updated: 2026-09-30"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
bibliography: citations/references_fireSense_ignitionPredict.bib
link-citations: true
always_allow_html: true
---

# fireSense_ignitionPredict Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:fireSense-ignitionPredict) *fireSense_ignitionPredict*



[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut, cre], Ian Eddy <ian.eddy@nrcan-rncan.gc.ca> [aut], Jean Marchal <jean.d.marchal@gmail.com> [aut], Alex M Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Each year, predicts ignitions and escapes from the models fitted by *fireSense_ignitionFit* and *fireSense_EscapeFit*, for the ignition component of fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019].

1. The covariates in `fireSense_igAndEscapePred_Covariates` are rescaled with `fireSenseUtils::prepareCovariatesOuter()`, the function used for fitting (`rescaleVars`, `modelAlgorithm`).
2. Expected ignitions per coarse pixel are the mean of the predictions of the per-fold ignition models; the number of ignitions is drawn from a Poisson.
3. For coarse pixels with ignitions, the escape probability is the mean of the per-fold escape models, clamped to [0, 1]; escapes are drawn from a binomial with size = ignitions.
4. Each ignition is placed in a randomly chosen flammable pixel of `flammableRTM` inside its coarse pixel.

Only `xgboost` models are supported.

### Module inputs and parameters

`ignitionFitRTM` (the coarse raster used for fitting, from *fireSense_dataPrepFit*) is also read from the `simList`, though it is not declared as an input.
`modelAlgorithm` and `rescaleVars` must have the same value in every module that defines them.

Table \@ref(tab:moduleInputs-fireSense-ignitionPredict) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-fireSense-ignitionPredict)(\#tab:moduleInputs-fireSense-ignitionPredict)List of (ref:fireSense-ignitionPredict) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fireSense_EscapeFitted </td>
   <td style="text-align:left;"> fireSense_EscapeFit </td>
   <td style="text-align:left;"> Fitted escape models (`$modelList$model`, one per fold), from `fireSense_EscapeFit`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_IgnitionFittedList </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Only with several fitted ELFs: one `fireSense_IgnitionFitted` per ELF, named by `ELFind`. Each ELF's model predicts the coarse pixels of that ELF. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_EscapeFittedList </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> Only with several fitted ELFs: one `fireSense_EscapeFitted` per ELF, named as `fireSense_IgnitionFittedList`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatchLargeELF </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Only with several fitted ELFs: each pixel's ELF (`ELFind`), from `fireSense_ELFs` with a `studyAreaLarge`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_IgnitionFitted </td>
   <td style="text-align:left;"> fireSense_IgnitionFit </td>
   <td style="text-align:left;"> Fitted ignition models (`$modelList$model`, one per fold) and `$modelList$fittingRes`, from `fireSense_ignitionFit`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_igAndEscapePred_Covariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> This year's covariates, from `fireSense_dataPrepPredict`. `pixelID` is the cell index of `ignitionFitRTM`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammableRTM </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Binary raster, 1 where the pixel is flammable. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-ignitionPredict))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-fireSense-ignitionPredict)(\#tab:moduleParams-fireSense-ignitionPredict)List of (ref:fireSense-ignitionPredict) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> modelAlgorithm </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> xgboost </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Algorithm used to fit the models; only `xgboost` is supported. Must agree with the value in the other fireSense modules. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rescaleVars </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Rescale the covariates before predicting? With `xgboost` they are standardized with `scale()`. Must agree with the value in the other fireSense modules. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Time of the first prediction. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Interval between predictions, in years. `NA` predicts once. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Time of the `save` event, which does nothing. `NA` means never. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> If not `NA`, the ignition probability raster is plotted each year. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant </td>
  </tr>
</tbody>
</table>

### Events

- `init`: schedules `run` at `.runInitialTime`, and `save` at `.saveInitialTime` if that is not `NA`.
- `run`: makes the predictions and draws described above; repeats every `.runInterval`.
- `save`: does nothing but say so. To save the predicted raster, name `fireSense_IgAndEscapeProbRas` in `outputs(sim)`.

### Plotting

If `.saveInterval` is not `NA`, the ignition probability raster is plotted each year with `Plots()`.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-fireSense-ignitionPredict)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-fireSense-ignitionPredict)(\#tab:moduleOutputs-fireSense-ignitionPredict)List of (ref:fireSense-ignitionPredict) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fireSense_IgAndEscapeProbRas </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Two layers, `ignitionProb` (expected ignitions per pixel) and `escapeProb`, at the resolution of `ignitionFitRTM`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ignitionsAndEscapes </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> One row per ignited pixel, in random order: `pixelID` (cell index of `flammableRTM`), `igProb`, `ignitions`, `escapeProb`, `escapes` of the coarse pixel it was drawn from, and `escaped`, whether this ignition escaped: exactly `escapes` of a coarse pixel's rows are TRUE. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Runs after *fireSense_dataPrepPredict*, which supplies the covariates. `ignitionsAndEscapes` is used by *fireSense* to start fires.
It is normally run as part of the [fireSense](https://github.com/PredictiveEcology/fireSense) module group.

### Getting help

- <https://github.com/PredictiveEcology/fireSense_ignitionPredict/issues>

## References

<!-- autogenerated from bibligraphy -->
