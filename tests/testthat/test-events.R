inputsNoIgnition <- function() toyIgInputs(list(constant(0)), list(constant(0)))

test_that("run repeats every .runInterval from .runInitialTime", {
  sim <- toyIgRun(inputsNoIgnition(), times = list(start = 2001, end = 2003))
  expect_equal(evOf(SpaDES.core::completed(sim), "run")$eventTime, 2001:2003)
  expect_equal(evOf(SpaDES.core::events(sim), "run")$eventTime, 2004)
  expect_equal(evOf(SpaDES.core::completed(sim), "init")$eventTime, 2001)
  ## one call to the ignition model per run event
  expect_length(toyCalls("ign1"), 3L)
})

test_that(".runInitialTime and .runInterval are honoured", {
  sim <- toyIgRun(inputsNoIgnition(), params = list(.runInitialTime = 2002, .runInterval = 2),
                  times = list(start = 2001, end = 2005))
  expect_equal(evOf(SpaDES.core::completed(sim), "run")$eventTime, c(2002, 2004))
  expect_equal(evOf(SpaDES.core::events(sim), "run")$eventTime, 2006)
})

test_that(".runInterval = NA predicts once", {
  sim <- toyIgRun(inputsNoIgnition(), params = list(.runInterval = NA_real_),
                  times = list(start = 2001, end = 2003))
  expect_equal(evOf(SpaDES.core::completed(sim), "run")$eventTime, 2001)
  expect_identical(nrow(evOf(SpaDES.core::events(sim), "run")), 0L)
})

test_that("no save event by default; .saveInitialTime schedules one, last in its year", {
  sim <- toyIgRun(inputsNoIgnition(), times = list(start = 2001, end = 2002))
  expect_identical(nrow(evOf(SpaDES.core::completed(sim), "save")), 0L)
  expect_identical(nrow(evOf(SpaDES.core::events(sim), "save")), 0L)
  sim <- toyIgRun(inputsNoIgnition(), params = list(.saveInitialTime = 2010),
                  times = list(start = 2001, end = 2001))
  sv <- evOf(SpaDES.core::events(sim), "save")
  expect_equal(sv$eventTime, 2010)
  expect_equal(sv$eventPriority, SpaDES.core::.last())
})

test_that("the save event says it does nothing, and changes nothing", {
  ins <- toyIgInputs(list(byPixel(c(0, 3, 0, 2))), list(constant(0.5)))
  without <- toyIgRun(ins, times = list(start = 2001, end = 2002))
  expect_message(
    with <- toyIgRun(ins, params = list(.saveInitialTime = 2001, .saveInterval = 1),
                     times = list(start = 2001, end = 2002)),
    "`save` event does nothing")
  ## it ran once and did not reschedule itself
  expect_equal(evOf(SpaDES.core::completed(with), "save")$eventTime, 2001)
  expect_identical(nrow(evOf(SpaDES.core::events(with), "save")), 0L)
  expect_identical(sort(ls(with)), sort(ls(without)))
  expect_identical(igTable(with), igTable(without))
  expect_identical(probVals(with), probVals(without))
  expect_identical(with$fireSense_IgnitionFitted, without$fireSense_IgnitionFitted)
  expect_length(list.files(SpaDES.core::outputPath(with), recursive = TRUE), 0L)
})

test_that("nothing is predicted before the first run event", {
  sim <- toyIgRun(inputsNoIgnition(), params = list(.runInitialTime = 2005),
                  times = list(start = 2001, end = 2002))
  expect_null(sim$ignitionsAndEscapes)
  expect_null(sim$fireSense_IgAndEscapeProbRas)
})
