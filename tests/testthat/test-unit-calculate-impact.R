test_args <- list(con = NULL,
                  method = "calendar_year",
                  touchstone = "t",
                  modelling_group = "IC:Garske",
                  disease = "HepB",
                  focal_scenario_type = "default",
                  baseline_scenario_type = "novac",
                  focal_vaccine_delivery = list(
                    list(
                      vaccine = "HepB",
                      activity_type = "campaign"
                    ),
                    list(
                      vaccine = "HepB_BD",
                      activity_type = "campaign"
                    )
                  ),
                  baseline_vaccine_delivery = list(
                    list(
                      vaccine = "HepB",
                      activity_type = "routine"
                    ),
                    list(
                      vaccine = "HepB_BD",
                      activity_type = "routine"
                    )
                  ),
                  burden_outcomes = "deaths",
                  countries = "AFG",
                  is_under5 = FALSE,
                  vaccination_years = 2000:2010)

test_outputs <- list(touchstone = "t-1",
                     burden_estimate_set_ids = c(10, 11),
                     burden_outcome_ids = 2,
                     baseline = data.frame(country = "AFG",
                                           year = 2020,
                                           age = 5,
                                           burden_outcomes = "deaths",
                                           scenario = "baseline",
                                           activity_type = "routine",
                                           value = 1000, stringsAsFactors = FALSE),
                     focal = data.frame(country = "AFG",
                                        year = 2020,
                                        age = 5,
                                        burden_outcomes = "deaths",
                                        scenario = "focal",
                                        activity_type = "campaign",
                                        value = 1000, stringsAsFactors = FALSE))

mock_get_touchstone_id <- function(con, t) {
  if (t == test_args$touchstone) {
    "t-1"
  } else {
    stop("Unexpected args to get_touchstone_id")
  }
}

mock_get_burden_outcome_ids <- function(con, b) {
  if (b == test_args$burden_outcomes) {
    test_outputs$burden_outcome_ids
  } else {
    stop("Unexpected args to get_burden_outcome_ids")
  }
}

mock_get_burden_estimate_set_ids <- function(con, baseline_scenario_type, baseline_scenario,
                                             focal_scenario_type, focal_scenario,
                                             touchstone, modelling_group, disease) {
  if (baseline_scenario_type != test_args$baseline_scenario_type) {
    stop("Unexpected baseline_scenario_type")
  }
  if (baseline_scenario != "novac-HepB-routine;novac-HepB_BD-routine") {
    stop(paste("Unexpected baseline_scenario", baseline_scenario))
  }
  if (focal_scenario_type != test_args$focal_scenario_type) {
    stop("Unexpected focal_scenario_type")
  }
  if (focal_scenario != "default-HepB-campaign;default-HepB_BD-campaign") {
    stop("Unexpected focal_scenario")
  }
  if (touchstone != test_outputs$touchstone) {
    stop("Unexpected touchstone")
  }
  if (modelling_group != test_args$modelling_group) {
    stop("Unexpected modelling_group")
  }
  if (disease != test_args$disease) {
    stop("Unexpected disease")
  }
  test_outputs$burden_estimate_set_ids
}

mock_get_impact_for_burden_estimate_set <- function(con, burden_estimate_sets, outcomes, countries, is_under5) {
  if (!all(burden_estimate_sets == test_outputs$burden_estimate_set_ids)) {
    stop("Unexpected burden_estimate_sets")
  }
  if (outcomes != test_outputs$burden_outcome_ids) {
    stop("Unexpected outcomes")
  }
  if (countries != test_args$countries) {
    stop("Unexpected countries")
  }
  data.frame(country = "AFG",
             year = 2020,
             age = 5,
             burden_outcomes = "deaths",
             scenario = c("focal", "baseline"),
             activity_type = c("campaign", "routine"),
             value = 1000)
}

mock_impact_by_calendar_year <- function(baseline, focal) {
  if (baseline$activity_type != "routine") {
    stop("Unexpected baseline")
  }
  if (focal$activity_type != "campaign") {
    stop("Unexpected focal")
  }
  "success"
}

test_that("calculate_impact fails if method is not recognised", {
  args <- test_args
  args$method <- "badmethod"
  expect_error(do.call(calculate_impact, args),
               "'method' must be one of calendar_year, birth_year, yov_activity_type, yov_birth_cohort got badmethod")
})

test_that("calculate_impact retrieves touchstone id if name given", {
  mock <- mockery::mock(test_outputs$touchstone)
  mockery::stub(calculate_impact, "get_touchstone_id", mock)
  mockery::stub(calculate_impact, "get_burden_outcome_ids", mock_get_burden_outcome_ids)
  mockery::stub(calculate_impact, "get_burden_estimate_set_ids", mock_get_burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_impact_for_burden_estimate_set", mock_get_impact_for_burden_estimate_set)
  mockery::stub(calculate_impact, "impact_by_calendar_year", mock_impact_by_calendar_year)

  expect_equal(do.call(calculate_impact, test_args), "success")
  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)
  expect_equal(as.character(args[[1]][[2]]), test_args$touchstone)
})

test_that("calculate_impact gets burden outcome ids from burden outcomes", {
  mock <- mockery::mock(test_outputs$burden_outcome_ids)
  mockery::stub(calculate_impact, "get_touchstone_id", mock_get_touchstone_id)
  mockery::stub(calculate_impact, "get_burden_outcome_ids", mock)
  mockery::stub(calculate_impact, "get_burden_estimate_set_ids", mock_get_burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_impact_for_burden_estimate_set", mock_get_impact_for_burden_estimate_set)
  mockery::stub(calculate_impact, "impact_by_calendar_year", mock_impact_by_calendar_year)

  expect_equal(do.call(calculate_impact, test_args), "success")
  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)
  expect_equal(as.character(args[[1]][[2]]), test_args$burden_outcomes)
})

test_that("calculate_impact gets burden estimate set ids", {
  mock <- mockery::mock(test_outputs$burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_touchstone_id", mock_get_touchstone_id)
  mockery::stub(calculate_impact, "get_burden_outcome_ids", mock_get_burden_outcome_ids)
  mockery::stub(calculate_impact, "get_burden_estimate_set_ids", mock)
  mockery::stub(calculate_impact, "get_impact_for_burden_estimate_set", mock_get_impact_for_burden_estimate_set)
  mockery::stub(calculate_impact, "impact_by_calendar_year", mock_impact_by_calendar_year)

  expect_equal(do.call(calculate_impact, test_args), "success")
  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)
  expect_equal(as.character(args[[1]][[2]]), test_args$baseline_scenario_type)
  expect_equal(as.character(args[[1]][[3]]), "novac-HepB-routine;novac-HepB_BD-routine")
  expect_equal(as.character(args[[1]][[4]]), test_args$focal_scenario_type)
  expect_equal(as.character(args[[1]][[5]]), "default-HepB-campaign;default-HepB_BD-campaign")
  expect_equal(as.character(args[[1]][[6]]), test_outputs$touchstone)
  expect_equal(as.character(args[[1]][[7]]), test_args$modelling_group)
  expect_equal(as.character(args[[1]][[8]]), test_args$disease)

})

test_that("calculate_impact gets impact by calendar year when method is calendar_year", {
  mock <- mockery::mock(test_outputs$burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_touchstone_id", mock_get_touchstone_id)
  mockery::stub(calculate_impact, "get_burden_outcome_ids", mock_get_burden_outcome_ids)
  mockery::stub(calculate_impact, "get_burden_estimate_set_ids", mock_get_burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_impact_for_burden_estimate_set", mock_get_impact_for_burden_estimate_set)
  mockery::stub(calculate_impact, "impact_by_calendar_year", mock)

  do.call(calculate_impact, test_args)

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)
  expect_equal(as.character(args[[1]][[1]]$scenario), "baseline")
  expect_equal(as.character(args[[1]][[2]]$scenario), "focal")
})

test_that("calculate_impact gets impact by birth year when method is birth_year", {
  mock <- mockery::mock(test_outputs$burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_touchstone_id", mock_get_touchstone_id)
  mockery::stub(calculate_impact, "get_burden_outcome_ids", mock_get_burden_outcome_ids)
  mockery::stub(calculate_impact, "get_burden_estimate_set_ids", mock_get_burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_impact_for_burden_estimate_set", mock_get_impact_for_burden_estimate_set)
  mockery::stub(calculate_impact, "impact_by_birth_year", mock)

  args <- test_args
  args$method <- "birth_year"
  do.call(calculate_impact, args)

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)
  expect_equal(as.character(args[[1]][[1]]$scenario), "baseline")
  expect_equal(as.character(args[[1]][[2]]$scenario), "focal")
})

test_that("calculate_impact calls impact_by_year_of_vaccination_activity_type when method is yov_activity_type", {
  mock <- mockery::mock(test_outputs$burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_touchstone_id", mock_get_touchstone_id)
  mockery::stub(calculate_impact, "get_burden_outcome_ids", mock_get_burden_outcome_ids)
  mockery::stub(calculate_impact, "get_burden_estimate_set_ids", mock_get_burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_impact_for_burden_estimate_set", mock_get_impact_for_burden_estimate_set)
  mockery::stub(calculate_impact, "impact_by_year_of_vaccination_activity_type", mock)
  mockery::stub(calculate_impact, "get_fvps", "test_fvps")

  args <- test_args
  args$method <- "yov_activity_type"
  do.call(calculate_impact, args)

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)
  expect_equal(as.character(args[[1]][[1]]$scenario), "baseline")
  expect_equal(as.character(args[[1]][[2]]$scenario), "focal")
  expect_equal(as.character(args[[1]][[3]]), "test_fvps")
  expect_equal(unlist(args[[1]][[4]]), test_args$vaccination_years)
})

test_that("calculate_impact calls impact_by_year_of_vaccination_birth_cohort when method is yov_birth_cohort", {
  mock <- mockery::mock(test_outputs$burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_touchstone_id", mock_get_touchstone_id)
  mockery::stub(calculate_impact, "get_burden_outcome_ids", mock_get_burden_outcome_ids)
  mockery::stub(calculate_impact, "get_burden_estimate_set_ids", mock_get_burden_estimate_set_ids)
  mockery::stub(calculate_impact, "get_impact_for_burden_estimate_set", mock_get_impact_for_burden_estimate_set)
  mockery::stub(calculate_impact, "impact_by_year_of_vaccination_birth_cohort", mock)
  mockery::stub(calculate_impact, "get_fvps", "test_fvps")

  args <- test_args
  args$method <- "yov_birth_cohort"
  do.call(calculate_impact, args)

  mockery::expect_called(mock, 1)
  args <- mockery::mock_args(mock)
  expect_equal(as.character(args[[1]][[1]]$scenario), "baseline")
  expect_equal(as.character(args[[1]][[2]]$scenario), "focal")
  expect_equal(as.character(args[[1]][[3]]), "test_fvps")
  expect_equal(unlist(args[[1]][[4]]), test_args$vaccination_years)
})
