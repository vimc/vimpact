#' Calculate impact - requires a DB connection to montagu.
#'
#' This depends on the DB format of VIMC and so is for internal use only.
#'
#' @param con Connection to database.
#' @param method Impact method to use one of calendar_year, birth_year,
#' yov_activity_type, yov_birth_cohort.
#' @param touchstone The montagu touchstone to calculate impact for. Either
#'   touchstone ID or touchstone name.
#' @param modelling_group The modelling group to calculate impact for.
#' @param disease The disease to calculate impact for.
#' @param focal_scenario_type The focal scenario scenario type e.g. "default"
#' @param focal_vaccine_delivery The focal vaccine delivery methods. This should
#'   be a list of lists or NULL if scenario type is novac. Each element of first
#'   list needs to specify the vaccination and activity type. e.g.
#'   list(
#'     list(
#'       vaccine = "HepB",
#'       activity_type = "routine"
#'     ),
#'     list(
#'       vaccine = "HepB_BD",
#'       activity_type = "routine"
#'     )
#'    )
#' @param baseline_scenario_type The baseline scenario scenario type e.g.
#'   "novac"
#' @param baseline_vaccine_delivery Like `focal_vaccine_delivery` this should be
#'   a list of lists, each element containing vaccination and activity type. If
#'   `baseline_vaccine_delivery` is `novac` then this should be NULL.
#' @param burden_outcomes List of burden outcomes, defaults to "deaths", "cases"
#'   and "dalys".
#' @param countries Vector of countries to get impact for. If NULL then impact
#'   calculated for all countries.
#' @param is_under5 If TRUE then only include data for age under 5, otherwise
#'   calculate impact for all ages
#' @param vaccination_years Years of vaccination of interest, only used for
#'   year of vaccination (yov) methods
#'
#' @return Impact for this set of parameters.
#' @export
calculate_impact <- function(con, method, touchstone, modelling_group, disease,
                             focal_scenario_type, focal_vaccine_delivery = NULL,
                             baseline_scenario_type,
                             baseline_vaccine_delivery = NULL,
                             burden_outcomes = c("deaths", "cases", "dalys"),
                             countries = NULL, is_under5 = FALSE,
                             vaccination_years = 2000:2030) {
  ## Check method is valid
  assert_one_of(method, c("calendar_year", "birth_year", "yov_activity_type",
                          "yov_birth_cohort"))
  ## Check focal and baseline params are valid
  ## Check other inputs are valid?

  ## Map touchstone name to ID if touchstone name given
  touchstones <- DBI::dbGetQuery(con,
                                 "SELECT id, touchstone_name FROM touchstone")
  if (!touchstone %in% touchstones[["id"]]) {
    touchstone <- get_touchstone(con, touchstone)
  }

  outcomes <- get_burden_outcome_ids(con, burden_outcomes)

  ## We need to locate unique ID for burden_estimate_set for
  ## this cominbation of touchstone, modelling_group, disease, vaccine, etc.
  ## There might be multiple scenarios which match the
  ## touchstone, modelling_group, disease, vaccine and activity_type
  ## e.g. default HepB-routine vs default HepB_BD-routine & HepB-routine
  ## We need to go burden-estimate-set-wise and identify which scenario matches
  ## the vaccine_delivery we are interested in
  none_vaccine_delivery <- list(list(activity_type = "none",
                                     vaccine = "none"))
  if (is.null(focal_vaccine_delivery)) {
    focal_vaccine_delivery <- none_vaccine_delivery
  }
  if (is.null(baseline_vaccine_delivery)) {
    baseline_vaccine_delivery <- none_vaccine_delivery
  }
  focal_scenario <- vcapply(focal_vaccine_delivery, function(x) {
    paste(focal_scenario_type, x$vaccine, x$activity_type, sep = "-")
  })
  focal_scenario <- paste0(focal_scenario, collapse = ";")
  baseline_scenario <- vcapply(baseline_vaccine_delivery, function(x) {
    paste(baseline_scenario_type, x$vaccine, x$activity_type, sep = "-")
  })
  baseline_scenario <- paste0(baseline_scenario, collapse = ";")

  burden_estimate_sets <- get_burden_estimate_set_ids(
    con, baseline_scenario_type, baseline_scenario,
    focal_scenario_type, focal_scenario,
    touchstone, modelling_group, disease)

  raw_impact <- get_impact_for_burden_estimate_set(
    con, burden_estimate_sets, outcomes, countries, is_under5)

  baseline <- raw_impact %>%
    dplyr::filter(scenario == "baseline")
  focal <- raw_impact %>%
    dplyr::filter(scenario == "focal")

  if (method == "calendar_year") {
    impact_by_calendar_year(baseline, focal)
  } else if (method == "birth_year") {
    impact_by_birth_year(baseline, focal)
  } else {
    fvps <- get_fvps(con)
    if (method == "yov_activity_type") {
      ## This won't work yet until we get activity_type out too
      ## but there is seemingly multiple activity types per scenario
      ## e.g. see scenario 494 has campaign and routine
      ## and has same burden estimate set for both campaign and routine
      ## so what should activity_type be for this?
      impact_by_year_of_vaccination_activity_type(baseline, focal,
                                                  fvps, vaccination_years)
    } else if (method == "yov_birth_cohort") {
      impact_by_year_of_vaccination_birth_cohort(baseline, focal,
                                                 fvps, vaccination_years)
    }
  }
}

filter_country <- function(df, countries) {
  country <- dplyr::tbl(con, "country")
  if (!is.null(countries)) {
    df %>%
      dplyr::left_join(country, by = c("country" = "nid")) %>%
      dplyr::filter(id %in% countries) %>%
      dplyr::select(burden_estimate_set, country = id, year, burden_outcome,
                    value, age, scenario)
  } else {
    ## Map country to readable ID
    df %>%
      dplyr::left_join(country, by = c("country" = "nid")) %>%
      dplyr::select(burden_estimate_set, country = id, year, burden_outcome,
                    value, age, scenario)
  }
}

filter_age <- function(df, is_under5) {
  if (is_under5) {
    df %>%
      dplyr::filter(age < 5)
  } else {
    df
  }
}

## country, year, vaccine, activity_type, age, fvps
get_fvps <- function(con, touchstone, countries, vaccination_years) {
  ## 2 paths to getting relevant cov_set - where do we need 2 and why?
  ## Do some stuff to get FVP data
  coverage <- dplyr::tbl(con, "coverage")
  gender <- dplyr::tbl(con, "gender")
  cov <- coverage %>%
    dplyr::left_join(gender, by = c("gender" = "id")) %>%
    dplyr::filter(year %in% vaccination_years) %>%
    filter_country(countries)
}

get_burden_outcome_ids <- function(con, burden_outcomes) {
  burden_outcome <- dplyr::tbl(con, "burden_outcome")
  burden_outcome %>%
    dplyr::filter(code %in% burden_outcomes) %>%
    dplyr::select(id, code)
}

get_burden_estimate_set_ids <- function(
  con, baseline_scenario_type, baseline_scenario, focal_scenario_type,
  focal_scenario, touchstone, modelling_group, disease) {

  scenario <- dplyr::tbl(con, "scenario")
  scenario_description <- dplyr::tbl(con, "scenario_description")
  responsibility <- dplyr::tbl(con, "responsibility")
  responsibility_set <- dplyr::tbl(con, "responsibility_set")
  scenario_coverage_set <- dplyr::tbl(con, "scenario_coverage_set")
  coverage_set <- dplyr::tbl(con, "coverage_set")

  scenario %>%
    dplyr::left_join(scenario_description,
                     by = c("scenario_description" = "id")) %>%
    dplyr::filter(scenario_type %in%
                    c(!!baseline_scenario_type, !!focal_scenario_type)) %>%
    dplyr::select(scenario_id = id, scenario_type, disease) %>%
    dplyr::inner_join(scenario_coverage_set,
                      by = c("scenario_id" = "scenario")) %>%
    dplyr::inner_join(coverage_set,
                      by = c("coverage_set" = "id")) %>%
    dplyr::select(scenario_id, scenario_type, disease, vaccine, activity_type) %>%
    dplyr::inner_join(responsibility, by = c("scenario_id" = "scenario")) %>%
    dplyr::inner_join(responsibility_set,
                      by = c("responsibility_set" = "id")) %>%
    dplyr::filter(touchstone == !!touchstone &
                    modelling_group == !!modelling_group &
                    disease == !!disease) %>%
    dplyr::mutate("delivery" = CONCAT(scenario_type, "-",
                                      vaccine, "-", activity_type)) %>%
    dplyr::group_by(current_burden_estimate_set) %>%
    dplyr::summarise(delivery = dplyr::str_flatten(delivery, collapse = ";"),
                     .groups = "keep") %>%
    dplyr::mutate(scenario = dplyr::case_when(
      delivery == focal_scenario ~ "focal",
      delivery == baseline_scenario ~ "baseline"
    )) %>%
    dplyr::filter(!is.na(scenario)) %>%
    dplyr::select(scenario, burden_estimate_set = current_burden_estimate_set)
}

get_impact_for_burden_estimate_set <- function(
  con, burden_estimate_sets, outcomes, countries, is_under5) {

  burden_estimate <- dplyr::tbl(con, "burden_estimate")
  burden_estimate %>%
    dplyr::inner_join(burden_estimate_sets,
                      by = c("burden_estimate_set" = "burden_estimate_set")) %>%
    dplyr::inner_join(outcomes,
                      by = c("burden_outcome" = "id")) %>%
    dplyr::select(-burden_outcome) %>%
    dplyr::rename(burden_outcome = code) %>%
    filter_country(countries) %>%
    filter_age(is_under5) %>%
    dplyr::mutate(
      burden_outcome = dplyr::case_when(
        grepl("deaths", burden_outcome) ~ "deaths",
        grepl("cases", burden_outcome) ~ "cases",
        grepl("dalys", burden_outcome) ~ "dalys"
      )
    ) %>%
    dplyr::group_by(country, burden_outcome, year, age, scenario) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
    dplyr::select(country, year, age, burden_outcome, scenario, value)
}
