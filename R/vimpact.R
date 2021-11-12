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
                             focal_scenario_type, baseline_scenario_type,
                             focal_vaccine_delivery = NULL,
                             baseline_vaccine_delivery = NULL,
                             burden_outcomes = c("deaths", "cases", "dalys"),
                             countries = NULL, is_under5 = FALSE,
                             vaccination_years = 2000:2030) {
  assert_one_of(method, c("calendar_year", "birth_year", "yov_activity_type",
                          "yov_birth_cohort"))
  scenario <- NULL

  ## Map touchstone name to ID if touchstone name given
  touchstones <- DBI::dbGetQuery(con,
                                 "SELECT id, touchstone_name FROM touchstone")
  if (!touchstone %in% touchstones[["id"]]) {
    touchstone <- get_touchstone(con, touchstone)
  }

  outcomes <- get_burden_outcome_ids(con, burden_outcomes)

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
    fvps <- get_fvps(con, touchstone, baseline_vaccine_delivery,
                     focal_vaccine_delivery, countries, vaccination_years)
    if (method == "yov_activity_type") {
      impact_by_year_of_vaccination_activity_type(baseline, focal,
                                                  fvps, vaccination_years)
    } else if (method == "yov_birth_cohort") {
      impact_by_year_of_vaccination_birth_cohort(baseline, focal,
                                                 fvps, vaccination_years)
    }
  }
}


filter_country_impact <- function(df, countries, country) {
  burden_estimate_set <- id <- year <- burden_outcome <- activity_type <- NULL
  value <- age <- scenario <- NULL
  if (!is.null(countries)) {
    df %>%
      dplyr::left_join(country, by = c("country" = "nid")) %>%
      dplyr::filter(id %in% countries) %>%
      dplyr::select(burden_estimate_set, country = id, year, burden_outcome,
                    activity_type, value, age, scenario)
  } else {
    ## Map country to readable ID
    df %>%
      dplyr::left_join(country, by = c("country" = "nid")) %>%
      dplyr::select(burden_estimate_set, country = id, year, burden_outcome,
                    activity_type, value, age, scenario)
  }
}


filter_country <- function(df, countries, country) {
  if (!is.null(countries)) {
    df %>%
      dplyr::filter(country %in% countries)
  } else {
    df
  }
}


filter_age <- function(df, is_under5) {
  age <- NULL
  if (is_under5) {
    df %>%
      dplyr::filter(age < 5)
  } else {
    df
  }
}


filter_year <- function(df, vaccination_years) {
  year <- NULL
  if (!is.null(vaccination_years)) {
    df %>%
      dplyr::filter(year %in% vaccination_years)
  } else {
    df
  }
}

#' Get adjusted fvps for particular touchstone and vaccine delivery methods
#'
#' @param con DB connection
#' @param touchtonse Touchstone to get data for
#' @param baseline_vaccine_delivery A list of lists containing vaccine and
#'   activity_type (routine or campaign) describing delivery
#' @param focal_vaccine_delivery A list of lists containing vaccine and
#'   activity_type (routine or campaign) describing delivery
#' @param countries Optional vector of countries to filter data
#' @param vaccination_years Option vector of years to filter data
#'
#' @return Tibble containing fvp data
#' @keywords internal
get_fvps <- function(con, touchstone, baseline_vaccine_delivery,
                     focal_vaccine_delivery, countries = NULL,
                     vaccination_years = NULL) {
  age_from <- age <- age_to <- coverage_set <- country <- year <- NULL
  gender <- activity_type <- value <- target <- fvps_source <- NULL
  vaccine <- fvps <- NULL
  population <- get_population_dplyr(con, touchstone, countries,
                                     vaccination_years)
  coverage <- get_coverage_data(con, touchstone, baseline_vaccine_delivery,
                                focal_vaccine_delivery, countries,
                                vaccination_years)

  ## We have pop data for each age group
  ## But coverage might be for a range of ages e.g. 1 to 100
  ## We need total pop over this range of years
  aggregate_pop <- coverage %>%
    dplyr::left_join(population, by = c("country" = "country",
                                     "year" = "year",
                                     "gender" = "gender")) %>%
    dplyr::filter(age_from <= age & age <= age_to) %>%
    dplyr::group_by(coverage_set, country, year, gender, activity_type,
                    age_from, age_to) %>%
    dplyr::summarise(population = sum(value, na.rm = TRUE))


  coverage %>%
    dplyr::left_join(population, by = c("country" = "country",
                                      "year" = "year",
                                      "gender" = "gender")) %>%
    dplyr::filter(age_from <= age & age <= age_to) %>%
    dplyr::left_join(aggregate_pop, by = c("country" = "country",
                                           "coverage_set" = "coverage_set",
                                           "year" = "year",
                                           "gender" = "gender",
                                           "activity_type" = "activity_type",
                                           "age_from" = "age_from",
                                           "age_to" = "age_to")) %>%
    dplyr::mutate(target =
                    dplyr::if_else(is.na(target), population, target)) %>%
    dplyr::mutate(fvps_source = (coverage * target * value) / population) %>%
    dplyr::mutate(fvps =
                    dplyr::if_else(fvps_source > value, value, fvps_source)) %>%
    dplyr::select(country, year, vaccine, activity_type, age, fvps) %>%
    dplyr::collect()
}

#' Get coverage data for a particular touchstone and vaccine delivery method
#'
#' @param con DB connection
#' @param touchtonse Touchstone to get data for
#' @param baseline_vaccine_delivery A list of lists containing vaccine and
#'   activity_type (routine or campaign) describing delivery
#' @param focal_vaccine_delivery A list of lists containing vaccine and
#'   activity_type (routine or campaign) describing delivery
#' @param countries Optional vector of countries to filter data
#' @param vaccination_years Option vector of years to filter data
#'
#' @return Tibble of coverage data
#' @keywords internal
get_coverage_data <- function(con, touchstone, baseline_vaccine_delivery,
                              focal_vaccine_delivery, countries = NULL,
                              vaccination_years = NULL) {
  gavi_support_level <- CONCAT <- vaccine <- activity_type <- id <- year <- NULL
  age_from <- age_to <- name <- target <- NULL
  delivery <-  vcapply(c(focal_vaccine_delivery, baseline_vaccine_delivery),
                       function(x) {
                         paste(x$vaccine, x$activity_type, sep = "-")
                       })
  coverage_set <- dplyr::tbl(con, "coverage_set")
  coverage <- dplyr::tbl(con, "coverage")
  gender <- dplyr::tbl(con, "gender")
  country <- dplyr::tbl(con, "country")
  cov_set <- coverage_set %>%
    dplyr::filter(touchstone == !!touchstone &
                    gavi_support_level != "none") %>%
    dplyr::mutate("delivery" = CONCAT(vaccine, "-", activity_type)) %>%
    dplyr::filter(delivery %in% !!delivery) %>%
    dplyr::select(coverage_set = id, vaccine, activity_type)

  cov <- cov_set %>%
    dplyr::left_join(coverage, by = c("coverage_set" = "coverage_set")) %>%
    dplyr::left_join(gender, by = c("gender" = "id")) %>%
    dplyr::filter(coverage > 0) %>%
    filter_country(countries, country) %>%
    filter_year(vaccination_years) %>%
    dplyr::select(coverage_set, vaccine, country, year, activity_type,
                  age_from, age_to, gender = name, target, coverage) %>%
    dplyr::collect()
}

#' Get population data for a particular touchstone
#'
#' Pull "int_pop" demographic statistic from db for a particular touchstone.
#' Optionally filter on country and years.
#'
#' @param con DB connection
#' @param touchtonse Touchstone to get data for
#' @param countries Optional vector of countries to filter data
#' @param years Option vector of years to filter data
#'
#' @return Tibble of population data
#' @keywords internal
get_population_dplyr <- function(con, touchstone, countries = NULL,
                                 years = NULL) {
  name <- code <- statistic_type <- year <- age_from <- value <- NULL
  demographic_statistic <- dplyr::tbl(con, "demographic_statistic")
  touchstone_demographic_dataset <- dplyr::tbl(con,
                                               "touchstone_demographic_dataset")
  demographic_statistic_type <- dplyr::tbl(con, "demographic_statistic_type")
  gender <- dplyr::tbl(con, "gender")
  country <- dplyr::tbl(con, "country")

  demographic_statistic %>%
    dplyr::left_join(touchstone_demographic_dataset,
                     by = c("demographic_dataset" = "demographic_dataset")) %>%
    dplyr::left_join(demographic_statistic_type,
                     by = c("demographic_statistic_type" = "id")) %>%
    dplyr::select(-name) %>%
    dplyr::rename(statistic_type = code) %>%
    dplyr::left_join(gender,
                     by = c("gender" = "id")) %>%
    dplyr::select(-gender) %>%
    dplyr::rename(gender = name) %>%
    dplyr::filter(touchstone == !!touchstone &
                    statistic_type == "int_pop" &
                    gender %in% c("Male", "Female", "Both")) %>%
    filter_country(countries, country) %>%
    filter_year(years) %>%
    dplyr::select(country, year, age = age_from, gender, value) %>%
    dplyr::collect()
}

#' Get burden outcome code and ID from a set of codes
#'
#' e.g. from "cases" return data frame of id - 1 and code - cases
#'
#' @param con DB connection
#' @param burden_outcomes Burden outcome codes to lookup in the database
#'
#' @return dbplyr lazy db connection containing burden_outcome ids and codes
#' @keywords internal
get_burden_outcome_ids <- function(con, burden_outcomes) {
  code <- id <- NULL
  burden_outcome <- dplyr::tbl(con, "burden_outcome")
  burden_outcome %>%
    dplyr::filter(code %in% burden_outcomes) %>%
    dplyr::select(id, code)
}


#' Get burden estimate set ids
#'
#' We need to locate unique ID for burden_estimate_set for
#' this combination of touchstone, modelling_group, disease, vaccine, etc.
#' There might be multiple scenarios which match the
#' touchstone, modelling_group, disease, vaccine and activity_type
#' e.g. default HepB-routine vs default HepB_BD-routine & HepB-routine
#' We need to go burden-estimate-set-wise and identify which scenario matches
#' the vaccine_delivery we are interested in.
#'
#' @param con DB connection
#' @param baseline_scenario_type Baseling scenario type, e.g. default, novac
#' @param baseline_scenario Scenario specification a string with form e.g.
#'   scenario_type-vaccine1-activity_type1;scenario_type-vaccine2-activity_type2
#' @param focal_scenario_type Focal scenario type e.g. default, novac
#' @param focal_scenario Scenario specification a string with form e.g.
#'   scenario_type-vaccine1-activity_type1;scenario_type-vaccine2-activity_type2
#' @param touchstone The touchstone to get data for
#' @param modelling_group The modelling group to get data for
#' @param disease The disease to get data for
#'
#' @return dbplyr lazy db connection containing the burden estimate set ids
#'   which match the baseline and focal scenarios
#' @keywords internal
get_burden_estimate_set_ids <- function(
  con, baseline_scenario_type, baseline_scenario, focal_scenario_type,
  focal_scenario, touchstone, modelling_group, disease) {

  scenario_type <- id <- scenario_id <- vaccine <- activity_type <- NULL
  CONCAT <- current_burden_estimate_set <- str_flatten <- delivery <- NULL

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
    dplyr::group_by(current_burden_estimate_set, activity_type) %>%
    dplyr::summarise(delivery = str_flatten(delivery, collapse = ";"),
                     .groups = "keep") %>%
    dplyr::mutate(scenario = dplyr::case_when(
      delivery == focal_scenario ~ "focal",
      delivery == baseline_scenario ~ "baseline"
    )) %>%
    dplyr::filter(!is.na(scenario)) %>%
    dplyr::select(scenario, activity_type,
                  burden_estimate_set = current_burden_estimate_set)
}


#' Get burden estimate data from db for a burden estimate set and outcome
#'
#' Given one or more burden estimate set ids and outcome ids return the
#' raw impact data fr that burden estimate. Optionally filter to get only
#' under 5 ages or for a particular country
#'
#' @param con DB connection
#' @param burden_estimate_sets Burden estimate set ids to get data from
#' @param outcomes Burden outcome ids to get data for
#' @param countries Optional filter of countries to pull data for
#' @param is_under5 If TRUE only gets burden estimate data for under 5
#'   ages
#'
#' @return dbplyr lazy db connection containing the raw impact data
#' @keywords internal
get_impact_for_burden_estimate_set <- function(
  con, burden_estimate_sets, outcomes, countries, is_under5) {
  burden_outcome <- code <- activity_type <- year <- age <- NULL
  scenario <- value <- NULL

  country <- dplyr::tbl(con, "country")
  burden_estimate <- dplyr::tbl(con, "burden_estimate")
  burden_estimate %>%
    dplyr::inner_join(burden_estimate_sets,
                      by = c("burden_estimate_set" = "burden_estimate_set")) %>%
    dplyr::inner_join(outcomes,
                      by = c("burden_outcome" = "id")) %>%
    dplyr::select(-burden_outcome) %>%
    dplyr::rename(burden_outcome = code) %>%
    filter_country_impact(countries, country) %>%
    filter_age(is_under5) %>%
    dplyr::mutate(
      burden_outcome = dplyr::case_when(
        grepl("deaths", burden_outcome) ~ "deaths",
        grepl("cases", burden_outcome) ~ "cases",
        grepl("dalys", burden_outcome) ~ "dalys"
      )
    ) %>%
    dplyr::group_by(country, activity_type, burden_outcome, year, age, scenario) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
    dplyr::select(country, year, age, burden_outcome, scenario, activity_type,
                  value)
}

#' Calculate impact from a recipe - requires a DB connection to montagu.
#'
#' This depends on the DB format of VIMC and so is for internal use only.
#'
#' @param con Connection to database
#' @param recipe_path Path to file containing recipe for burden outcome
#'    calculation. TODO: Add a vignette with more info about the recipe and
#'    reference here.d
#' @param method Impact method to use one of calendar_year, birth_year,
#'   yov_activity_type, yov_birth_cohort.
#' @param countries Vector of countries to get impact for. If NULL then impact
#'   calculated for all countries.
#' @param is_under5 If TRUE then only include data for age under 5, otherwise
#'   calculate impact for all ages
#' @param vaccination_years Years of vaccination of interest, only used for
#'   year of vaccination (yov) methods
#'
#' @return The impact for each row in the recipe
calculate_impact_from_recipe <- function(con, recipe_path, method,
                                         countries = NULL, is_under5 = FALSE,
                                         vaccination_years = 2000:2030) {
  recipe <- utils::read.csv(recipe_path)
  full_impact <- lapply(seq_len(nrow(recipe)), function(row_number) {
    row <- recipe[row_number, ]
    focal <- split_scenario_vaccine_delivery(row$focal)
    baseline <- split_scenario_vaccine_delivery(row$baseline)
    if (row$burden_outcome == "*") {
      burden_outcomes <- c("deaths", "cases", "dalys")
    } else {
      burden_outcomes <- unlist(strsplit(
        strsplit(row$burden_outcome, ";")[[1]], ","))
      ## Always include dalys
      burden_outcomes <- unique(c(burden_outcomes, "dalys"))
    }
    impact <- calculate_impact(
      con, method,
      touchstone = row$touchstone,
      modelling_group = row$modelling_group,
      disease = row$disease,
      focal_scenario_type = focal$scenario_type,
      baseline_scenario_type = baseline$scenario_type,
      focal_vaccine_delivery = focal$vaccine_delivery,
      baseline_vaccine_delivery = baseline$vaccine_delivery,
      burden_outcomes = burden_outcomes,
      countries = countries,
      is_under5 = is_under5,
      vaccination_years = vaccination_years)
    impact$index <- row_number
    impact
  })
  do.call(rbind, full_impact)
}

split_scenario_vaccine_delivery <- function(string) {
  if (string == "novac") {
    return(list(
      scenario_type = "novac",
      vaccine_delivery = NULL
    ))
  }
  type_and_delivery <- strsplit(string, ":")
  scenario_type <- type_and_delivery[[1]][1]
  delivery <- strsplit(type_and_delivery[[1]][2], ";")
  vaccine_delivery <- lapply(delivery[[1]], function(item) {
    vaccine_activity <- strsplit(item, "-")[[1]]
    list(vaccine = vaccine_activity[1],
         activity_type = vaccine_activity[2])
  })
  list(scenario_type = scenario_type,
       vaccine_delivery = vaccine_delivery)
}
