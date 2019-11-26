#' Create parameters for impact calculations
#'
#' @param method Method to use - either cohort or standard
#' @param fvps Data frame of FVP information
#' @param countries Data frame of country information
#' @param disease Name of the disease to compute impact for
#' @param outcome Name of the outcome (deaths, cases, dalys)
#' @param by_country Logical, indicating if impact should be computed by country
#' @param cohorts Vector of cohorts (default is all GAVI years)
#' @param present Year to interpret as present
#'
#' @export
impact_ratio_parameters <- function(method, fvps, countries, disease,
                                    outcome, by_country = TRUE,
                                    cohorts = 2000:2030,
                                    present = 2019) {
  method <- match_value(method, c("cohort", "standard"))
  assert_is(fvps, "data.frame")
  assert_is(countries, "data.frame")
  disease <- match_value(disease, valid_diseases())
  outcome <- match_value(outcome, valid_outcomes())
  assert_scalar_logical(by_country)
  assert_numeric(cohorts)
  assert_scalar_numeric(present)

  cohort_min <- min(cohorts)
  cohort_max <- max(cohorts)

  if (present <= cohort_min || present >= cohort_max) {
    stop(sprintf("'present' must lie between (%d, %d)",
                 cohort_min, cohort_max))
  }
  cohort_groups <- list(
    cohort_min:present,
    (present + 1):cohort_max,
    cohort_min:cohort_max)

  ret <- list(method = method,
              fvps = fvps,
              countries = countries,
              disease = disease,
              outcome = outcome,
              by_country = by_country,
              cohorts = cohorts,
              present = present,
              cohort_groups = cohort_groups)
  class(ret) <- "impact_ratio_parameters"
  ret
}
