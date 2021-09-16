test_that("can calculate impact by calendar year from db", {
  con <- test_montagu_readonly_connection()
  impact <- calculate_impact(
    con, "calendar_year", touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi",  disease = "HepB",
    focal_scenario_type = "default", focal_vaccine_delivery = list(
      list(vaccine = "HepB_BD", activity_type = "routine"),
      list(vaccine = "HepB", activity_type = "routine")
    ),
    baseline_scenario_type = "novac",
    burden_outcomes = c("hepb_deaths_acute", "hepb_deaths_dec_cirrh",
                        "hepb_deaths_hcc", "hepb_cases_acute_severe",
                        "hepb_cases_dec_cirrh", "hepb_cases_hcc"))

  recipe <- data.frame(
    touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi", disease = "HepB",
    focal = "default:HepB_BD-routine;HepB-routine",
    baseline = "novac",
    burden_outcome = "hepb_deaths_acute,hepb_deaths_dec_cirrh,hepb_deaths_hcc;hepb_cases_acute_severe,hepb_cases_dec_cirrh,hepb_cases_hcc")
  t <- tempfile(fileext = ".csv")
  write.csv(recipe, t, row.names = FALSE)
  meta <- get_meta_from_recipe(default_recipe = FALSE, recipe = t, con = con)
  old_impact <- get_raw_impact_details(con, meta, "deaths")
})

test_that("can calculate impact by calendar year from db", {
  con <- test_montagu_readonly_connection()
  impact <- calculate_impact(
    con, "calendar_year", touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi",  disease = "HepB",
    focal_scenario_type = "default", focal_vaccine_delivery = list(
      list(vaccine = "HepB_BD", activity_type = "routine"),
      list(vaccine = "HepB", activity_type = "routine")
    ),
    baseline_scenario_type = "novac",
    burden_outcomes = c("dalys"))

  recipe <- data.frame(
    touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi", disease = "HepB",
    focal = "default:HepB_BD-routine;HepB-routine",
    baseline = "novac",
    burden_outcome = "hepb_deaths_acute;hepb_cases_acute_severe")
  t <- tempfile(fileext = ".csv")
  write.csv(recipe, t, row.names = FALSE)
  meta <- get_meta_from_recipe(default_recipe = FALSE, recipe = t, method = "method0", con = con)
  old_impact <- get_raw_impact_details(con, meta, "dalys")
  country <- dplyr::tbl(con, "country")
  old_impact <- old_impact %>%
    dplyr::left_join(country, by = c("country" = "nid"), copy = TRUE) %>%
    dplyr::select(-country, -name) %>%
    dplyr::select(country = id, burden_outcome, year = time, impact = value) %>%
    dplyr::arrange(country, year)

  expect_equal(impact, old_impact)
})

test_that("can calculate impact by birth year from db", {
  con <- test_montagu_readonly_connection()
  impact <- calculate_impact(
    con, "birth_year", touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi",  disease = "HepB",
    focal_scenario_type = "default", focal_vaccine_delivery = list(
      list(vaccine = "HepB_BD", activity_type = "routine"),
      list(vaccine = "HepB", activity_type = "routine")
    ),
    baseline_scenario_type = "novac",
    burden_outcomes = c("dalys"))

  recipe <- data.frame(
    touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi", disease = "HepB",
    focal = "default:HepB_BD-routine;HepB-routine",
    baseline = "novac",
    burden_outcome = "hepb_deaths_acute;hepb_cases_acute_severe")
  t <- tempfile(fileext = ".csv")
  write.csv(recipe, t, row.names = FALSE)
  meta <- get_meta_from_recipe(default_recipe = FALSE, recipe = t, method = "method1", con = con)
  old_impact <- get_raw_impact_details(con, meta, "dalys")
  country <- dplyr::tbl(con, "country")
  old_impact <- old_impact %>%
    dplyr::left_join(country, by = c("country" = "nid"), copy = TRUE) %>%
    dplyr::select(-country, -name) %>%
    dplyr::select(country = id, burden_outcome, year = time, impact = value) %>%
    dplyr::arrange(country, year)

  expect_equal(impact, old_impact)
})
