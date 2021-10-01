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
                        "hepb_deaths_hcc"))

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
  country <- dplyr::tbl(con, "country")
  old_impact <- old_impact %>%
    dplyr::left_join(country, by = c("country" = "nid"), copy = TRUE) %>%
    dplyr::select(-country, -name) %>%
    dplyr::select(country = id, burden_outcome, year = time, impact = value) %>%
    dplyr::arrange(country, year)

  expect_equal(impact, old_impact, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("can calculate impact by calendar year from db: dalys", {
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
  meta <- get_meta_from_recipe(default_recipe = FALSE, recipe = t,
                               method = "method0", con = con)
  old_impact <- get_raw_impact_details(con, meta, "dalys")
  country <- dplyr::tbl(con, "country")
  old_impact <- old_impact %>%
    dplyr::left_join(country, by = c("country" = "nid"), copy = TRUE) %>%
    dplyr::select(-country, -name) %>%
    dplyr::select(country = id, burden_outcome, year = time, impact = value) %>%
    dplyr::arrange(country, year)

  expect_equal(impact, old_impact, tolerance = 1e-5, ignore_attr = TRUE)
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
  meta <- get_meta_from_recipe(default_recipe = FALSE, recipe = t,
                               method = "method1", con = con)
  old_impact <- get_raw_impact_details(con, meta, "dalys")
  country <- dplyr::tbl(con, "country")
  old_impact <- old_impact %>%
    dplyr::left_join(country, by = c("country" = "nid"), copy = TRUE) %>%
    dplyr::select(-country, -name) %>%
    dplyr::select(country = id, burden_outcome, year = time, impact = value) %>%
    dplyr::arrange(country, year)

  expect_equal(impact, old_impact, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("can calculate impact by yov: birth cohort", {
  con <- test_montagu_readonly_connection()
  impact <- calculate_impact(
    con, "yov_birth_cohort", touchstone = "201710gavi-5",
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
  meta <- get_meta_from_recipe(default_recipe = FALSE, recipe = t,
                               method = "method2b", con = con)
  old_raw_impact <- get_raw_impact_details(con, meta, "dalys")
  fvps <- extract_vaccination_history(con, "201710gavi-5", year_min = 2000,
                                      year_max = 2030,
                                      disease_to_extract = "HepB")
  fvps$fvps <- fvps$fvps_adjusted
  fvps$country <- fvps$country_nid
  old_impact <- impact_by_year_of_vaccination(
    meta, old_raw_impact, fvps, vaccination_years = 2000:2030)

  country <- dplyr::tbl(con, "country")
  old_impact <- old_impact %>%
    dplyr::left_join(country, by = c("country" = "nid"), copy = TRUE) %>%
    dplyr::select(country = id, year = time, burden_outcome, vaccine,
                  activity_type, impact = impact) %>%
    dplyr::filter(!is.na(impact)) %>%
    dplyr::arrange(activity_type, country, year, vaccine)

  impact <- impact %>%
    dplyr::arrange(activity_type, country, year, vaccine)

  expect_equal(impact, old_impact, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("can calculate impact by yov: activity type", {
  con <- test_montagu_readonly_connection()
  impact <- calculate_impact(
    con, "yov_activity_type", touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi",  disease = "HepB",
    focal_scenario_type = "default", focal_vaccine_delivery = list(
      list(vaccine = "HepB_BD", activity_type = "routine"),
      list(vaccine = "HepB", activity_type = "routine")
    ),
    baseline_scenario_type = "novac",
    burden_outcomes = "dalys")

  recipe <- data.frame(
    touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi", disease = "HepB",
    focal = "default:HepB_BD-routine;HepB-routine",
    baseline = "novac",
    burden_outcome = "hepb_deaths_acute;hepb_cases_acute_severe")
  t <- tempfile(fileext = ".csv")
  write.csv(recipe, t, row.names = FALSE)
  meta <- get_meta_from_recipe(default_recipe = FALSE, recipe = t,
                               method = "method2a", con = con)
  old_raw_impact <- get_raw_impact_details(con, meta, "dalys")
  fvps <- extract_vaccination_history(con, "201710gavi-5", year_min = 2000,
                                      year_max = 2030,
                                      disease_to_extract = "HepB")
  fvps$fvps <- fvps$fvps_adjusted
  fvps$country <- fvps$country_nid
  old_impact <- impact_by_year_of_vaccination(
    meta, old_raw_impact, fvps, vaccination_years = 2000:2030)

  country <- dplyr::tbl(con, "country")
  old_impact <- old_impact %>%
    dplyr::left_join(country, by = c("country" = "nid"), copy = TRUE) %>%
    dplyr::select(country = id, vaccine, activity_type, year = time,
                  burden_outcome, impact) %>%
    dplyr::filter(!is.na(impact)) %>%
    dplyr::arrange(activity_type, country, year, vaccine)

  impact <- impact %>%
    dplyr::arrange(activity_type, country, year, vaccine)

  expect_equal(impact, old_impact, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("can get FVPS", {
  con <- test_montagu_readonly_connection()
  touchstone <- "201710gavi-5"
  focal_vaccine_delivery <- list(
    list(vaccine = "YF", activity_type = "routine"),
    list(vaccine = "YF", activity_type = "campaign"))
  baseline_vaccine_delivery <- list(
    list(vaccine = "none", activity_type = "none"))
  countries <- "UGA"
  vaccination_years <- 2000:2030
  ## TODO: What about if vaccines != diseases? Getting FVPs for HepB where there
  ## are multiple vaccines tis probably works because of Xiangs magic file
  new_fvps <- get_fvps(
    con, touchstone, baseline_vaccine_delivery = baseline_vaccine_delivery,
    focal_vaccine_delivery = focal_vaccine_delivery, countries = countries,
    vaccination_years = vaccination_years) %>%
    dplyr::arrange(activity_type, year, age) %>%
    dplyr::collect()
  old_fvps <- extract_vaccination_history(
    con, touchstone, year_min = 2000, year_max = 2030,
    disease_to_extract = "YF", countries_to_extract = "UGA")
  old_fvps <- old_fvps %>%
    dplyr::select(country, year, vaccine, activity_type, age,
                  fvps = fvps_adjusted) %>%
    dplyr::arrange(activity_type, year, age)
  expect_equal(new_fvps, old_fvps, ignore_attr = TRUE)
})
