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

test_that("can use wrapper function to run impact for a recipe", {
  mock_data_1 <- data.frame(
    country = c("AFG", "AFG"),
    vaccine = c("HepB", "HepB"),
    activity_type = c("routine", "routine"),
    year = c(2000, 2001),
    burden_outcome = c("dalys", "dalys"),
    impact = c(2342, 3543)
  )
  mock_data_2 <- data.frame(
    country = c("AGO", "AGO"),
    vaccine = c("YF", "YF"),
    activity_type = c("campaign", "campaign"),
    year = c(2000, 2001),
    burden_outcome = c("dalys", "dalys"),
    impact = c(653, 765)
  )
  mock_calculate_impact <- mockery::mock(mock_data_1, mock_data_2,
                                         cycle = TRUE)

  recipe <- data.frame(
    touchstone = c("201710gavi-5", "201710gavi-6"),
    modelling_group = c("CDA-Razavi", "XYZ-someone"),
    disease = c("HepB", "YF"),
    focal = c("default:HepB_BD-routine;HepB-routine",
              "default:YF-routine;YF-campaign"),
    baseline = c("novac", "novac:YF-routine"),
    burden_outcome = c("hepb_deaths_acute;hepb_cases_acute_severe", "*"))
  t <- tempfile(fileext = ".csv")
  write.csv(recipe, t, row.names = FALSE)

  with_mock(calculate_impact = mock_calculate_impact, {
    impact <- calculate_impact_from_recipe("con", t, "calendar_year",
                                           countries = c("AFG", "AGO"),
                                           is_under5 = TRUE,
                                           vaccination_years = 2000:2005)
  })

  mock_data_1$index <- 1
  mock_data_2$index <- 2
  expected <- rbind(mock_data_1, mock_data_2)
  expect_equal(impact, expected)

  mockery::expect_called(mock_calculate_impact, 2)
  args <- mockery::mock_args(mock_calculate_impact)

  expect_equal(args[[1]][[1]], "con")
  expect_equal(args[[1]][[2]], "calendar_year")
  expect_equal(args[[1]]$touchstone, "201710gavi-5")
  expect_equal(args[[1]]$modelling_group, "CDA-Razavi")
  expect_equal(args[[1]]$disease, "HepB")
  expect_equal(args[[1]]$focal_scenario_type, "default")
  expect_equal(args[[1]]$baseline_scenario_type, "novac")
  expect_equal(args[[1]]$focal_vaccine_delivery, list(
    list(vaccine = "HepB_BD",
         activity_type = "routine"),
    list(vaccine = "HepB",
         activity_type = "routine")
  ))
  expect_null(args[[1]]$baseline_vaccine_delivery)
  expect_equal(args[[1]]$burden_outcomes,
               c("hepb_deaths_acute", "hepb_cases_acute_severe", "dalys"))
  expect_equal(args[[2]]$countries, c("AFG", "AGO"))
  expect_true(args[[2]]$is_under5)
  expect_equal(args[[2]]$vaccination_years, 2000:2005)

  expect_equal(args[[2]][[1]], "con")
  expect_equal(args[[2]][[2]], "calendar_year")
  expect_equal(args[[2]]$touchstone, "201710gavi-6")
  expect_equal(args[[2]]$modelling_group, "XYZ-someone")
  expect_equal(args[[2]]$disease, "YF")
  expect_equal(args[[2]]$focal_scenario_type, "default")
  expect_equal(args[[2]]$baseline_scenario_type, "novac")
  expect_equal(args[[2]]$focal_vaccine_delivery, list(
    list(vaccine = "YF",
         activity_type = "routine"),
    list(vaccine = "YF",
         activity_type = "campaign")
  ))
  expect_null(args[[2]]$baseline_vaccine_delivery)
  expect_equal(args[[2]]$burden_outcomes,
               c("deaths", "cases", "dalys"))
  expect_equal(args[[2]]$countries, c("AFG", "AGO"))
  expect_true(args[[2]]$is_under5)
  expect_equal(args[[2]]$vaccination_years, 2000:2005)
})

test_that("get_burden_estimate_set_ids can get ids from scenario specification with any order", {
  con <- test_montagu_readonly_connection()
  ids <- get_burden_estimate_set_ids(
    con,
    baseline_scenario_type = "novac",
    baseline_scenario = "novac-none-none",
    focal_scenario_type = "default",
    focal_scenario = "default-HepB-routine;default-HepB_BD-routine",
    touchstone = "201710gavi-5",
    modelling_group = "CDA-Razavi",
    disease = "HepB") %>%
    dplyr::collect()
  expect_equal(ids, data.frame(
    scenario = c("baseline", "focal"),
    delivery = c("novac-none-none",
                 "default-HepB_BD-routine;default-HepB-routine"),
    burden_estimate_set = c(579, 581),
    stringsAsFactors = FALSE
  ), ignore_attr = TRUE)
})
