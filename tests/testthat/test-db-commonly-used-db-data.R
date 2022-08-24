test_that("test get_touchstone",{

  con <- test_montagu_readonly_connection()
  expect_equal(get_touchstone(con, "201710gavi"), "201710gavi-6")
  expect_error(get_touchstone(con, "201710"))
})

test_that("test extract_vaccination_history",{
  con <- test_montagu_readonly_connection()
  test_data <- readRDS("vimpact-test-data/fvps.rds")
  test_data <- stats::aggregate(fvps ~ country + year + vaccine + activity_type, test_data, sum, na.rm = TRUE)
  dat <- extract_vaccination_history(con, touchstone_cov = "201710gavi", year_min = 2000, year_max = 2100,
                                     countries_to_extract = unique(test_data$country),
                                     disease_to_extract = c("HepB", "Measles", "YF"), full_description = FALSE)
  dat <- stats::aggregate(fvps_adjusted ~ country + year + vaccine + activity_type, dat, sum, na.rm = TRUE)

  d <- merge(dat, test_data, by = intersect(names(dat), names(test_data)), all = TRUE)
  d$fvps[is.na(d$fvps)] <- 0
  d$fvps_adjusted[is.na(d$fvps_adjusted)] <- 0
  d$diff <- round(d$fvps_adjusted - d$fvps)

  expect_true(all(d$diff[d$activity_type == "routine"] == 0))
  message("Only PAK 2015 Measles SIA has discrepancy.
          It is because previous and current approach cap campaign coverage at 100% differently.
          Current approahc is more accurate.")

  ## validate parameter
  expect_error(extract_vaccination_history(con, touchstone_cov = "201710gavi", year_min = 2000, year_max = 2000,
                                           countries_to_extract = unique(test_data$country),
                                           disease_to_extract = "Measles", full_description = FALSE,
                                           demographic_source = "dds"))
  expect_message(extract_vaccination_history(con, touchstone_cov = "201710gavi", year_min = 2000, year_max = 2000,
                                             countries_to_extract = unique(test_data$country),
                                             disease_to_extract = "Measles", full_description = FALSE,
                                             demographic_source = "dds-201710"))
})

test_that("test get_population",{
  con <- test_montagu_readonly_connection()
  dat <- get_population(con, touchstone_pop = "201710gavi-5", demographic_statistic = "int_pop", gender = "Both",
                             country_ = "PAK", year_ = 2019, age_ = 0)
  test_dat <- DBI::dbGetQuery(con, "SELECT country, year, age_from AS age, gender.name AS gender, value
                              FROM demographic_statistic
                              JOIN touchstone_demographic_dataset
                              ON touchstone_demographic_dataset.demographic_dataset = demographic_statistic.demographic_dataset
                              JOIN demographic_statistic_type
                              ON demographic_statistic_type.id = demographic_statistic.demographic_statistic_type
                              JOIN gender
                              ON gender.id = demographic_statistic.gender
                              WHERE touchstone_demographic_dataset.touchstone = '201710gavi-5'
                              AND demographic_statistic_type.code = 'int_pop'
                              AND gender.name = 'Both'
                              AND country = 'PAK'
                              AND age_from = 0
                              AND year = 2019")
  expect_equal(dat, test_dat)
})

test_that("test get_population - two ways one result",{
  con <- test_montagu_readonly_connection()
  d1 <- get_population(con, "201710gavi-5", country_= "CHN", year_ = 2020, age_ = 0, demographic_source = NULL)
  d2 <- get_population(con, touchstone_pop = NULL, country_= "CHN", year_ = 2020, age_ = 0, demographic_source = "dds-201710")
  expect_equal(d1, d2)

  d1 <- extract_vaccination_history(con, year_max = 2000, disease_to_extract = "Measles", countries_to_extract = "CHN")
  d2 <- extract_vaccination_history(con, year_max = 2000, disease_to_extract = "Measles", countries_to_extract = "CHN",
                                    demographic_source = "dds-201710")
  expect_equal(d1, d2)

})

test_that("test sql_constrain",{
  dat <- sql_constrains(country_ = NULL, year_ = NULL, age_ = NULL, burden_estimate_table = FALSE)
  expect_equal("\t \t \t", dat)

  dat <- sql_constrains(country_ = "PAK", year_ = NULL, age_ = NULL, burden_estimate_table = FALSE)
  expect_equal("AND country IN ('PAK') \t \t", dat)

  dat <- sql_constrains(country_ = "PAK", year_ = 2019, age_ = NULL, burden_estimate_table = FALSE)
  expect_equal("AND country IN ('PAK') AND year IN (2019) \t", dat)

  dat <- sql_constrains(country_ = "PAK", year_ = 2019, age_ = 0, burden_estimate_table = FALSE)
  expect_equal("AND country IN ('PAK') AND year IN (2019) AND age_from IN (0)", dat)

  dat <- sql_constrains(country_ = "PAK", year_ = 2019, age_ = 0, burden_estimate_table = TRUE)
  expect_equal("AND country.id IN ('PAK') AND year IN (2019) AND age IN (0)", dat)
})
