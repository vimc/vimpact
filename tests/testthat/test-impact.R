context("Test Impact Calculations")

test_that("test if vimpact functions are working as expected for central estimates", {
  #skip_if_no_reference_data()
  standardise_impact_output_for_test <- function(meta, dat){

    i <- match(dat$index, meta$index)
    dat$disease <- meta$disease[i]
    dat$modelling_group <- meta$modelling_group[i]

    method <- meta$method[1]
    if(method %in% "method1"){
      names(dat)[which(names(dat) == "time")] <- "cohort"
    } else {
      names(dat)[which(names(dat) == "time")] <- "year"
    }
    names(dat)[which(names(dat) == "value")] <- "impact"

    dat
  }


  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  import_test_data_central_estimates(con, con_test)
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })

  vaccination_years <- 2000:2030
  country <- DBI::dbReadTable(con_test, "country")
  fvps <- readRDS("vimpact-test-data/fvps.rds")
  fvps$disease[fvps$vaccine %in% c("HepB", "HepB_BD")] <- "HepB"
  fvps$disease[fvps$vaccine %in% c("MCV1", "MCV2", "Measles")] <- "Measles"
  fvps$disease[fvps$vaccine %in% c("YF")] <- "YF"

  #message("preparing in-memory database storing test data")
  #import_test_data_central_estimates(con, con_test)

  ### test method 0
  message("test cross-view impact")
  meta <- DBI::dbReadTable(con_test, "recipe_0")
  meta_s <- split(meta, meta$index)
  dat <- lapply(meta_s, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)

  dat <- dat[dat$time %in% vaccination_years, ]
  dat <- standardise_impact_output_for_test(meta, dat)
  dat$country <- country$id[match(dat$country, country$nid)]

  test_data <- readRDS("vimpact-test-data/impact_method0.rds")
  test_data <- test_data[test_data$burden_outcome == "deaths_averted", ]
  test_data <- test_data[test_data$disease %in% unique(dat$disease), ]
  a <- stats::aggregate(impact ~ disease + modelling_group + country, dat, sum, na.rm = TRUE)
  b <- stats::aggregate(impact ~ disease + modelling_group + country, test_data, sum, na.rm = TRUE)
  a$impact <- round(a$impact/100)
  b$impact <- round(b$impact/100)

  expect_equal(a, b)


  ### test method 1
  message("test cohort-view impact")
  meta <- DBI::dbReadTable(con_test, "recipe_1")
  meta_s <- split(meta, meta$index)
  dat <- lapply(meta_s, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)

  dat <- dat[dat$time %in% vaccination_years, ]
  dat <- standardise_impact_output_for_test(meta, dat)
  dat$country <- country$id[match(dat$country, country$nid)]

  test_data <- readRDS("vimpact-test-data/impact_method1.rds")
  test_data <- test_data[test_data$burden_outcome == "deaths_averted", ]
  test_data <- test_data[test_data$disease %in% unique(dat$disease) & test_data$cohort %in% vaccination_years, ]
  a <- stats::aggregate(impact ~ disease + modelling_group + country, dat, sum, na.rm = TRUE)
  b <- stats::aggregate(impact ~ disease + modelling_group + country, test_data, sum, na.rm = TRUE)
  a$impact <- round(a$impact/100)
  b$impact <- round(b$impact/100)

  expect_equal(a, b)

  ### test method 2a
  #message("test impact_by_year_of_vaccination conventional approach")
  meta <- DBI::dbReadTable(con_test, "recipe_2a")
  metas <- split(meta, meta$index)
  dat <- lapply(metas, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)
  dat$country <- country$id[match(dat$country, country$nid)]
  dat2 <- lapply(metas, function(meta1) impact_by_year_of_vaccination(meta1, raw_impact = dat, fvps = fvps,
                                                                     vaccination_years = vaccination_years))
  dat2 <- do.call(rbind, dat2)

  test_data <- readRDS("vimpact-test-data/impact_method2a.rds")
  test_data <- test_data[test_data$vaccine %in% unique(dat2$vaccine), ]
  a <- unique(dat2[c("country", "vaccine", "activity_type", "impact_ratio")])
  b <- unique(test_data[test_data$burden_outcome == "deaths_averted_rate",
                        c("country", "vaccine", "activity_type", "impact")])
  d <- merge_by_common_cols(a, b, all = TRUE)
  expect_equal(d$impact_ratio*10^(-log10(d$impact_ratio)), d$impact*10^(-log10(d$impact)), tolerance = 1.e-3)

  ### test method 2b
  message("test impact_by_year_of_vaccination cohort-based approach")
  meta <- DBI::dbReadTable(con_test, "recipe_2b")

  meta <- split(meta, meta$index)
  dat <- lapply(meta, function(meta1) get_raw_impact_details(con = con_test, meta1, burden_outcome = "deaths"))
  dat <- do.call(rbind, dat)
  dat$country <- country$id[match(dat$country, country$nid)]
  dat2 <- lapply(meta, function(meta1) impact_by_year_of_vaccination(meta1, raw_impact = dat, fvps = fvps,
                                                                     vaccination_years = vaccination_years))
  dat2 <- do.call(rbind, dat2)

  test_data <- readRDS("vimpact-test-data/impact_method2b.rds")
  test_data <- test_data[test_data$vaccine %in% unique(dat2$vaccine), ]

  a <- stats::aggregate(impact ~ country + vaccine + activity_type, dat2, sum, na.rm = TRUE)
  b <- stats::aggregate(impact ~ country +  vaccine + activity_type, test_data[test_data$burden_outcome == "deaths_averted", ], sum, na.rm = TRUE)
  d <- merge(a, b, by = c("country", "vaccine", "activity_type"), all = TRUE)
  expect_equal(d$impact.x, d$impact.y, tolerance = 1.e-1)

})

test_that("impact calculation by year of vaccination country perspective", {
  impact <- impact_by_year_of_vaccination_country_perspective(
    impact_test_data, fvp_test_data_10, "routine", 2000:2030)
  expect_equal(colnames(impact),
              c("country", "burden_outcome", "value", "fvps", "impact_ratio"))
  ## One row for each country, burden_outcome combination
  expect_equal(nrow(impact), 2)
  expect_equal(unique(impact$country), c("ETH", "PAK"))
  expect_equal(unique(impact$burden_outcome), "deaths")
  expect_equal(impact$value, c(1614, 3508))
  expect_equal(impact$fvps, c(199, 417))
  expect_equal(impact$impact_ratio, c(8.1105527638191, 8.41247002398081))

  ## Impact for routine filters on birth cohorts in range of
  ## year of vaccinations - min age fvps
  fvps <- fvp_test_data_10
  fvps$age <- rep(1, 10)
  impact <- impact_by_year_of_vaccination_country_perspective(
    impact_test_data, fvps, "routine", 2005:2030)
  expect_equal(impact, data_frame(
    country = c("ETH", "PAK"),
    burden_outcome = rep("deaths", 2),
    value = c(579, 1131),
    fvps = c(23, 98),
    impact_ratio = c(25.173913, 11.540816)
  ))

  ## Impact for campaign scenarios only uses birth cohort within vaccination
  ## years range
  impact <- impact_by_year_of_vaccination_country_perspective(
    impact_test_data, fvps, "campaign", 2005:2030)
  expect_equal(impact, data_frame(
    country = c("ETH", "PAK"),
    burden_outcome = rep("deaths", 2),
    value = c(345, 456),
    fvps = c(23, 98),
    impact_ratio = c(15.0, 4.6530612)
  ))

  expect_error(
    impact_by_year_of_vaccination_country_perspective(impact_test_data, fvps,
                                                      "routine", 1980:1990),
    "No FVP data for this range of vaccination years")

  raw_impact <- impact_test_data
  raw_impact$age <- rep(10, 10)
  expect_error(
    impact_by_year_of_vaccination_country_perspective(raw_impact, fvps,
                                                      "routine", 2000:2005),
    "No impact data for this range of birth cohort and vaccination years")

  expect_error(
    impact_by_year_of_vaccination_country_perspective(raw_impact, fvps, "test",
                                                      2000:2030),
    'Activity type must be "routine" or "campaign" got "test".'
  )
})

test_that("impact calculation by year of vaccination cohort perspective", {
  impact <- impact_by_year_of_vaccination_cohort_perspective(
    impact_test_data, fvp_test_data_15, 2000:2030)
  expect_equal(colnames(impact),
              c("country", "birth_cohort", "burden_outcome", "value", "fvps",
                "impact_ratio"))
  ## One row for each country, burden_outcome, birth_cohort combination
  expect_equal(nrow(impact), 10)
  expect_equal(unique(impact$country), c("ETH", "PAK"))
  expect_equal(unique(impact$birth_cohort), 2001:2005)
  expect_equal(impact$value,
               c(234, 456, 345, 234, 345, 934, 567, 876, 675, 456))
  ## This is the unchanged fvps - cohort perspective just
  expect_equal(impact$fvps, c(34, 54, 34, 54, 23, 108, 123, 163, 123, 163))
  expect_equal(impact$impact_ratio,
               c(6.88235294117647, 8.44444444444444, 10.1470588235294,
                 4.33333333333333, 15, 8.64814814814815, 4.60975609756098,
                 5.37423312883436, 5.48780487804878,
                 2.79754601226994))

  ## Impact calculation filters on impact calculations within range of
  ## birth cohort
  raw_impact <- impact_test_data
  raw_impact <- raw_impact[raw_impact$year == 2001, ]
  impact <- impact_by_year_of_vaccination_cohort_perspective(
    raw_impact, fvp_test_data_15, 2000:2030)
  expect_equal(impact,
               data_frame(
                 country = c(rep("ETH", 5), rep("PAK", 5)),
                 birth_cohort = rep(2001:2005, 2),
                 burden_outcome = rep(c("deaths", NA, NA, NA, NA), 2),
                 value = c(234, NA, NA, NA, NA, 934, NA, NA, NA, NA),
                 fvps = c(34, 54, 34, 54, 23, 108, 123, 163, 123, 163),
                 impact_ratio = c(6.88235294117647, NA, NA, NA, NA,
                                  8.64814814814815, NA, NA, NA, NA)
               ))

  expect_error(
    impact_by_year_of_vaccination_cohort_perspective(
      raw_impact, fvp_test_data_15, 1980:1990),
    "No FVP data for this range of vaccination years")

  fvps <- fvp_test_data_15
  fvps$age <- rep(10, 15)
  expect_error(
    impact_by_year_of_vaccination_cohort_perspective(raw_impact, fvps,
                                                     2000:2005),
    "No impact data for this range of birth cohort and fvp data")
})

test_that("impact by calendar year can be calculated", {
  impact <- impact_by_calendar_year(impact_test_data_baseline,
                                    impact_test_data_focal)
  expected_data <- data_frame(
    country = c(rep("ETH", 3), rep("PAK", 3)),
    burden_outcome = rep("deaths", 6),
    year = rep(2001:2003, 2),
    impact = c(355, 348, 211, 945, 366, 101)
  )
  expect_equal(impact, expected_data)
})

test_that("impact by calendar errors if columns missing", {
  focal_data <- impact_test_data_focal[, c("country", "burden_outcome", "year")]
  expect_error(impact_by_calendar_year(impact_test_data_baseline, focal_data),
               "Required column names age, value are missing from focal_impact")
})

test_that("impact by calendar year only returns rows where groups match", {
  focal_data <- impact_test_data_focal
  focal_data$year <- rep(2003:2007, 2)
  impact <- impact_by_calendar_year(impact_test_data_baseline, focal_data)
  expect_equal(nrow(impact), 2)
})

test_that("impact by calendar year: external and internal functions agree", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
  })
  ## Add test data to db we need to add some columns for this to work
  baseline <- impact_test_data_baseline
  baseline$burden_estimate_set <- 1
  baseline$burden_outcome <- 1
  focal <- impact_test_data_focal
  focal$burden_estimate_set <- 2
  focal$burden_outcome <- 1
  burden_estimate <- rbind(baseline, focal)
  DBI::dbWriteTable(con, "burden_estimate", burden_estimate)

  meta <- data_frame(
    scenario_type = c("default", "default"),
    vaccine_delivery = c("YF-campaign,YF-routine", "YF-routine"),
    meta_type = c("baseline", "focal"),
    index = c(1, 1),
    method = c("method0", "method0"),
    burden_estimate_set = c(1, 2),
    burden_outcome_id = c("1", "1"))

  vimc_impact <- get_raw_impact_details(con = con, meta,
                                        burden_outcome = "deaths")
  public_impact <- impact_by_calendar_year(impact_test_data_baseline,
                                           impact_test_data_focal)
  ## Throw away columns we don't care about
  vimc_impact <- vimc_impact[, c("country", "burden_outcome", "time", "value")]
  ## column names slightly different time vs year and value vs impact
  expect_equivalent(vimc_impact, public_impact)
})

test_that("impact by birth year can be caluclated", {
  impact <- impact_by_birth_year(impact_test_data_baseline,
                                    impact_test_data_focal)
  expected_data <- data_frame(
    country = c(rep("ETH", 3), rep("PAK", 3)),
    burden_outcome = rep("deaths", 6),
    birth_cohort = rep(2000:2002, 2),
    impact = c(211, 157, 546, 443, 835, 134)
  )
  expect_equal(impact, expected_data)
})

test_that("impact by birth year only returns rows where birth year match", {
  focal_data <- impact_test_data_focal
  focal_data$year <- rep(2002:2006, 2)
  impact <- impact_by_birth_year(impact_test_data_baseline, focal_data)
  expect_equal(nrow(impact), 2)
  expect_equal(impact, data_frame(
    country = c("ETH", "PAK"),
    burden_outcome = rep("deaths", 2),
    birth_cohort = rep(2002, 2),
    impact = c(711, 591)
  ))
})

test_that("impact by birth year: external and internal functions agree", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
  })
  ## Add test data to db we need to add some columns for this to work
  baseline <- impact_test_data_baseline
  baseline$burden_estimate_set <- 1
  baseline$burden_outcome <- 1
  focal <- impact_test_data_focal
  focal$burden_estimate_set <- 2
  focal$burden_outcome <- 1
  burden_estimate <- rbind(baseline, focal)
  DBI::dbWriteTable(con, "burden_estimate", burden_estimate)

  meta <- data_frame(
    scenario_type = c("default", "default"),
    vaccine_delivery = c("YF-campaign,YF-routine", "YF-routine"),
    meta_type = c("baseline", "focal"),
    index = c(1, 1),
    method = c("method1", "method1"),
    burden_estimate_set = c(1, 2),
    burden_outcome_id = c("1", "1"))

  vimc_impact <- get_raw_impact_details(con = con, meta,
                                        burden_outcome = "deaths")
  public_impact <- impact_by_birth_year(impact_test_data_baseline,
                                           impact_test_data_focal)
  ## Throw away columns we don't care about
  vimc_impact <- vimc_impact[, c("country", "burden_outcome", "time", "value")]
  ## column names slightly different time vs year and value vs impact
  expect_equivalent(vimc_impact, public_impact)
})

test_that("impact by year of vaccination activity type", {
  impact <- impact_by_year_of_vaccination_activity_type(
    impact_test_data_baseline, impact_test_data_focal, fvp_test_data_15,
    2000:2030)
  expect_equal(nrow(impact), nrow(fvp_test_data_15))
  expect_equal(
    colnames(impact),
    c("country", "activity_type", "year", "burden_outcome", "impact"))
})

test_that("impact by year of vaccination activity type: only campaign", {
  baseline <- impact_test_data_baseline[
    impact_test_data_baseline$activity_type == "campaign", ]
  focal <- impact_test_data_focal[
    impact_test_data_focal$activity_type == "campaign", ]
  fvps <- fvp_test_data_15[fvp_test_data_15$activity_type == "campaign", ]
  impact <- impact_by_year_of_vaccination_activity_type(baseline, focal,
                                                        fvps, 2000:2030)
  expect_equal(nrow(impact), nrow(fvps))
  expect_equal(
    colnames(impact),
    c("country", "activity_type", "year", "burden_outcome", "impact"))
})

test_that("impact by year of vaccination activity type: only routine", {
  baseline <- impact_test_data_baseline[
    impact_test_data_baseline$activity_type == "routine", ]
  focal <- impact_test_data_focal[
    impact_test_data_focal$activity_type == "routine", ]
  fvps <- fvp_test_data_15[fvp_test_data_15$activity_type == "routine", ]
  impact <- impact_by_year_of_vaccination_activity_type(baseline, focal,
                                                        fvps, 2000:2030)
  expect_equal(nrow(impact), nrow(fvps))
  expect_equal(
    colnames(impact),
    c("country", "activity_type", "year", "burden_outcome", "impact"))
})

test_that("impact by YOV activity type: different impact & fvp", {
  baseline <- impact_test_data_baseline[
    impact_test_data_baseline$activity_type == "routine", ]
  focal <- impact_test_data_focal[
    impact_test_data_focal$activity_type == "routine", ]
  fvps <- fvp_test_data_15[fvp_test_data_15$activity_type == "campaign", ]
  impact <- impact_by_year_of_vaccination_activity_type(baseline, focal,
                                                        fvps, 2000:2030)
  ## No common entries for impact and fvps so return empty
  expect_equal(nrow(impact), 0)
})

test_that("impact by YOV activity type: no fvps in vaccination years", {
  expect_error(impact_by_year_of_vaccination_activity_type(
    impact_test_data_baseline, impact_test_data_focal, fvp_test_data_15,
    2050:2060),
    "No FVP data for this range of vaccination years")
})

test_that("impact activity type: internal and external functions agree", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
  })
  ## Add test data to db we need to add some columns for this to work
  baseline <- impact_test_data_baseline
  baseline$burden_estimate_set <-
    as.integer(baseline$activity_type != "routine") + 1
  baseline$burden_outcome <- 1

  focal <- impact_test_data_focal
  focal$burden_estimate_set <-
    as.integer(baseline$activity_type != "routine") + 3
  focal$burden_outcome <- 1
  burden_estimate <- rbind(baseline, focal)
  DBI::dbWriteTable(con, "burden_estimate", burden_estimate)

  ## Campaign
  meta <- data_frame(
    scenario_type = c("default", "default"),
    vaccine_delivery = c("no-vaccination", "YF-campaign"),
    disease = c("YF", "YF"),
    meta_type = c("baseline", "focal"),
    index = c(1, 1),
    method = c("method2a", "method2a"),
    burden_estimate_set = c(2, 4),
    burden_outcome_id = c("1", "1"))

  campaign_raw_impact <- get_raw_impact_details(con = con, meta,
                                                burden_outcome = "deaths")
  fvp <- fvp_test_data_15
  fvp$vaccine <- "YF"
  fvp$disease <- "YF"
  campaign_impact <- impact_by_year_of_vaccination(
    meta, campaign_raw_impact, fvp, vaccination_years = 2000:2030)

  ## Routine
  meta <- data_frame(
    scenario_type = c("default", "default"),
    vaccine_delivery = c("no-vaccination", "YF-routine"),
    disease = c("YF", "YF"),
    meta_type = c("baseline", "focal"),
    index = c(1, 1),
    method = c("method2a", "method2a"),
    burden_estimate_set = c(1, 3),
    burden_outcome_id = c("1", "1"))

  routine_raw_impact <- get_raw_impact_details(con = con, meta,
                                               burden_outcome = "deaths")
  routine_impact <- impact_by_year_of_vaccination(
    meta, routine_raw_impact, fvp, vaccination_years = 2000:2030)

  vimc_impact <- rbind(campaign_impact, routine_impact)

  public_impact <- impact_by_year_of_vaccination_activity_type(
    impact_test_data_baseline, impact_test_data_focal, fvp,
    2000:2030)
  ## Filter vimc impact to same columns as public to compare values
  vimc_impact <- vimc_impact[, colnames(public_impact)]
  vimc_impact <- vimc_impact[
    order(vimc_impact$country, vimc_impact$activity_type, vimc_impact$year), ]
  expect_equal(public_impact, vimc_impact, check.attributes = FALSE)
})

test_that("impact by year of vaccination birth cohort", {
  impact <- impact_by_year_of_vaccination_birth_cohort(
    impact_test_data_baseline, impact_test_data_focal, fvp_test_data_15,
    2000:2030)
  ## 2 countries, 2 birth years each for which there is impact data & FVPs
  expect_equal(nrow(impact), 4)
  expect_equal(
    colnames(impact),
    c("country", "year", "burden_outcome", "impact"))
})

test_that("impact by YOV birth cohort: no fvps in vaccination years", {
  expect_error(impact_by_year_of_vaccination_birth_cohort(
    impact_test_data_baseline, impact_test_data_focal, fvp_test_data_15,
    2050:2060),
    "No FVP data for this range of vaccination years")
})

test_that("impact birth cohort: internal and external functions agree", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
  })
  ## Add test data to db we need to add some columns for this to work
  baseline <- impact_test_data_baseline
  baseline$burden_estimate_set <- 1
  baseline$burden_outcome <- 1
  focal <- impact_test_data_focal
  focal$burden_estimate_set <- 2
  focal$burden_outcome <- 1
  burden_estimate <- rbind(baseline, focal)
  DBI::dbWriteTable(con, "burden_estimate", burden_estimate)

  ## Campaign
  meta <- data_frame(
    scenario_type = c("default", "default"),
    vaccine_delivery = c("no-vaccination", "YF-routine,YF-campaign"),
    disease = c("YF", "YF"),
    meta_type = c("baseline", "focal"),
    index = c(1, 1),
    method = c("method2b", "method2b"),
    burden_estimate_set = c(1, 2),
    burden_outcome_id = c("1", "1"))

  vimc_raw_impact <- get_raw_impact_details(con = con, meta,
                                            burden_outcome = "deaths")
  fvp <- fvp_test_data_15
  fvp$vaccine <- "YF"
  fvp$disease <- "YF"
  vimc_impact <- impact_by_year_of_vaccination(
    meta, vimc_raw_impact, fvp, vaccination_years = 2000:2030)

  public_impact <- impact_by_year_of_vaccination_birth_cohort(
    impact_test_data_baseline, impact_test_data_focal, fvp,
    2000:2030)

  ## Aggregate vimc impact and compare with public
  vimc_impact <- stats::aggregate(impact ~ country + year + burden_outcome,
                                  vimc_impact, sum, na.rm = TRUE)
  vimc_impact <- vimc_impact[
    order(vimc_impact$country, vimc_impact$year), ]
  expect_equal(public_impact, vimc_impact, check.attributes = FALSE)
})
