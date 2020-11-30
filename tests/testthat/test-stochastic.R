context("stochastic")

test_that("can get summary data of stochastic table", {
  con <- get_test_connection()

  data <- fetch_stochastic_data(con, "cross_all")
  expect_equal(nrow(data), 60)
  expect_setequal(
    colnames(data),
    c("disease", "country", "year", "deaths_default_mid", "deaths_novac_mid",
      "deaths_impact_mid", "dalys_default_mid", "dalys_novac_mid",
      "dalys_impact_mid", "deaths_default_lo", "deaths_novac_lo",
      "deaths_impact_lo", "dalys_default_lo", "dalys_novac_lo",
      "dalys_impact_lo", "deaths_default_hi", "deaths_novac_hi",
      "deaths_impact_hi", "dalys_default_hi", "dalys_novac_hi",
      "dalys_impact_hi"))
  expect_true(all(data$deaths_default_lo < data$deaths_default_mid))
  expect_true(all(data$deaths_default_mid < data$deaths_default_hi))
  expect_true(all(data$deaths_novac_lo < data$deaths_novac_mid))
  expect_true(all(data$deaths_novac_mid < data$deaths_novac_hi))
  expect_true(all(data$deaths_impact_lo < data$deaths_impact_mid))
  expect_true(all(data$deaths_impact_mid < data$deaths_impact_hi))
  expect_true(all(data$dalys_default_lo < data$dalys_default_mid))
  expect_true(all(data$dalys_default_mid < data$dalys_default_hi))
  expect_true(all(data$dalys_novac_lo < data$dalys_novac_mid))
  expect_true(all(data$dalys_novac_mid < data$dalys_novac_hi))
  expect_true(all(data$dalys_impact_lo < data$dalys_impact_mid))
  expect_true(all(data$dalys_impact_mid < data$dalys_impact_hi))

  data <- fetch_stochastic_data(con, "cross_under5")
  expect_equal(nrow(data), 60)
  expect_setequal(
    colnames(data),
    c("disease", "country", "year", "deaths_default_mid", "deaths_novac_mid",
      "deaths_impact_mid", "dalys_default_mid", "dalys_novac_mid",
      "dalys_impact_mid", "deaths_default_lo", "deaths_novac_lo",
      "deaths_impact_lo", "dalys_default_lo", "dalys_novac_lo",
      "dalys_impact_lo", "deaths_default_hi", "deaths_novac_hi",
      "deaths_impact_hi", "dalys_default_hi", "dalys_novac_hi",
      "dalys_impact_hi"))

  data <- fetch_stochastic_data(con, "cohort_all")
  expect_equal(nrow(data), 60)
  expect_setequal(
    colnames(data),
    c("disease", "country", "year", "deaths_default_mid", "deaths_novac_mid",
      "deaths_impact_mid", "dalys_default_mid", "dalys_novac_mid",
      "dalys_impact_mid", "deaths_default_lo", "deaths_novac_lo",
      "deaths_impact_lo", "dalys_default_lo", "dalys_novac_lo",
      "dalys_impact_lo", "deaths_default_hi", "deaths_novac_hi",
      "deaths_impact_hi", "dalys_default_hi", "dalys_novac_hi",
      "dalys_impact_hi"))

  data <- fetch_stochastic_data(con, "cohort_under5")
  expect_equal(nrow(data), 60)
  expect_setequal(
    colnames(data),
    c("disease", "country", "year", "deaths_default_mid", "deaths_novac_mid",
      "deaths_impact_mid", "dalys_default_mid", "dalys_novac_mid",
      "dalys_impact_mid", "deaths_default_lo", "deaths_novac_lo",
      "deaths_impact_lo", "dalys_default_lo", "dalys_novac_lo",
      "dalys_impact_lo", "deaths_default_hi", "deaths_novac_hi",
      "deaths_impact_hi", "dalys_default_hi", "dalys_novac_hi",
      "dalys_impact_hi"))
})

test_that("can get summary data of non-stochastic table throws error", {
  expect_error(fetch_stochastic_data(NULL, "not valid"),
               paste0("Table must be one of cross_all, cross_under5, ",
               "cohort_all or cohort_under5 got not valid."))
})

test_that("get_stochastic can set groups", {
  con <- get_test_connection()

  data <- fetch_stochastic_data(con, "cross_all", groups = c("disease", "year"))
  expect_equal(nrow(data), 30)
  expect_setequal(
    colnames(data),
    c("disease", "year", "deaths_default_mid", "deaths_novac_mid",
      "deaths_impact_mid", "dalys_default_mid", "dalys_novac_mid",
      "dalys_impact_mid", "deaths_default_lo", "deaths_novac_lo",
      "deaths_impact_lo", "dalys_default_lo", "dalys_novac_lo",
      "dalys_impact_lo", "deaths_default_hi", "deaths_novac_hi",
      "deaths_impact_hi", "dalys_default_hi", "dalys_novac_hi",
      "dalys_impact_hi"))
})

test_that("get_stochastic can filter before aggregating", {
  con <- get_test_connection()

  data <- fetch_stochastic_data(con, "cross_all",
                                filters = list(
                                  disease = "HepB",
                                  year = c(2001, 2002, 2003)
                                ))
  expect_equal(nrow(data), 6)
  expect_setequal(
    colnames(data),
    c("disease", "country", "year", "deaths_default_mid", "deaths_novac_mid",
      "deaths_impact_mid", "dalys_default_mid", "dalys_novac_mid",
      "dalys_impact_mid", "deaths_default_lo", "deaths_novac_lo",
      "deaths_impact_lo", "dalys_default_lo", "dalys_novac_lo",
      "dalys_impact_lo", "deaths_default_hi", "deaths_novac_hi",
      "deaths_impact_hi", "dalys_default_hi", "dalys_novac_hi",
      "dalys_impact_hi"))
  expect_setequal(data$disease, "HepB")
  expect_setequal(data$country, c("AFG", "NGA"))
  expect_setequal(data$year, c(2001, 2002, 2003))
})

test_that("where clause build correctly", {
  filters <- list(
    disease = "HepB",
    year = c(2001, 2002, 2003)
  )
  expect_equal(build_where(filters), paste0(
    "WHERE ((disease = 'HepB')) AND\n",
    "((year = 2001) OR (year = 2002) OR (year = 2003))"))

  filters <- list(
    test = TRUE
  )
  expect_equal(build_where(filters), "WHERE (test = TRUE)")

  filters <- list(
    test = NA
  )
  expect_equal(build_where(filters), "WHERE (test IS NULL)")
})

test_that("build years clause builds correctly", {
  years <- list(2000:2003, 2004, 2005, c(2010, 2013))
  clause <- build_years(years)
  expect_equal(clause, paste0(
               "SELECT 2000 AS start_year, 2003 AS end_year UNION ALL\n",
               "SELECT 2004 AS start_year, 2004 AS end_year UNION ALL\n",
               "SELECT 2005 AS start_year, 2005 AS end_year UNION ALL\n",
               "SELECT 2010 AS start_year, 2013 AS end_year"))
})

test_that("can pull stochastic data means for year groups", {
  con <- get_test_connection()
  data <- fetch_stochastic_data_year_groups(con, "cross_all_2019")

  ## Aggregating over all years, 2 countries and 2 diseases in test data
  ## gives 4 rows of data
  expect_equal(nrow(data), 4)
  expect_setequal(
    colnames(data),
    c("disease", "country", "start_time", "end_time", "deaths_default_mean",
      "deaths_novac_mean", "deaths_impact_mean", "dalys_default_mean",
      "dalys_novac_mean", "dalys_impact_mean", "deaths_default_q1",
      "deaths_novac_q1", "deaths_impact_q1", "dalys_default_q1",
      "dalys_novac_q1", "dalys_impact_q1", "deaths_default_q3",
      "deaths_novac_q3", "deaths_impact_q3", "dalys_default_q3",
      "dalys_novac_q3", "dalys_impact_q3"))
  expect_true(all(data$deaths_default_q1 < data$deaths_default_mean))
  expect_true(all(data$deaths_default_mean < data$deaths_default_q3))
  expect_true(all(data$deaths_novac_q1 < data$deaths_novac_mean))
  expect_true(all(data$deaths_novac_mean < data$deaths_novac_q3))
  expect_true(all(data$deaths_impact_q1 < data$deaths_impact_mean))
  expect_true(all(data$deaths_impact_mean < data$deaths_impact_q3))
  expect_true(all(data$dalys_default_q1 < data$dalys_default_mean))
  expect_true(all(data$dalys_default_mean < data$dalys_default_q3))
  expect_true(all(data$dalys_novac_q1 < data$dalys_novac_mean))
  expect_true(all(data$dalys_novac_mean < data$dalys_novac_q3))
  expect_true(all(data$dalys_impact_q1 < data$dalys_impact_mean))
  expect_true(all(data$dalys_impact_mean < data$dalys_impact_q3))
  expect_true(all(data$start_time == 2000))
  expect_true(all(data$end_time == 2019))

  data <- fetch_stochastic_data_year_groups(
    con, "cross_under5_2019", year_groups = list(2001:2005, 2006, 2007:2015))
  ## 2 countries, 2 diseases, 3 year groups
  expect_equal(nrow(data), 12)
  expect_setequal(
    colnames(data),
    c("disease", "country", "start_time", "end_time", "deaths_default_mean",
      "deaths_novac_mean", "deaths_impact_mean", "dalys_default_mean",
      "dalys_novac_mean", "dalys_impact_mean", "deaths_default_q1",
      "deaths_novac_q1", "deaths_impact_q1", "dalys_default_q1",
      "dalys_novac_q1", "dalys_impact_q1", "deaths_default_q3",
      "deaths_novac_q3", "deaths_impact_q3", "dalys_default_q3",
      "dalys_novac_q3", "dalys_impact_q3"))

  data <- fetch_stochastic_data_year_groups(con, "cohort_all_2019",
                                            year_groups = 2001:2015)
  ## 2 countries, 2 diseases, 15 years
  expect_equal(nrow(data), 60)
  expect_setequal(
    colnames(data),
    c("disease", "country", "start_time", "end_time", "deaths_default_mean",
      "deaths_novac_mean", "deaths_impact_mean", "dalys_default_mean",
      "dalys_novac_mean", "dalys_impact_mean", "deaths_default_q1",
      "deaths_novac_q1", "deaths_impact_q1", "dalys_default_q1",
      "dalys_novac_q1", "dalys_impact_q1", "deaths_default_q3",
      "deaths_novac_q3", "deaths_impact_q3", "dalys_default_q3",
      "dalys_novac_q3", "dalys_impact_q3"))

  data <- fetch_stochastic_data_year_groups(con, "cohort_under5_2019",
                                            year_groups = 2001:2020)
  ## 2 countries, 2 diseases, 20 years but only 5 years present in data
  ## maybe we should error if years outside db range?
  expect_equal(nrow(data), 60)
  expect_setequal(
    colnames(data),
    c("disease", "country", "start_time", "end_time", "deaths_default_mean",
      "deaths_novac_mean", "deaths_impact_mean", "dalys_default_mean",
      "dalys_novac_mean", "dalys_impact_mean", "deaths_default_q1",
      "deaths_novac_q1", "deaths_impact_q1", "dalys_default_q1",
      "dalys_novac_q1", "dalys_impact_q1", "deaths_default_q3",
      "deaths_novac_q3", "deaths_impact_q3", "dalys_default_q3",
      "dalys_novac_q3", "dalys_impact_q3"))
})

test_that("year group summarised stochastics can filter", {
  con <- get_test_connection()
  data <- fetch_stochastic_data_year_groups(con, "cross_all_2019",
                                            filters = list(disease = "HepB"))
  ## 2 countries, 1 diseases, 1 year group
  expect_equal(nrow(data), 2)
  expect_setequal(
    colnames(data),
    c("disease", "country", "start_time", "end_time", "deaths_default_mean",
      "deaths_novac_mean", "deaths_impact_mean", "dalys_default_mean",
      "dalys_novac_mean", "dalys_impact_mean", "deaths_default_q1",
      "deaths_novac_q1", "deaths_impact_q1", "dalys_default_q1",
      "dalys_novac_q1", "dalys_impact_q1", "deaths_default_q3",
      "deaths_novac_q3", "deaths_impact_q3", "dalys_default_q3",
      "dalys_novac_q3", "dalys_impact_q3"))
  expect_true(all(data$disease == "HepB"))
})

test_that("year group summarised stochastics can aggregate over groups", {
  con <- get_test_connection()
  data <- fetch_stochastic_data_year_groups(con, "cross_all_2019",
                                            groups = "disease")
  ## 2 diseases, 1 year group
  expect_equal(nrow(data), 2)
  expect_true(!("country" %in% colnames(data)))
})

test_that("year group summarised stochastics can get proportion averted", {
  con <- get_test_connection()
  data <- fetch_stochastic_data_year_groups(con, "cross_all_2019",
                                            include_proportion_averted = TRUE)
  ## 2 diseases, 2 countries, 1 year group
  expect_equal(nrow(data), 4)
  expect_setequal(
    colnames(data),
    c("disease", "country", "start_time", "end_time", "deaths_default_mean",
      "deaths_novac_mean", "deaths_impact_mean", "dalys_default_mean",
      "dalys_novac_mean", "dalys_impact_mean", "proportion_deaths_averted_mean",
      "proportion_dalys_averted_mean", "deaths_default_q1",
      "deaths_novac_q1", "deaths_impact_q1", "dalys_default_q1",
      "dalys_novac_q1", "dalys_impact_q1", "proportion_deaths_averted_q1",
      "proportion_dalys_averted_q1", "deaths_default_q3",
      "deaths_novac_q3", "deaths_impact_q3", "dalys_default_q3",
      "dalys_novac_q3", "dalys_impact_q3",  "proportion_deaths_averted_q3",
      "proportion_dalys_averted_q3"))
  expect_true(all(is.finite(data$proportion_deaths_averted_mean)))
  expect_true(all(is.finite(data$proportion_dalys_averted_mean)))
  expect_true(all(is.finite(data$proportion_deaths_averted_q1)))
  expect_true(all(is.finite(data$proportion_dalys_averted_q1)))
  expect_true(all(is.finite(data$proportion_deaths_averted_q3)))
  expect_true(all(is.finite(data$proportion_dalys_averted_q3)))
})

test_that("proportion averted deals with missing data by ignoring", {
  con <- get_test_connection()

  ## Try 1 empty group (so summary table has an empty entry)
  updated <- DBI::dbExecute(con, "UPDATE cross_all_2019
                 SET deaths_impact = NULL
                 WHERE disease = 'HepB'
                 AND country = 'AFG'
                 AND year = 2001")
  on.exit(add_dummy_data(con))
  expect_equal(updated, 10)

  data <- fetch_stochastic_data_year_groups(con, "cross_all_2019",
                                            include_proportion_averted = TRUE)
  ## 2 diseases, 2 countries, 1 year group
  expect_equal(nrow(data), 4)
  expect_true(all(is.finite(data$proportion_deaths_averted_mean)))
  expect_true(all(is.finite(data$proportion_dalys_averted_mean)))
  expect_true(all(is.finite(data$proportion_deaths_averted_q1)))
  expect_true(all(is.finite(data$proportion_dalys_averted_q1)))
  expect_true(all(is.finite(data$proportion_deaths_averted_q3)))
  expect_true(all(is.finite(data$proportion_dalys_averted_q3)))

  ## Empty denominator will return NA
  data <- fetch_stochastic_data_year_groups(con, "cross_all_2019",
                                            include_proportion_averted = TRUE,
                                            year_groups = list(2001, 2002:2015))
  ## 2 diseases, 2 countries, 2 year group
  expect_equal(nrow(data), 8)
  expect_equal(data$proportion_deaths_averted_mean[1], NA_real_)
  expect_true(all(is.finite(data$proportion_deaths_averted_mean[2:8])))
  expect_true(all(is.finite(data$proportion_dalys_averted_mean)))
  expect_equal(data$proportion_deaths_averted_q1[1], NA_real_)
  expect_true(all(is.finite(data$proportion_deaths_averted_q1[2:8])))
  expect_true(all(is.finite(data$proportion_dalys_averted_q1)))
  expect_equal(data$proportion_deaths_averted_q3[1], NA_real_)
  expect_true(all(is.finite(data$proportion_deaths_averted_q3[2:8])))
  expect_true(all(is.finite(data$proportion_dalys_averted_q3)))
})

test_that("using a single year group returns same as fetch_stochastic_data", {
  con <- get_test_connection()
  data_grouped <- fetch_stochastic_data_year_groups(con, "cross_all_2019",
                                            year_groups = as.list(2001:2015))
  data <- fetch_stochastic_data(con, "cross_all_2019",
                               groups = c("disease", "country", "year"))

  ## Align format
  nms <- names(data_grouped)
  nms[which(nms == "start_time")] <- "year"
  names(data_grouped) <- nms
  data_grouped <- data_grouped[ , colnames(data_grouped) != "end_time"]

  expect_equivalent(data_grouped, data)

  ## and filters work
  filters <- list(
    disease = "HepB"
  )
  data_grouped <- fetch_stochastic_data_year_groups(
    con, "cross_all_2019", year_groups = as.list(2001:2015), filters = filters)

  data <- fetch_stochastic_data(con, "cross_all_2019", filters = filters)

  ## Align format
  nms <- names(data_grouped)
  nms[which(nms == "start_time")] <- "year"
  names(data_grouped) <- nms
  data_grouped <- data_grouped[ , colnames(data_grouped) != "end_time"]

  expect_equivalent(data_grouped, data)
})

test_that("fetch_stochastic_data_year_groups errors if invalid table", {
  expect_error(fetch_stochastic_data_year_groups(NULL, "not a table"),
               paste0("Table must be one of cross_all_2019, cross_under5_2019,",
               " cohort_all_2019 or cohort_under5_2019",
               " got not a table"))
})

test_that("fetch_stochastic_data_year_groups filtering on year doesn't work", {
  expect_error(
    fetch_stochastic_data_year_groups(NULL, "cross_all_2019",
                                      filters = list(year = "2015")),
    "Can't filter year as year used in grouping")
})

test_that("proportion averted deals with divide by 0", {
  con <- get_test_connection()

  ## Make denominator 0 for one of the groups
  updated <- DBI::dbExecute(con, "UPDATE cross_all_2019
                 SET deaths_novac = 0
                 WHERE disease = 'HepB'
                 AND country = 'AFG'
                 AND year = 2001")
  on.exit(add_dummy_data(con))
  expect_equal(updated, 10)

  data <- fetch_stochastic_data_year_groups(con, "cross_all_2019",
                                            year_groups = 2001,
                                            include_proportion_averted = TRUE)
  ## 2 diseases, 2 countries, 1 year group
  expect_equal(nrow(data), 4)
  expect_equal(data$proportion_deaths_averted_mean[[1]], NA_real_)
  expect_equal(data$proportion_deaths_averted_q1[[1]], NA_real_)
  expect_equal(data$proportion_deaths_averted_q3[[1]], NA_real_)
})
