test_that("can filter by countries", {
  countries <- c("AFG", "UGA")
  df <- data.frame(
    country = c("AFG", "PAK", "UGA"),
    other_val = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  result <- filter_country(df, countries, NULL)
  expected <- data.frame(
    country = c("AFG", "UGA"),
    other_val = c(1, 3),
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected)
})

test_that("filter by countries can handle nulls", {
  df <- data.frame(
    country = c("AFG", "PAK", "UGA"),
    other_val = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  result <- filter_country(df, NULL, NULL)
  expect_equal(result, df)
})

test_that("can filter burden estimates by countries and map numeric to character ids", {
  countries <- c("AFG", "UGA")
  df <- data.frame(
    country = c(1, 2, 3), # note numeric ids
    burden_estimate_set = c(11, 12, 13),
    year = "2020",
    burden_outcome = "dalys",
    activity_type = "routine",
    value = 1000,
    age = 5,
    scenario = 10,
    stringsAsFactors = FALSE
  )
  country <- data.frame(
    id = c("AFG", "PAK", "UGA"),
    nid = c(1, 2, 3)
  )
  result <- filter_country_impact(df, countries, country)
  result$country <- as.character(result$country)
  expected <- data.frame(
    burden_estimate_set = c(11, 13),
    country = c("AFG", "UGA"), # note human readable ids
    year = "2020",
    burden_outcome = "dalys",
    activity_type = "routine",
    value = 1000,
    age = 5,
    scenario = 10,
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected, ignore_attr = TRUE)
})

test_that("filter_country_impact can handle nulls", {
  df <- data.frame(
    country = c(1, 2, 3), # note numeric ids
    burden_estimate_set = c(11, 12, 13),
    year = "2020",
    burden_outcome = "dalys",
    activity_type = "routine",
    value = 1000,
    age = 5,
    scenario = 10,
    stringsAsFactors = FALSE
  )
  country <- data.frame(
    id = c("AFG", "PAK", "UGA"),
    nid = c(1, 2, 3)
  )
  result <- filter_country_impact(df, NULL, country)
  result$country <- as.character(result$country)
  expected <- data.frame(
    burden_estimate_set = c(11, 12, 13),
    country = c("AFG", "PAK", "UGA"), # note human readable ids
    year = "2020",
    burden_outcome = "dalys",
    activity_type = "routine",
    value = 1000,
    age = 5,
    scenario = 10,
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected, ignore_attr = TRUE)
})

test_that("can filter by age", {
  df <- data.frame(
    age = c(5, 3, 6),
    other_val = c(1, 2, 3)
  )
  result <- filter_age(df, is_under5 = TRUE)
  expected <- data.frame(
    age = 3,
    other_val = 2
  )
  expect_equal(result, expected)
})

test_that("can not filter by age", {
  df <- data.frame(
    age = c(5, 3, 6),
    other_val = c(1, 2, 3)
  )
  result <- filter_age(df, is_under5 = FALSE)
  expect_equal(result, df)
})

test_that("can filter by year", {
  df <- data.frame(
    year = c("2020", "2020", "2021"),
    other_val = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  result <- filter_year(df, "2021")
  expected <- data.frame(
    year = "2021",
    other_val = 3,
    stringsAsFactors = FALSE
  )
  expect_equal(result, expected, ignore_attr = TRUE)
})

test_that("filter_year can handle nulls", {
  df <- data.frame(
    year = c("2020", "2020", "2021"),
    other_val = c(1, 2, 3)
  )
  result <- filter_year(df, NULL)
  expect_equal(result, df)
})

test_that("can aggregate population", {
  coverage <- data.frame(coverage_set = 1, vaccine = "HepB", country = "AFG", year = "2020", activity_type = "routine",
                         age_from = 1, age_to = 3, gender = "male", target = 1000, coverage = 1000)

  # test only correct ages included
  population <- data.frame(country = "AFG",
                           year = "2020",
                           age = c(1, 2, 3, 4),
                           gender = "male",
                           value = 10000)

  result <- aggregate_pop(coverage, population)
  expect_equal(result$population, 30000)

  # test only correct years included
  population <- data.frame(country = "AFG",
                           year = c("2020", "2020", "2021"),
                           age = 1,
                           gender = "male",
                           value = 10000)

  result <- aggregate_pop(coverage, population)
  expect_equal(result$population, 20000)

  # test only correct gender included
  population <- data.frame(country = "AFG",
                           year = "2020",
                           age = c(1, 2, 1),
                           gender = c("female", "male", "male"),
                           value = 10000)

  result <- aggregate_pop(coverage, population)
  expect_equal(result$population, 20000)

  # test only correct country included
  population <- data.frame(country = c("AFG", "AFG", "UGA"),
                           year = "2020",
                           age = 1,
                           gender = "male",
                           value = 10000)

  result <- aggregate_pop(coverage, population)
  expect_equal(result$population, 20000)

})
