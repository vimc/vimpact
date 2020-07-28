context("stochastic")

test_that("can get summary data of stochastic table", {
  con <- get_test_connection()
  
  data <- get_stochastic_data(con, "cross_all")
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
  
  data <- get_stochastic_data(con, "cross_under5")
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
  
  data <- get_stochastic_data(con, "cohort_all")
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
  
  data <- get_stochastic_data(con, "cohort_under5")
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

test_that("get_stochastic can set groups", {
  con <- get_test_connection()
  
  data <- get_stochastic_data(con, "cross_all", groups = c("disease", "year"))
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