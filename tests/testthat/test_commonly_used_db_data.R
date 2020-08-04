context("Test Commonly used db functions")

test_that("test get_touchstone",{
  
  con <- test_montagu_readonly_connection()
  expect_equal(get_touchstone(con, "201710gavi"), "201710gavi-6")
  expect_error(get_touchstone(con, "201710"))
})

test_that("test extract_vaccination_history",{
  con <- test_montagu_readonly_connection()
  test_data <- readRDS("testthat/test_data/fvps.rds")
  dat <- extract_vaccination_history(con, touchstone_cov = "201710gavi", year_min = 2000, year_max = 2100,
                                     countries_to_extract = unique(test_data$country),
                                     disease_to_extract = c("HepB", "Measles", "YF"))
})
