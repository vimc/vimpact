test_args <- list(
  con = NULL,
  touchstone = "t1",
  baseline_vaccine_delivery = list(list(activity_type = "none",
                                        vaccine = "none")),
  focal_vaccine_delivery = list(list(vaccine = "HepB",
                                     activity_type = "routine")),
  countries = "AFG",
  vaccination_years = 2000:2010
)

mock_get_population_dplyr <- function(con, touchstone, countries, years) {
  data.frame(country = "AFG",
             year = "2000",
             age = c(1, 2, 3, 4),
             gender = "male",
             value = 10000)
}

mock_get_coverage_data <- function(con, touchstone, baseline_vaccine_delivery,
                                   focal_vaccine_delivery, countries,
                                   vaccination_years) {
  data.frame(coverage_set = 1, vaccine = "HepB", country = "AFG", year = "2000",
             activity_type = "routine", age_from = 1, age_to = 3,
             gender = "male", target = 1000, coverage = 1000)
}

test_that("only relevant ages are included", {
  mockery::stub(get_fvps, "get_population_dplyr", mock_get_population_dplyr)
  mockery::stub(get_fvps, "get_coverage_data", mock_get_coverage_data)
  result <- do.call(get_fvps, test_args)

  expect_equal(result$age, c(1, 2, 3))
})
