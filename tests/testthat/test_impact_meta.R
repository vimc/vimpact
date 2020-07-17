context("Test Impact metadata")

test_that("test assert_version",{
  method <- "method0"
  version <- "201710"
  expect_true(assert_version(method, version) == paste0(version, ".csv"))
 
  method <- "methoda"
  version <- "201710"
  expect_error(assert_version(method, version))
  
})

test_that("test assert_method",{
  method <- "method0"
  expect_invisible(assert_method(method))
  
  method <- "methoda"
  expect_error(assert_method(method))
  
})

test_that("test assert_recipe_format",{
  recipe <- data_frame(touchstone = 1, 
                       modelling_group = 2, 
                       disease = 3, 
                       focal = 4, 
                       baseline = 5, 
                       burden_outcome = 6,
                       redundant = 7)
  expect_invisible(assert_recipe_format(recipe))
  
  recipe$touchstone <- NULL
  expect_error(assert_recipe_format(recipe))
  
})
test_that("test recipe_template",{
  ## parameters
  template_version <- "201710"
  method <- "method0"
  #browser()
  ## run function for data
  data <- recipe_template(template_version, method)
  data <- read_csv(system_file("tests/testthat/recipe", method, "impact_recipe.csv"))
  
  ## test data to validate against
  test_data <- read_csv(system_file("inst/recipe", method, paste0(template_version, ".csv" )))
  
  expect_equal(data, test_data)
  unlink(system_file("tests/testthat/recipe"), recursive = TRUE)
})
