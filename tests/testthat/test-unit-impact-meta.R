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

test_that("test order_vaccine_delivery",{
  expect_equal(order_vaccine_delivery("3,2,1"), "1,2,3")
  expect_equal(order_vaccine_delivery("c,a,b"), "a,b,c")

})

test_that("test replace_burden_outcomes",{
  burden_outcomes <- data_frame(id = 1:3,
                                code = c("deaths", "cases", "dalys"))
  expect_equal(replace_burden_outcome(burden_outcomes, "deaths;cases;dalys"), "1;2;3") # match burden outcome
  expect_equal(replace_burden_outcome(burden_outcomes, "deaths;dalys;cases"), "1;2;3") # order of burden outcome dose not matter
  expect_error(replace_burden_outcome(burden_outcomes, "cases;dalys")) # must have at least three burden outcomes
  expect_error(replace_burden_outcome(burden_outcomes, "deaths;cases;dalys;d")) # must have three burden outcomes

})
