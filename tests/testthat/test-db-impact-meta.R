test_that("test get_meta_from_recipe",{
  ## run function for data
  con <- test_montagu_readonly_connection()
  data <- get_meta_from_recipe(con = con, disease = "Hib")
  expect_true(all(data$method == "method0"))
  expect_true(all(data$touchstone == "201710gavi-5"))
  expect_true(all(data$disease == "Hib"))
  expect_equal(sum(data$meta_type == "baseline"), sum(data$meta_type == "focal"))
  expect_equal(length(unique(data$modelling_group)), 2L)
})
