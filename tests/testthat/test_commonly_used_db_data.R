context("Test Commonly used db functions")

test_that("test get_touchstone",{
  
  con <- test_montagu_readonly_connection()
  expect_equal(get_touchstone(con, "201710gavi"), "201710gavi-6")
  
})
