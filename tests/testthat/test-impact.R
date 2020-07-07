context("Test Impact Calculations")

test_that("test if vimpact functions are working as expected for central estimates", {
  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })
  
  message("preparing in-memory database storing test data")
  #import_test_data_central_estimates(con, con_test)
  
  ### test extract_vaccination_history() function
  message("test extract_vaccination_history")

  ### test method 0
  message("test cross-view impact")
  #meta <- DBI::dbReadTable(con_test, "recipe_0")
  
  
  ### test method 1
  message("test cohort-view impact")
  #meta <- DBI::dbReadTable(con_test, "recipe_1")
  
  
  ### test method 2a
  message("test impact_by_year_of_vaccination conventional approach")
  #meta <- DBI::dbReadTable(con_test, "recipe_2a")
  
  
  ### test method 2b
  message("test impact_by_year_of_vaccination cohort-based approach")
  #meta <- DBI::dbReadTable(con_test, "recipe_2b")
  
  
})


