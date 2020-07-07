context("Test Impact Calculations")

test_that("test if vimpact functions are working as expected for central estimates", {
  skip_if_not_installed("RSQLite")
  con <- test_montagu_readonly_connection()
  con_test <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit({
    DBI::dbDisconnect(con)
    DBI::dbDisconnect(con_test)
  })
  
  
  ### test extract_vaccination_history() function
  
  
})
