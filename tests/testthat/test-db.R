context("db")

test_that("test db has been setup correctly", {
  con <- get_test_connection()
  
  expect_setequal(DBI::dbListTables(con), c("cross_all", "cross_under5",
                                            "cohort_all", "cohort_under5"))
  expect_equal(
    as.integer(DBI::dbGetQuery(con, "SELECT count(*) from cross_all")[1, 1]),
    600)
  expect_equal(
    as.integer(DBI::dbGetQuery(con, "SELECT count(*) from cross_under5")[1, 1]),
    600)
  expect_equal(
    as.integer(DBI::dbGetQuery(con, "SELECT count(*) from cohort_all")[1, 1]),
    600)
  expect_equal(as.integer(
    DBI::dbGetQuery(con, "SELECT count(*) from cohort_under5")[1, 1]),
    600)
})