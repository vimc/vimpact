test_that("coverage set by touchstone table connection setup correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit(
    DBI::dbDisconnect(con)
  )
  DBI::dbCreateTable(con, "coverage_set", c(touchstone = "text",
                                            gavi_support_level = "text"))
  DBI::dbAppendTable(con, "coverage_set",
                     data.frame(touchstone = c("1", "1", "2"),
                                 gavi_support_level = c("none", "test", "none")))
  coverage_by_touchstone <- get_coverage_set_by_touchstone(con)
  expect_true(dplyr::is.tbl(coverage_by_touchstone))
  expect_equal(nrow(dplyr::collect(coverage_by_touchstone)), 1)
})

test_that("coverage table connection setup correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit(
    DBI::dbDisconnect(con)
  )
  DBI::dbCreateTable(con, "coverage", c(name = "text"))
  DBI::dbAppendTable(con, "coverage", data.frame(name = "1"))
  coverage <- get_coverage(con)
  expect_true(dplyr::is.tbl(coverage))
})

test_that("gender table connection setup correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit(
    DBI::dbDisconnect(con)
  )
  DBI::dbCreateTable(con, "gender", c(name = "text"))
  DBI::dbAppendTable(con, "gender", data.frame(name = "1"))
  gender <- get_gender(con)
  expect_true(dplyr::is.tbl(gender))
})

test_that("country table connection setup correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit(
    DBI::dbDisconnect(con)
  )
  DBI::dbCreateTable(con, "country", c(name = "text"))
  DBI::dbAppendTable(con, "country", data.frame(name = "1"))
  country <- get_country(con)
  expect_true(dplyr::is.tbl(country))
})

test_that("scenario table connection setup correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit(
    DBI::dbDisconnect(con)
  )
  DBI::dbCreateTable(con, "scenario", c(name = "text"))
  DBI::dbAppendTable(con, "scenario", data.frame(name = "1"))
  scenario <- get_scenario(con)
  expect_true(dplyr::is.tbl(scenario))
})

test_that("scenario_description table connection setup correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
  on.exit(
    DBI::dbDisconnect(con)
  )
  DBI::dbCreateTable(con, "scenario_description", c(name = "text"))
  DBI::dbAppendTable(con, "scenario_description", data.frame(name = "1"))
  scenario_description <- get_scenario_description(con)
  expect_true(dplyr::is.tbl(scenario_description))
})
