test_args <- list(
  con = NULL,
  touchstone = "t1",
  delivery_methods = list("HepB-routine"),
  focal_scenario_type = "bestcase",
  countries = list("AFG"),
  vaccination_years = 2020L:2024L
)

test_that("returns coverage data tibble", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone",
                function(con, touchstone) mock_coverage_set(id = 11L:14L,
                                                            touchstone = test_args$touchstone,
                                                            vaccine = "HepB"))
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage(id = as.integer(c(1, 2, 3, 4)),
                                                                               coverage_set = 11L:14L,
                                                                               year = 2020L,
                                                                               country = "AFG",
                                                                               gavi_support = c(TRUE, FALSE),
                                                                               age_from = 1,
                                                                               age_to = 5,
                                                                               coverage = 1000,
                                                                               target = 2000,
                                                                               gender = 1L))
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country(id = "AFG"))
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario(id = as.integer(c(1, 2, 3, 4)),
                                                                               touchstone = rep("t1", 4),
                                                                               scenario_description = c("desc1", "desc2", "desc3", "desc4"),
                                                                               focal_coverage_set = 11L:14L))
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description(
    id = c("desc1", "desc2", "desc3", "desc4"),
    description = rep("desc", 4),
    disease = rep("HepB", 4),
    scenario_type = rep("bestcase", 4)
  ))
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(names(result), c("coverage_set", "vaccine", "country", "year", "activity_type",
                                "age_from", "age_to", "gender", "target", "coverage"))
  expect_equal(result$coverage_set, c(11L, 13L))
  expect_true(all(result$vaccine == "HepB"))
  expect_true(all(result$country == "AFG"))
  expect_true(all(result$yeay == "2020"))
  expect_true(all(result$activity_type == "routine"))
  expect_true(all(result$age_from == 1))
  expect_true(all(result$age_to == 5))
  expect_true(all(result$gender == "Male"))
  expect_true(all(result$target == 2000))
  expect_true(all(result$coverage == 1000))
})

test_that("only gets coverage for correct coverage sets", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set(id = 11L:14L))
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage(coverage_set = 100L))
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})

test_that("only gets coverage sets for given touchstone", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone",
                function(con, touchstone) if (touchstone == test_args$touchstone) mock_coverage_set()
                else stop("Unexpected touchstone arg"))
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage())
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 5)
})

test_that("only gets coverage sets for given activity type", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set(activity_type = "campaign"))
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage())
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})

test_that("only gets coverage sets for given vaccine", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set(vaccine = "wrong"))
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage())
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})

test_that("only gets coverage for given countries", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set())
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage(country = "UGA"))
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})

test_that("only gets coverage for given years", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set())
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage(year = 2000L))
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})

test_that("only gets non-zero coverage", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set())
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage(coverage = 0))
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})

test_that("only gets coverage for given focal coverage set", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set())
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage())
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario(focal_coverage_set = 20L))
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description())
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})

test_that("only gets coverage for given scenario type", {
  mockery::stub(get_coverage_data, "get_coverage_set_by_touchstone", function(con, touchstone) mock_coverage_set())
  mockery::stub(get_coverage_data, "get_coverage", function(con) mock_coverage())
  mockery::stub(get_coverage_data, "get_country", function(con) mock_country())
  mockery::stub(get_coverage_data, "get_gender", function(con) mock_gender())
  mockery::stub(get_coverage_data, "get_scenario", function(con) mock_scenario())
  mockery::stub(get_coverage_data, "get_scenario_description", function(con) mock_scenario_description(scenario_type = "bestminus"))
  mockery::stub(get_coverage_data, "CONCAT", function(x, sep, y) paste(x, y, sep = sep))
  result <- do.call(get_coverage_data, test_args)
  expect_equal(nrow(result), 0)
})
