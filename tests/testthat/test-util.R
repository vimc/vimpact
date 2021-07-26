context("util")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})

test_that("not_if_finite works", {
  expect_true(not_is_finite(NaN))
  expect_false(not_is_finite(1L))
  expect_true(not_is_finite(Inf))
  expect_true(not_is_finite(-Inf))
})

test_that("merge_by_common_cols works", {
  x <- data_frame(id = c(1, 2), name = c("a", "b"), hight = c(1.75, 1.80))
  y <- data_frame(id = c(1, 3), name = c("a", "c"), age = c(20, 30))
  expect_equivalent(merge_by_common_cols(x, y), merge(x, y, by = c("id", "name")))
  expect_equivalent(merge_by_common_cols(x, y, all.x = TRUE), merge(x, y, by = c("id", "name"), all.x = TRUE))
  expect_equivalent(merge_by_common_cols(x, y, all = TRUE), merge(x, y, by = c("id", "name"), all = TRUE))

})

test_that("set_as_na works", {
  expect_true(is.na(set_as_na(1)))
  expect_true(is.na(set_as_na("1")))
  expect_true(is.na(set_as_na(NaN)))
  expect_true(is.na(set_as_na(Inf)))

})

test_that("'%!in%' works", {
  expect_true(1 %!in% (c(2, 3)))
  expect_false(2 %!in% (c(2, 3)))
})

test_that("grepv works", {
  tmp <- c("a_b_c")
  expect_true(all(grepv(c("a", "b", "c"), tmp)))
})

test_that("squote works", {
  a <- 1
  expect_true(squote(a) == "'1'")
})

test_that("sql_in works", {
  a <- 1:3
  expect_true(sql_in(a, text_item = FALSE) == "(1, 2, 3)")
  expect_true(sql_in(a, text_item = TRUE) == "('1', '2', '3')")

})

test_that("assert_col_names checks required columns are present", {
  data <- data_frame(one = c("1", "2", "3"),
                     this = c(1, 2, 3))
  expect_true(assert_has_columns(data, "one"))
  expect_true(assert_has_columns(data, c("one", "this")))
  expect_error(assert_has_columns(data, c("one", "two", "three")),
               "Required column names two, three are missing from data")
})

test_that("assert_allowed_values checks columns have only allowed values", {
  data <- data_frame(one = c("1", "2", "3"),
                     this = c(1, 2, 3))
  expect_true(assert_allowed_values(data, "one", c("1", "2", "3")))
  expect_true(assert_allowed_values(data, "one", c("1", "2", "3")))
  expect_error(assert_allowed_values(data, "one", "1"),
               "Column 'one' contains values '2', '3'. Allowed values are '1'.")
})
