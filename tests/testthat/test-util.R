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


test_that("'%!in%' works", {
  expect_true(1 %!in% (c(2, 3)))
  expect_false(2 %!in% (c(2, 3)))
})

test_that("grepv works", {
  tmp <- c("a_b_c")
  expect_true(all(grepv(c("a", "b", "c"), tmp)))
})