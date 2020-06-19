context("util")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})

test_that("not_if_finite works", {
  expect_true(not_is_finite(NaN))
  expect_true(not_is_finite("c"))
  expect_true(not_is_finite(Inf))
  expect_true(not_is_finite(-Inf))
})
