context("test-algo.R")

test_that("any_na", {
  expect_false(tf_any_na(double()))
  expect_false(tf_any_na(1.0))
  expect_true(tf_any_na(c(1.0, NA)))
  expect_true(tf_any_na(NA))
})

test_that("na_vector", {
  expect_equal(tf_na_vector(1), c(NA_real_))
  expect_equal(tf_na_vector(5), rep(NA_real_, 5))
  expect_equal(tf_na_vector(0), double())
})


test_that("delta", {
  expect_equal(tf_delta(c(3, 5, NA, NA, 3, 4, NA)), c(2, NA, NA, NA, 1, NA))
  expect_equal(tf_delta(c(3, 8, 20)), c(5, 12))
  expect_equal(tf_delta(c(3, 3)), 0)
  expect_error(tf_delta(3), "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_delta(double()), "x must have at least 2 elements.", fixed = TRUE)
})

test_that("rank", {
  x <- c(10, 1, 2, 8, 1.3, 1.34, 1)
  expect_equal(tf_rank(x), frank(x, ties.method = "min"))
  expect_equal(tf_rank(double()), double())
  expect_equal(tf_rank(20), 1)
  expect_equal(tf_rank(c(1, 3, 5, NA, 7, 9)), rep(NA_real_, 6))
})

test_that("sum", {
  expect_equal(tf_sum(double()), 0)
  expect_equal(tf_sum(1.5), 1.5)
  expect_equal(tf_sum(c(0.3, 1.5)), 1.8)
  expect_equal(tf_sum(c(0.3, 1.5, NA_real_)), NA_real_)
})

test_that("mean", {
  expect_error(tf_mean(double()), "x must have at least 1 elements.", fixed = TRUE)
  expect_equal(tf_mean(1.5), 1.5)
  expect_equal(tf_mean(c(1.5, NA_real_)), NA_real_)
  expect_equal(tf_mean(c(0.3, 1.5)), 0.9)
})

test_that("stdev", {
  expect_error(tf_stdev(double()), "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_stdev(1.2), "x must have at least 2 elements.", fixed = TRUE)

  x <- rnorm(10)
  expect_equal(tf_stdev(x), sd(x))
  expect_equal(tf_stdev(c(NA, x)), NA_real_)
})

test_that("tsmin", {
  x <- rnorm(10)
  expect_equal(tf_tsmin(x), min(x))
  expect_equal(tf_tsmin(c(NA, x)), NA_real_)
  expect_error(tf_tsmin(double()), "at least 1 elements")
  expect_equal(tf_tsmin(1.2), 1.2)
})

test_that("tsmax", {
  x <- rnorm(10)
  expect_equal(tf_tsmax(x), max(x))
  expect_equal(tf_tsmax(c(NA, x)), NA_real_)
  expect_error(tf_tsmax(double()), "at least 1 elements")
  expect_equal(tf_tsmax(1.2), 1.2)
})

test_that("tsrank", {
  x <- c(5, 3, 2, 4, 8)
  expect_equal(tf_tsrank(x), 5)
  expect_equal(tf_tsrank(c(NA, x)), NA_real_)
  expect_error(tf_tsrank(double()), "at least 1 elements")
  expect_equal(tf_tsrank(1.2), 1)
})

test_that("covariance", {
  expect_error(tf_covariance(double(), double()),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_covariance(1.2, 1.2),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_covariance(c(1, 2), c(1, 2, 3)),
               "The size of x and y must be the same.", fixed = TRUE)
  x <- rnorm(10)
  y <- rnorm(10)
  expect_equal(tf_covariance(x, y), cov(x, y))
  expect_equal(tf_covariance(c(NA, x), c(1, y)), NA_real_)
  expect_equal(tf_covariance(c(1, x), c(NA, y)), NA_real_)
})

test_that("corr", {
  expect_error(tf_corr(double(), double()),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_corr(1.2, 1.2),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_corr(c(1, 2), c(1, 2, 3)),
               "The size of x and y must be the same.", fixed = TRUE)
  x <- rnorm(10)
  y <- rnorm(10)
  expect_equal(tf_corr(x, y), cor(x, y))
  expect_equal(tf_corr(c(NA, x), c(1, y)), NA_real_)
  expect_equal(tf_corr(c(1, x), c(NA, y)), NA_real_)
})

test_that("sign", {
  expect_equal(tf_sign(NA_real_), NA_real_)
  expect_equal(tf_sign(0), 0)
  expect_equal(tf_sign(1.3), 1)
  expect_equal(tf_sign(-3.1), -1)
})
