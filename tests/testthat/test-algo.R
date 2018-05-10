context("test-algo.R")

test_that("delta", {
  expect_equal(tf_delta(c(3, 8, 20)), c(5, 12))
  expect_equal(tf_delta(c(3, 3)), 0)
  expect_error(tf_delta(3), "x must have at least two elements.", fixed = TRUE)
  expect_error(tf_delta(double()), "x must have at least two elements.", fixed = TRUE)
})

test_that("rank", {
  x <- c(10, 1, 2, 8, 1.3, 1.34, 1)
  expect_equal(tf_rank(x), frank(x, ties.method = "min"))
  expect_equal(tf_rank(double()), double())
  expect_equal(tf_rank(20), 1)
})

test_that("sum", {
  expect_equal(tf_sum(double()), 0)
  expect_equal(tf_sum(1.5), 1.5)
  expect_equal(tf_sum(c(0.3, 1.5)), 1.8)
})

test_that("mean", {
  expect_equal(tf_mean(1.5), 1.5)
  expect_equal(tf_mean(c(0.3, 1.5)), 0.9)
})

test_that("stdev", {
  expect_equal(tf_stdev(double()), NA_real_)
  expect_equal(tf_stdev(1.2), NA_real_)

  x <- rnorm(10)
  expect_equal(tf_stdev(x), sd(x))
})