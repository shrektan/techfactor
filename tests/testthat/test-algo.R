context("test-algo.R")

test_that("delta", {
  expect_equal(tf_delta(c(3, 8, 20)), c(5, 12))
  expect_equal(tf_delta(c(3, 3)), 0)
  expect_equal(
    tf_delta(c(NA, 0.1, 0.3, NA, 0.4, 0.5, NA)),
    c(NA, 0.2, NA, NA, 0.1, NA)
  )
  expect_error(tf_delta(3), "x must have at least two elements.", fixed = TRUE)
  expect_error(tf_delta(double()), "x must have at least two elements.", fixed = TRUE)
})

test_that("rank", {
  x <- c(10, 1, 2, 8, 1.3, 1.34, 1)
  expect_equal(tf_rank(x), frank(x, ties.method = "min"))
  expect_equal(tf_rank(double()), double())
  expect_equal(tf_rank(20), 1)

  x <- c(100, NA, 2, 10, NA)
  expect_equal(tf_rank(x), frank(x, na.last = "keep", ties.method = "min"))

  x <- c(NA_real_, NA_real_)
  expect_equal(tf_rank(x), x)
})
