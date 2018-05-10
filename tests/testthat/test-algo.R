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

test_that("sma", {
  x <- rnorm(10)
  expect_error(tf_sma(x, 0), "m must be positive")
  expect_error(tf_sma(x, 11), "x must have at least 11 elements")
  expect_equal(tf_sma(x, 5), {
    res <- 0
    for (i in seq_along(x)) {
      if (i == 1) res <- x[i]
      res <- (res * (length(x) - 5) + x[i] * 5) / length(x)
    }
    res
  })
  expect_silent(tf_sma(x, 10))
  expect_equal(tf_sma(c(x, NA), 3), NA_real_)
})

test_that("wma", {
  x <- rnorm(10)
  expect_equal(tf_wma(x), {
    wts <- 0.9 ^ (length(x) - seq_along(x))
    sum(x * wts)
  })
  expect_equal(tf_wma(c(x, NA)), NA_real_)
  expect_equal(tf_wma(1.2), 1.2)
  expect_error(tf_wma(double()), "x must have at least 1 elements")
})

test_that("decaylinear", {
  expect_error(tf_decaylinear(double()), "x must have at least 1 elements")
  expect_equal(tf_decaylinear(1.2), 1.2)
  x <- rnorm(10)
  expect_equal(tf_decaylinear(x), {
    wts <- seq_along(x)
    x * wts / sum(wts)
  })
})

test_that("sequence", {
  expect_error(tf_sequence(0), "n must be a positive integer")
  expect_equal(tf_sequence(1), 1)
  expect_equal(tf_sequence(20), 1:20)
})

test_that("sumac", {
  expect_equal(tf_sumac(double()), double())
  expect_equal(tf_sumac(1.3), 1.3)
  x <- rnorm(10)
  expect_equal(tf_sumac(x), cumsum(x))
  expect_equal(tf_sumac(c(x, NA)), cumsum(c(x, NA)))
  expect_equal(tf_sumac(c(NA, x)), cumsum(c(NA, x)))
})

test_that("abs", {
  expect_equal(tf_abs(double()), double())
  expect_equal(tf_abs(-1.3), 1.3)
  x <- rnorm(10)
  expect_equal(tf_abs(x), abs(x))
  expect_equal(tf_abs(c(x, NA)), tf_abs(c(x, NA)))
  expect_equal(tf_abs(c(NA, x)), tf_abs(c(NA, x)))
})

test_that("prod", {
  expect_error(tf_prod(double()), "x must have at least 1 elements.")
  expect_equal(tf_prod(-1.3), -1.3)
  x <- rnorm(10)
  expect_equal(tf_prod(x), prod(x))
  expect_equal(tf_prod(c(x, NA)), tf_prod(c(x, NA)))
  expect_equal(tf_prod(c(NA, x)), tf_prod(c(NA, x)))
})

test_that("count", {
  expect_equal(tf_count(logical()), 0.0)
  expect_equal(tf_count(TRUE), 1.0)
  expect_equal(tf_count(FALSE), 0.0)
  x <- rnorm(10) > 0
  expect_equal(tf_count(x), sum(x))
})

test_that("regbeta", {
  expect_error(tf_regbeta(double(), double()),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_regbeta(1.2, 1.2),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_regbeta(c(1, 2), c(1, 2, 3)),
               "The size of x and y must be the same.", fixed = TRUE)
  x <- rnorm(10)
  y <- rnorm(10)
  expect_equal(tf_regbeta(y, x), coef(lm(y ~ x))[["x"]])
  expect_equal(tf_regbeta(c(NA, x), c(1, y)), NA_real_)
  expect_equal(tf_regbeta(c(1, x), c(NA, y)), NA_real_)
})

test_that("regresi", {
  expect_error(tf_regresi(double(), double()),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_regresi(1.2, 1.2),
               "x must have at least 2 elements.", fixed = TRUE)
  expect_error(tf_regresi(c(1, 2), c(1, 2, 3)),
               "The size of x and y must be the same.", fixed = TRUE)
  x <- rnorm(10)
  y <- rnorm(10)
  expect_equivalent(tf_regresi(y, x), resid(lm(y ~ x)))
  expect_equal(tf_regresi(c(NA, x), c(1, y)), rep(NA_real_, 11))
  expect_equal(tf_regresi(c(1, x), c(NA, y)), rep(NA_real_, 11))
})

test_that("filter", {
  expect_equal(tf_filter(double(), logical()), double())
  expect_error(tf_filter(c(1, 2), c(TRUE, FALSE, TRUE)),
               "The length of x and cond must equal", fixed = TRUE)
  x <- rnorm(10)
  cond <- sample(c(TRUE, FALSE), 10, replace = TRUE)
  expect_equal(tf_filter(x, cond), x[cond])
  cond <- rep(FALSE, 10)
  expect_equal(tf_filter(x, cond), double())
  cond <- rep(TRUE, 10)
  expect_equal(tf_filter(x, cond), x)
  expect_equal(tf_filter(c(1.0, NA, 2.0), c(TRUE, TRUE, FALSE)), c(1.0, NA))
})

test_that("highday", {
  expect_error(tf_highday(double()), "x must have at least 1 elements")
  expect_equal(tf_highday(1.2), 0.0)
  expect_equal(tf_highday(c(0.2, 8.8, 2.3, 1.2)), 2.0)
  expect_equal(tf_highday(c(0.2, NA, 8.8, 2.3, 1.2)), NA_real_)
})

test_that("lowday", {
  expect_error(tf_lowday(double()), "x must have at least 1 elements")
  expect_equal(tf_lowday(1.2), 0.0)
  expect_equal(tf_lowday(c(0.2, 8.8, 2.3, 1.2)), 3.0)
  expect_equal(tf_lowday(c(0.2, NA, 8.8, 2.3, 1.2)), NA_real_)
})

test_that("assert_valid_from_to", {
  expect_error(
    tf_assert_valid_from_to(c(Sys.Date())),
    "must be a length 2 date vector"
  )
  expect_error(
    tf_assert_valid_from_to(as.Date(c("2018-01-01", "2017-12-20"))),
    "larger than or equal to"
  )
  expect_silent(
    tf_assert_valid_from_to(as.Date(c("2017-12-20", "2017-12-20")))
  )
})

test_that("assert_no_na", {
  expect_error(tf_assert_no_na(c(NA, 1.0)), "mustn't contain NA")
  expect_silent(tf_assert_no_na(c(1.0)))
  expect_silent(tf_assert_no_na(double()))
})

test_that("assert_is_sorted", {
  dates <- anytime::anydate(c(180101, 180102, 180103))
})
