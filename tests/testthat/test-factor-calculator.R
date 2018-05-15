context("test-factor-calculator.R")

data("tf_quote")
dt <- data.table::copy(tf_quote)
qt <- tf_quote_xptr(dt)

test_that("qt has class", {
  expect_is(qt, "tf_quote_xptr")
})

test_that("tf_quote_cal() and tf_quotes_cal() will check the xptr's class", {
  tf_qts_cal(qt, "abc", range(tail(dt$DATE)))
})

test_that("create_xts() return the same xts object as xts::xts()", {
  dat <- as.matrix(head(cars))
  dates <- as.Date("2018-01-01") + seq_len(nrow(dat)) - 1
  expect_equal(
    create_xts(dat, dates),
    xts::xts(dat, dates)
  )
  dimnames(dat) <- NULL
  expect_equal(
    create_xts(dat, dates),
    xts::xts(dat, dates)
  )
})

test_that("tf_reg_factors", {
  res <- tf_reg_factors()
  expect_is(res, "character")
  expect_is(attr(res, "normal", exact = TRUE), "character")
  expect_is(attr(res, "panel", exact = TRUE), "character")
  expect_equal(
    sort(res),
    sort(unique(c(
      attr(res, "normal", exact = TRUE),
      attr(res, "panel", exact = TRUE)
    )))
  )
})

test_that("fails for undefined factor names", {
  expect_error(
    tf_qt_cal(qt, "#garbname#", anydate(c(20180101, 20180109))),
    "factor #garbname# must be defined before using"
  )
})

test_that("all normal factors can be run by tf_qt_cal()", {
  factors <- attr(tf_reg_factors(), "normal", exact = TRUE)
  from_to <- range(tail(dt$DATE, 10))
  res <- tf_qt_cal(qt, factors, from_to)
  expect_is(res, "xts")
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), length(factors))
  expect_named(res, factors)
  expect_equivalent(index(res), tail(dt$DATE, 10))
  expect_true(all(is.finite(res) | is.na(res)))
  expect_true(!any(is.nan(res)))

  expect_equal(
    tf_qt_cal(qt, factors[2], from_to),
    res[, 2]
  )
})

test_that("tf_quotes_xptr requires named list", {
  expect_error(tf_quotes_xptr(list(dt)), "must have name")
})

test_that("all factors can be run by tf_qts_cal()", {
  qts <- tf_quotes_xptr(list(aa = dt, bb = dt))
  factors <- tf_reg_factors()
  from_to <- range(tail(dt$DATE, 10))
  res <- tf_qts_cal(qts, factors[1], from_to)
  expect_is(res, "xts")
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), length(factors))
  expect_named(res, factors)
  expect_equivalent(index(res), tail(dt$DATE, 10))
  expect_true(all(is.finite(res) | is.na(res)))
  expect_true(!any(is.nan(res)))
})

