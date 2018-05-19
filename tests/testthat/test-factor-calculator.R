context("test-factor-calculator.R")

data("tf_quote", "tf_quotes")
dt <- data.table::copy(tf_quote)
qt <- tf_quote_xptr(dt)
qts <- tf_quotes_xptr(list(bb = dt[-(1:5)], aa = dt[1:(.N - 5)]))

test_that("rcpp enc2utf8 works", {
  x <- c("El. pa\u00c5\u00a1tas", "fa\u00e7ile", "\u00a1tas", "\u00de", "\u00e7ile")
  latin1 <- iconv(x, from = "UTF-8", to = "latin1")
  res <- tf_enc2utf8(latin1)
  expect_equal(
    Encoding(res),
    rep("UTF-8", 5)
  )
  expect_identical(res, x)
  Encoding(res) <- "unknown"
  expect_equal(
    Encoding(res),
    rep("unknown", 5)
  )
  res <- tf_enc2utf8(res, TRUE)
  expect_equal(
    Encoding(res),
    rep("UTF-8", 5)
  )
  expect_identical(res, x)
})

test_that("assert_class works", {
  x <- structure(list(a = 1), class = c("abc", "bcd"))
  expect_error(
    tf_assert_class(1, "integer"), # 1 actually has no class attributes
    "must be integer"
  )
  expect_silent(tf_assert_class(x, "abc"))
  expect_silent(tf_assert_class(x, "bcd"))
  expect_error(tf_assert_class(x, "ddd"), "must be ddd")
})


test_that("qt/qts has class", {
  expect_is(qt, "tf_quote_xptr")
  expect_is(qts, "tf_quotes_xptr")
})

test_that("tf_qt_cal() and tf_qts_cal() will check the xptr's class", {
  expect_error(
    tf_qts_cal(qt, "abc", range(tail(dt$DATE))),
    "must be tf_quotes_xptr"
  )
  expect_error(
    tf_qt_cal(qts, "abc", range(tail(dt$DATE))),
    "must be tf_quote_xptr"
  )
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
  expect_error(
    tf_qts_cal(qts, "#garbname#", anydate(c(20180101, 20180109))),
    "factor #garbname# must be defined before using"
  )
})

test_that("tf_qt_cal throws errors if the factor is supporsed to be cal by qts", {
  factors <- attr(tf_reg_factors(), "panel", exact = TRUE)
  skip_if(length(factors) == 0)
  expect_error(
    tf_qt_cal(qt, factors[1], anydate(c(20180101, 20180109))),
    "can only be used in tf_mcal()",
    fixed = TRUE
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
  factors <- tf_reg_factors()
  from_to <- range(tail(dt$DATE, 10))
  for (factor in factors) {
    res <- tf_qts_cal(qts, factor, from_to)
    expect_is(res, "xts")
    expect_equal(nrow(res), 10)
    expect_equal(ncol(res), 2)
    ## ensure it won't change the order of the name of the input list
    expect_named(res, c("bb", "aa"))
    expect_equivalent(index(res), tail(dt$DATE, 10))
    expect_true(all(is.finite(res) | is.na(res)))
    expect_true(!any(is.nan(res)))
  }
})

test_that("tf_qts_cal support nonASCII inputs", {
  from_to <- range(tail(dt$DATE, 10))
  factor <- tf_reg_factors()[1]

  utf8_names <- c("El. pa\u00c5\u00a1tas", "fa\u00e7ile", "\u00a1tas", "\u00de", "\u00e7ile")
  latin1_names <- iconv(utf8_names, from = "UTF-8", to = "latin1")

  x <- setNames(list(dt, dt, dt, dt, dt), utf8_names)
  qts <- tf_quotes_xptr(x)
  res <- tf_qts_cal(qts, factor, from_to)
  expect_identical(names(res), utf8_names)
  expect_equal(Encoding(names(res)), rep("UTF-8", 5))

  x <- setNames(list(dt, dt, dt, dt, dt), latin1_names)
  qts <- tf_quotes_xptr(x)
  res <- tf_qts_cal(qts, factor, from_to)
  expect_identical(names(res), utf8_names)
  expect_equal(Encoding(names(res)), rep("UTF-8", 5))
})


test_that("tf_qts_cal concurrecy returns the same as one core", {
  skip_if(tf_hardware_cores() <= 1, "only one core available")
  from_to <- range(tail(dt$DATE, 10))
  factor <- tf_reg_factors()[1]
  res1 <- tf_qts_cal(qts, factor, from_to, threads_no = 1)
  res2 <- tf_qts_cal(qts, factor, from_to, threads_no = 2)
  expect_equal(res1, res2)
})

