context("test-factor-calculator.R")

data("tf_quote")
dt <- data.table::copy(tf_quote)
qt <- tf_quote_xptr(dt)


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
  expect_is(tf_reg_factors(), "character")
})

test_that("fails for undefined factor names", {
  expect_error(
    tf_cal(qt, "#garbname#", anydate(c(20180101, 20180109))),
    "factor #garbname# must be defined before using"
  )
})

test_that("all the factors can be run", {
  factors <- tf_reg_factors()
  from_to <- range(tail(dt$DATE, 10))
  for (factor in factors) {
    res <- tf_cal(qt, factor, from_to)
    expect_is(res, "data.frame")
    expect_equal(nrow(res), 10)
    expect_equal(res$DATE, tail(dt$DATE, 10))
    expect_true(all(is.finite(res$VALUE[!is.na(res$VALUE)])))
    expect_true(!any(is.nan(res$VALUE)))
  }
})
