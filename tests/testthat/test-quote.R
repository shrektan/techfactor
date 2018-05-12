context("test-quote.R")

tags <- c("pclose", "open", "high", "low", "close", "vwap",
          "volume", "amount", "bmk_close", "bmk_open")

data("tf_quote")
dt <- data.table::copy(tf_quote)
qt <- tf_quotes_xptr(dt)

test_that("quotes' input must be a sorted tbl", {
  dt_copy <- data.table::copy(dt)
  setorder(dt_copy, -DATE)
  expect_error(
    tf_quotes_xptr(dt_copy),
    "x must be a sorted date vector"
  )
  setorder(dt_copy, DATE)
  dt_copy[, CLOSE := NULL]
  expect_error(
    tf_quotes_xptr(dt_copy),
    "CLOSE"
  )
})

test_that("quotes.tdates()", {
  # from is lower than min
  from_to <- as.Date(c("2017-12-01", "2018-01-06"))
  expect_equivalent(
    test_qt_tdates(qt, from_to),
    as.Date(c("2018-01-02", "2018-01-03", "2018-01-04", "2018-01-05"))
  )
  # to is larger than max
  from_to <- as.Date(c("2018-05-01", "2018-05-10"))
  expect_equivalent(
    test_qt_tdates(qt, from_to),
    as.Date(c("2018-05-02", "2018-05-03", "2018-05-04", "2018-05-07"))
  )
  # from_to are tdates
  from_to <- anydate(c(20180427, 20180502))
  expect_equivalent(
    test_qt_tdates(qt, from_to),
    from_to
  )
  # from_to are not tdates
  from_to <- anydate(c(20180501, 20180505))
  expect_equivalent(
    test_qt_tdates(qt, from_to),
    anydate(c(20180502, 20180503, 20180504))
  )
  # from_to are all smaller than min
  expect_equivalent(
    test_qt_tdates(qt, anydate(c(20160101, 20160101))),
    anydate(double())
  )
  # from_to are all larger than max
  expect_equivalent(
    test_qt_tdates(qt, anydate(c(20190101, 20190101))),
    anydate(double())
  )
})


test_that("quotes.set()", {
  # too small
  expect_error(
    test_qt_today(qt, anydate(20180101)),
    "negative today_index_ -1"
  )
  # too large
  expect_error(
    test_qt_today(qt, anydate(20180801)),
    "negative today_index_ -1"
  )
  # holiday
  expect_error(
    test_qt_today(qt, anydate(20180501)),
    "negative today_index_ -1"
  )
  # first
  expect_equivalent(
    test_qt_today(qt, anydate(20180102)),
    anydate(20180102)
  )
  # last
  expect_equivalent(
    test_qt_today(qt, anydate(20180507)),
    anydate(20180507)
  )
  # middle
  expect_equivalent(
    test_qt_today(qt, anydate(20180502)),
    anydate(20180502)
  )
})

test_that("qt.get()", {
  date <- anydate(20180504)
  expect_equal(
    purrr::map_dbl(tags, ~test_qt_get(qt, date, ., 0)),
    as.double(dt[DATE == date, seq_len(ncol(dt))[-1], with = FALSE])
  )
  date <- anydate(20180501)
  expect_equal(
    purrr::map_dbl(tags, ~test_qt_get(qt, date, ., 0)),
    as.double(dt[DATE == date, seq_len(ncol(dt))[-1], with = FALSE])
  )
  expect_equal(
    purrr::map_dbl(tags, ~test_qt_get(qt, anydate(20180507), ., 5)),
    as.double(dt[DATE == anydate(20180426), seq_len(ncol(dt))[-1], with = FALSE])
  )
  expect_error(
    test_qt_get(qt, anydate(20180503), "amount", -1),
    "delay must be a non-negative integer"
  )
  expect_equal(
    test_qt_get(qt, anydate(20180803), "volume", 5),
    NA_real_
  )
  expect_equal(
    test_qt_get(qt, anydate(20171231), "close", 0),
    NA_real_
  )
})

test_that("qt.ts_get()", {
  expect_equal(
    purrr::map(tags, ~test_qt_ts_get(qt, anydate(20180503), ., 3, 0)),
    purrr::map(tags, ~as.double(
      dt[DATE %in% anydate(c(20180427, 20180502, 20180503)),
         toupper(.), with = FALSE][[1L]]
    ))
  )
  expect_equal(
    purrr::map(tags, ~test_qt_ts_get(qt, anydate(20180503), ., 3, 5)),
    purrr::map(tags, ~as.double(
      dt[DATE %in% anydate(c(20180420, 20180423, 20180424)),
         toupper(.), with = FALSE][[1L]]
    ))
  )
  expect_equal(
    purrr::map(tags, ~test_qt_ts_get(qt, anydate(20180104), ., 5, 0)),
    purrr::map(tags, ~{
      res <- dt[DATE %in% anydate(c(20180102, 20180103, 20180104)),
                toupper(.), with = FALSE][[1L]]
      c(NA, NA, res)
    })
  )
  expect_equal(
    purrr::map(tags, ~test_qt_ts_get(qt, anydate(20180104), ., 5, 10)),
    purrr::map(tags, ~rep(NA_real_, 5))
  )
  expect_equal(
    test_qt_ts_get(qt, anydate(20180104), "close", 3, 3),
    rep(NA_real_, 3)
  )
  expect_equal(
    test_qt_ts_get(qt, anydate(20180104), "close", 3, 4),
    rep(NA_real_, 3)
  )
  expect_equal(
    test_qt_ts_get(qt, anydate(20180104), "close", 3, 2),
    c(NA, NA, 32.56)
  )
  expect_equal(
    test_qt_ts_get(qt, anydate(20180104), "close", 3, 1),
    c(NA, 32.56, 32.33)
  )
})

test_that("ts_qt_misc_getter", {
  date <- anydate(20180504)
  expect_equal(
    test_qt_get(qt, date, "tr", 0),
    {
      crt <- dt[J(date)]
      prev <- dt[J(date - 1), roll = TRUE]
      max(
        max(crt$HIGH - crt$LOW, abs(crt$HIGH - prev$CLOSE)),
        abs(crt$LOW - prev$CLOSE)
      )
    }
  )
  expect_equal(
    test_qt_get(qt, date, "ret", 0),
    {
      crt <- dt[J(date)]
      crt$CLOSE / crt$PCLOSE - 1.0
    }
  )
  expect_equal(
    test_qt_get(qt, date, "dtm", 0),
    {
      crt <- dt[J(date)]
      prev <- dt[J(date - 1), roll = TRUE]
      if (crt$OPEN <= prev$OPEN) {
        0
      } else {
        max(
          crt$HIGH - crt$OPEN,
          crt$OPEN - prev$OPEN
        )
      }
    }
  )
  expect_equal(
    test_qt_get(qt, date, "dbm", 0),
    {
      crt <- dt[J(date)]
      prev <- dt[J(date - 1), roll = TRUE]
      if (crt$OPEN >= prev$OPEN) {
        0
      } else {
        max(
          crt$OPEN - crt$LOW,
          crt$OPEN - prev$OPEN
        )
      }
    }
  )
  expect_equal(
    test_qt_get(qt, date, "hd", 0),
    {
      crt <- dt[J(date)]
      prev <- dt[J(date - 1), roll = TRUE]
      crt$HIGH - prev$HIGH
    }
  )
  expect_equal(
    test_qt_get(qt, date, "ld", 0),
    {
      crt <- dt[J(date)]
      prev <- dt[J(date - 1), roll = TRUE]
      prev$LOW - crt$LOW
    }
  )
})


test_that("ts_op_ts", {
  x <- c(-1.0, 0.0, 1.0, 2.0, NA, 3.0, NA, 0.0)
  y <- c(-0.1, 0.1, 0.0, 1.0, 1.2, NA, NA, 0.0)
  expect_equal(
    test_ts_op(x, y, "+"),
    x + y
  )
  expect_equal(
    test_ts_op(x, y, "-"),
    x - y
  )
  expect_equal(
    test_ts_op(x, y, "*"),
    x * y
  )
  expect_equal(
    test_ts_op(x, y, "/"),
    {
      res <- rep(NA_real_, length(x))
      flag <- !is.na(y) & y != 0.0
      res[flag] <- x[flag] / y[flag]
      res
    }
  )
})


test_that("ts_op_scalar", {
  x <- c(-1.0, 0.0, 1.0, 2.0, NA_real_, NA_real_, 0.0)
  y <- c(-1.3, 1.3, NA_real_, 0.0, -0.3, NA_real_, 0.0)
  expect_equal(
    purrr::map(y, ~test_ts_scalar_op(x, ., "+")),
    purrr::map(y, ~`+`(x, .))
  )
  expect_equal(
    purrr::map(y, ~test_ts_scalar_op(x, ., "-")),
    purrr::map(y, ~`-`(x, .))
  )
  expect_equal(
    purrr::map(y, ~test_ts_scalar_op(x, ., "*")),
    purrr::map(y, ~`*`(x, .))
  )
  # return NA if any NA or find zero in the denominator
  expect_equal(
    purrr::map(y, ~test_ts_scalar_op(x, ., "/")),
    purrr::map(y, ~{
      if (is.na(.) || . == 0.0) return(rep(NA_real_, length(x)))
      x / .
    })
  )
  # return FALSE if any NA
  expect_equal(
    purrr::map(y, ~test_ts_scalar_op(x, ., ">")),
    purrr::map(y, ~{
      res <- as.double(`>`(x, .))
      res[is.na(res)] <- 0.0
      res
    })
  )
  expect_equal(
    purrr::map(y, ~test_ts_scalar_op(x, ., "<")),
    purrr::map(y, ~{
      res <- as.double(`<`(x, .))
      res[is.na(res)] <- 0.0
      res
    })
  )
})
