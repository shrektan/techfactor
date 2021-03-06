context("test-quote.R")

tags <- c("pclose", "open", "high", "low", "close", "vwap",
          "volume", "amount", "bmk_close", "bmk_open")

data("tf_quote")
dt <- data.table::copy(tf_quote)
qt <- tf_quote_xptr(dt)

test_that("quote input will check valid price, date and volume", {
  dt_copy <- data.table::copy(dt)[1, DATE := NA_real_]
  expect_error(
    tf_quote_xptr(dt_copy),
    "dates mustn't contain NA"
  )
  dt_copy <- data.table::copy(dt)[1, OPEN := 0.0]
  expect_error(
    tf_quote_xptr(dt_copy),
    "open must be finite positive value or NA"
  )
  dt_copy <- data.table::copy(dt)[1, OPEN := NA_real_]
  expect_silent(
    tf_quote_xptr(dt_copy)
  )
  dt_copy <- data.table::copy(dt)[1, AMOUNT := 0.0]
  expect_silent(
    tf_quote_xptr(dt_copy)
  )
  dt_copy <- data.table::copy(dt)[1, AMOUNT := -1.0]
  expect_error(
    tf_quote_xptr(dt_copy),
    "amount must be finite non-negative value or NA"
  )
})

test_that("quote input must be a sorted tbl", {
  dt_copy <- data.table::copy(dt)
  setorder(dt_copy, -DATE)
  expect_error(
    tf_quote_xptr(dt_copy),
    "x must be a sorted date vector"
  )
  setorder(dt_copy, DATE)
  dt_copy[, CLOSE := NULL]
  expect_error(
    tf_quote_xptr(dt_copy),
    class = "Rcpp::index_out_of_bounds"
  )
})

test_that("quote.tdates()", {
  # from is lower than min
  from_to <- as.Date(c("2017-12-01", "2018-01-06"))
  expect_equivalent(
    tf_qt_tdates(qt, from_to),
    as.Date(c("2018-01-02", "2018-01-03", "2018-01-04", "2018-01-05"))
  )
  # to is larger than max
  from_to <- as.Date(c("2018-05-01", "2018-05-10"))
  expect_equivalent(
    tf_qt_tdates(qt, from_to),
    as.Date(c("2018-05-02", "2018-05-03", "2018-05-04", "2018-05-07"))
  )
  # from_to are tdates
  from_to <- anydate(c(20180427, 20180502))
  expect_equivalent(
    tf_qt_tdates(qt, from_to),
    from_to
  )
  # from_to are not tdates
  from_to <- anydate(c(20180501, 20180505))
  expect_equivalent(
    tf_qt_tdates(qt, from_to),
    anydate(c(20180502, 20180503, 20180504))
  )
  # from_to are all smaller than min
  expect_equivalent(
    tf_qt_tdates(qt, anydate(c(20160101, 20160101))),
    anydate(double())
  )
  # from_to are all larger than max
  expect_equivalent(
    tf_qt_tdates(qt, anydate(c(20190101, 20190101))),
    anydate(double())
  )
})


test_that("quote.set()", {
  # too small
  expect_error(
    tf_qt_today(qt, anydate(20180101)),
    "negative today_index_ -1"
  )
  # too large
  expect_error(
    tf_qt_today(qt, anydate(20180801)),
    "negative today_index_ -1"
  )
  # holiday
  expect_error(
    tf_qt_today(qt, anydate(20180501)),
    "negative today_index_ -1"
  )
  # first
  expect_equivalent(
    tf_qt_today(qt, anydate(20180102)),
    anydate(20180102)
  )
  # last
  expect_equivalent(
    tf_qt_today(qt, anydate(20180507)),
    anydate(20180507)
  )
  # middle
  expect_equivalent(
    tf_qt_today(qt, anydate(20180502)),
    anydate(20180502)
  )
})

test_that("qt.get()", {
  date <- anydate(20180504)
  expect_equal(
    vapply(tags, function(tag) tf_qt_get(qt, date, tag, 0), 0.0, USE.NAMES = FALSE),
    as.double(dt[DATE == date, seq_len(ncol(dt))[-1], with = FALSE])
  )
  date <- anydate(20180501)
  expect_equal(
    vapply(tags, function(tag) tf_qt_get(qt, date, tag, 0), 0.0, USE.NAMES = FALSE),
    as.double(dt[DATE == date, seq_len(ncol(dt))[-1], with = FALSE])
  )
  expect_equal(
    vapply(tags, function(tag) tf_qt_get(qt, anydate(20180507), tag, 5), 0.0, USE.NAMES = FALSE),
    as.double(dt[DATE == anydate(20180426), seq_len(ncol(dt))[-1], with = FALSE])
  )
  expect_error(
    tf_qt_get(qt, anydate(20180503), "amount", -1),
    "delay must be a non-negative integer"
  )
  expect_equal(
    tf_qt_get(qt, anydate(20180803), "volume", 5),
    NA_real_
  )
  expect_equal(
    tf_qt_get(qt, anydate(20171231), "close", 0),
    NA_real_
  )
})

test_that("qt.ts_get()", {
  expect_equal(
    lapply(tags, function(tag) tf_qt_ts_get(qt, anydate(20180503), tag, 3, 0)),
    lapply(tags, function(tag) as.double(
      dt[DATE %in% anydate(c(20180427, 20180502, 20180503)),
         toupper(tag), with = FALSE][[1L]]
    ))
  )
  expect_equal(
    lapply(tags, function(tag) tf_qt_ts_get(qt, anydate(20180503), tag, 3, 5)),
    lapply(tags, function(tag) as.double(
      dt[DATE %in% anydate(c(20180420, 20180423, 20180424)),
         toupper(tag), with = FALSE][[1L]]
    ))
  )
  expect_equal(
    lapply(tags, function(tag) tf_qt_ts_get(qt, anydate(20180104), tag, 5, 0)),
    lapply(tags, function(tag) {
      res <- dt[DATE %in% anydate(c(20180102, 20180103, 20180104)),
                toupper(tag), with = FALSE][[1L]]
      c(NA, NA, res)
    })
  )
  expect_equal(
    lapply(tags, function(tag) tf_qt_ts_get(qt, anydate(20180104), tag, 5, 10)),
    lapply(tags, function(tag) rep(NA_real_, 5))
  )
  expect_identical(
    tf_qt_ts_get(qt, anydate(20180104), "close", 3, 3),
    rep(NA_real_, 3)
  )
  expect_identical(
    tf_qt_ts_get(qt, anydate(20180104), "close", 3, 4),
    rep(NA_real_, 3)
  )
  expect_equal(
    tf_qt_ts_get(qt, anydate(20180104), "close", 3, 2),
    c(NA, NA, 32.56)
  )
  expect_equal(
    tf_qt_ts_get(qt, anydate(20180104), "close", 3, 1),
    c(NA, 32.56, 32.33)
  )
})

test_that("ts_qt_misc_getter", {
  date <- anydate(20180504)
  date2 <- anydate(20180502)
  expect_equal(
    c(tf_qt_get(qt, date, "tr", 0), tf_qt_get(qt, date, "tr", 2)),
    {
      crt <- dt[J(c(date, date2))]
      prev <- dt[J(c(date, date2) - 1), roll = TRUE]
      pmax(
        pmax(crt$HIGH - crt$LOW, abs(crt$HIGH - prev$CLOSE)),
        abs(crt$LOW - prev$CLOSE)
      )
    }
  )
  expect_equal(
    c(tf_qt_get(qt, date, "ret", 0), tf_qt_get(qt, date, "ret", 2)),
    {
      crt <- dt[J(c(date, date2))]
      crt$CLOSE / crt$PCLOSE - 1.0
    }
  )
  expect_equal(
    c(tf_qt_get(qt, date, "dtm", 0), tf_qt_get(qt, date, "dtm", 2)),
    {
      crt <- dt[J(c(date, date2))]
      prev <- dt[J(c(date, date2) - 1), roll = TRUE]
      ifelse(
        crt$OPEN <= prev$OPEN,
        0,
        pmax(
          crt$HIGH - crt$OPEN,
          crt$OPEN - prev$OPEN
        )
      )
    }
  )
  expect_equal(
    c(tf_qt_get(qt, date, "dbm", 0), tf_qt_get(qt, date, "dbm", 2)),
    {
      crt <- dt[J(c(date, date2))]
      prev <- dt[J(c(date, date2) - 1), roll = TRUE]
      ifelse(
        crt$OPEN >= prev$OPEN,
        0,
        pmax(
          crt$OPEN - crt$LOW,
          crt$OPEN - prev$OPEN
        )
      )
    }
  )
  expect_equal(
    c(tf_qt_get(qt, date, "hd", 0), tf_qt_get(qt, date, "hd", 2)),
    {
      crt <- dt[J(c(date, date2))]
      prev <- dt[J(c(date, date2) - 1), roll = TRUE]
      crt$HIGH - prev$HIGH
    }
  )
  expect_equal(
    c(tf_qt_get(qt, date, "ld", 0), tf_qt_get(qt, date, "ld", 2)),
    {
      crt <- dt[J(c(date, date2))]
      prev <- dt[J(c(date, date2) - 1), roll = TRUE]
      prev$LOW - crt$LOW
    }
  )
})


test_that("ts_qt_misc_ts_getter", {
  date <- anydate(20180504)
  vars <- c("tr", "ret", "dtm", "dbm", "hd", "ld")
  lapply(vars, function(var) {
    expect_equal(
      tf_qt_ts_get(qt, date, var, 5, 2),
      vapply(4:0, function(.) tf_qt_get(qt, date, var, 2 + .), 0.0, USE.NAMES = FALSE)
    )
  })
})


test_that("ts_op_ts", {
  x <- c(-1.0, 0.0, 1.0, 2.0, NA, 3.0, NA, 0.0, -3.0, 2.3, 0.0)
  y <- c(-0.1, 0.1, 0.0, 1.0, 1.2, NA, NA, 0.0, 2.3, -3.0, -2.0)
  expect_equal(
    tf_ts_op(x, y, "+"),
    x + y
  )
  expect_equal(
    tf_ts_op(x, y, "-"),
    x - y
  )
  expect_equal(
    tf_ts_op(x, y, "*"),
    x * y
  )
  expect_equal(
    tf_ts_op(x, y, "/"),
    {
      res <- rep(NA_real_, length(x))
      flag <- !is.na(y) & y != 0.0
      res[flag] <- x[flag] / y[flag]
      res
    }
  )
  expect_equal(
    tf_ts_op(x, y, "^"),
    {
      res <- x ^ y
      res[!is.finite(res)] <- NA_real_
      res
    }
  )
  expect_false(is.nan(tf_ts_op(-4, -0.5, "^")))
  expect_equal(
    tf_ts_op(x, y, ">"),
    as.double(x > y)
  )
  expect_equal(
    tf_ts_op(x, y, "=="),
    as.double(x == y)
  )
  expect_equal(
    tf_ts_op(x, y, "<"),
    as.double(x < y)
  )
  expect_equal(
    tf_ts_op(x, y, "pmax"),
    pmax(x, y)
  )
  expect_equal(
    tf_ts_op(x, y, "pmin"),
    pmin(x, y)
  )
})


test_that("ts_op_scalar", {
  x <- c(-1.0, 0.0, 1.0, 2.0, NA_real_, NA_real_, 0.0)
  y <- c(-1.3, 1.3, NA_real_, 0.0, -0.3, NA_real_, 0.0)
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "+")),
    lapply(y, function(.) `+`(x, .))
  )
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "-")),
    lapply(y, function(.) `-`(x, .))
  )
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "*")),
    lapply(y, function(.) `*`(x, .))
  )
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "^")),
    lapply(y, function(.) {
      res <- `^`(x, .)
      res[is.infinite(res)] <- NA_real_
      res
    })
  )
  # return NA if any NA or find zero in the denominator
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "/")),
    lapply(y, function(.) {
      if (is.na(.) || . == 0.0) return(rep(NA_real_, length(x)))
      x / .
    })
  )
  # return FALSE if any NA
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., ">")),
    lapply(y, function(.) {
      res <- as.double(`>`(x, .))
      res[is.na(res)] <- 0.0
      res
    })
  )
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "<")),
    lapply(y, function(.) {
      res <- as.double(`<`(x, .))
      res[is.na(res)] <- 0.0
      res
    })
  )
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "==")),
    lapply(y, function(.) {
      res <- as.double(`==`(x, .))
      res[is.na(res)] <- 0.0
      res
    })
  )
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "pmin")),
    lapply(y, function(.) pmin(x, .))
  )
  expect_equal(
    lapply(y, function(.) tf_ts_scalar_op(x, ., "pmax")),
    lapply(y, function(.) pmax(x, .))
  )
})

test_that("ts() algo works expectly", {
  date <- anydate(20180502)
  expect_error(tf_ts(qt, date, 0), "must be positive")
  res <- tf_ts(qt, date, 1)
  expect_length(res, 1)
  expect_equal(
    res,
    {
      rowindex <- dt[, which(DATE == date)]
      tail(frank(dt[(-4:0 + rowindex), VOLUME]), 1)
    }
  )
  res <- tf_ts(qt, date, 3)
  expect_length(res, 3)
  expect_equal(
    res,
    {
      rowindex <- dt[, which(DATE == date)]
      c(
        tail(frank(dt[(-6:-2 + rowindex), VOLUME]), 1),
        tail(frank(dt[(-5:-1 + rowindex), VOLUME]), 1),
        tail(frank(dt[(-4:0 + rowindex), VOLUME]), 1)
      )
    }
  )
})
