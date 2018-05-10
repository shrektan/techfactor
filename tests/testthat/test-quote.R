context("test-quote.R")

tags <- c("pclose", "open", "high", "low", "close", "vwap",
          "volume", "amount", "bmk_close", "bmk_open")
dt <- data.table::fread(
  "quote-sample.csv",
  colClasses = c("character", rep("double", 10))
)
dt[, DATE := as.Date(DATE)]
qt <- tf_quotes_ptr(dt)

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
