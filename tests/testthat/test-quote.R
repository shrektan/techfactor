context("test-quote.R")

dt <- data.table::fread(
  "quote-sample.csv",
  colClasses = c("character", rep("double", 10))
)
dt[, DATE := as.Date(DATE)]
qt <- tf_quotes_ptr(dt)

test_that("dates seq is correct", {
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
  from_to <- anytime::anydate(c(20180427, 20180502))
  expect_equivalent(
    test_qt_tdates(qt, from_to),
    from_to
  )

  # from_to are not tdates
  from_to <- anytime::anydate(c(20180501, 20180505))
  expect_equivalent(
    test_qt_tdates(qt, from_to),
    anytime::anydate(c(20180502, 20180503, 20180504))
  )

  # from_to are all smaller than min
  expect_equivalent(
    test_qt_tdates(qt, anytime::anydate(c(20160101, 20160101))),
    anytime::anydate(double())
  )

  # from_to are all larger than max
  expect_equivalent(
    test_qt_tdates(qt, anytime::anydate(c(20190101, 20190101))),
    anytime::anydate(double())
  )
})
