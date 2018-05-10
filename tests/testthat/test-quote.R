context("test-quote.R")

dt <- data.table::fread(
  "quote-sample.csv",
  colClasses = c("character", rep("double", 10))
)
dt[, DATE := as.Date(DATE)]

test_that("dates seq is correct", {
  qt <- tf_quotes_ptr(dt)
  from_to <- as.Date(c("2017-12-01", "2018-01-06"))
  expect_equal(
    tf_tdates(qt, from_to),
    as.Date(c("2018-01-02", "2018-01-03", "2018-01-04", "2018-01-05"))
  )

  from_to <- as.Date(c("2018-05-01", "2018-05-10"))
  expect_equal(
    tf_tdates(qt, from_to),
    as.Date(c("2018-05-02", "2018-05-03", "2018-05-04", "2018-05-07"))
  )
})
