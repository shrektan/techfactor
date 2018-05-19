
# tf_quote ----------------------------------------------------------------

tf_quote <- data.table::fread(
  "data-raw/quote-sample.csv",
  colClasses = c("character", rep("double", 10))
)
tf_quote[, DATE := as.Date(DATE)]
data.table::setkey(tf_quote, DATE)
usethis::use_data(tf_quote, internal = FALSE, overwrite = FALSE)


# tf_quotes --------------------------------------------------------------


tf_quotes <- local({
  raw <- GCAMCQT::ashare_quote(c(201401, 201804), FALSE)
  # sec <- sample(unique(raw$INNERCODE), 40)
  # sec <- setNames(sec, GCAMCQT::to_symbol_ashare(sec))
  # sec <- sec[!is.na(names(sec))]
  # length(sec)
  # sec <- sec[1:20]
  sec <- c(16504, 10033, 35005, 34798, 1145, 7033, 1824, 17481, 35232,
           13443, 124271, 7359, 1703, 49338, 81850, 61421, 36226, 14222,
           16476, 1944)
  sec <- setNames(sec, GCAMCQT::to_symbol_ashare(sec))
  bmk <- raw[INNERCODE == -1, .(TRADINGDAY, CLOSEPRICE, OPENPRICE)]
  data.table::setkey(bmk, TRADINGDAY)
  dat <- purrr::map(sec, ~{
    quote <- raw[INNERCODE == ., .(DATE = TRADINGDAY, PCLOSE = PREVCLOSEPRICE, OPEN = OPENPRICE,
                                   HIGH = HIGHPRICE, LOW = LOWPRICE, CLOSE = CLOSEPRICE,
                                   VWAP, VOLUME = TURNOVERVOLUME, AMOUNT = TURNOVERVALUE)]
    quote[, c("BMK_CLOSE", "BMK_OPEN") := bmk[J(quote$DATE), .(CLOSEPRICE, OPENPRICE)]]
    quote
  })
  dat
})

usethis::use_data(tf_quotes, internal = FALSE, overwrite = FALSE)

