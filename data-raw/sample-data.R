
# tf_quote ----------------------------------------------------------------

tf_quote <- data.table::fread(
  "data-raw/quote-sample.csv",
  colClasses = c("DATE" = "Date")
)
tf_quote[, DATE := as.Date(DATE)]
data.table::setkey(tf_quote, DATE)
usethis::use_data(tf_quote, internal = FALSE, overwrite = FALSE)


# tf_quotes --------------------------------------------------------------


tf_quotes <- data.table::fread(
  "data-raw/quotes-sample.csv",
  colClasses = c("DATE" = "Date")
)
tf_quotes <- split(tf_quotes, by = "SYMBOL", keep.by = FALSE)

usethis::use_data(tf_quotes, internal = FALSE, overwrite = FALSE)

