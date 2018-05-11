tf_quote <- data.table::fread(
  "data-raw/quote-sample.csv",
  colClasses = c("character", rep("double", 10))
)
tf_quote[, DATE := as.Date(DATE)]
data.table::setkey(tf_quote, DATE)
usethis::use_data(tf_quote, internal = FALSE)
