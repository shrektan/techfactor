#' @useDynLib GCAMCTF, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import data.table
NULL


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @importFrom anytime anydate
#' @export
anytime::anydate


#' The sample quote data used in GCAMCTF
#'
#' The data is a realistic China-A security quote of 81 days
#' in total, from 2018-01-02 ~ 2018-05-07. The column `VWAP`
#' equals to `AMOUNT` / `VOLUME`. The columns `BMK_CLOSE` and
#' `BMK_OPEN` is the close and open price data of the index
#' _China-A All Shares_.
#'
#' @name tf_quote
#' @source The Gildata database
#' @docType data
#' @keywords data
#' @usage data(tf_quote)
NULL
