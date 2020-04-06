#' @useDynLib techfactor, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom data.table data.table setkey setorder frank
#' @importFrom xts xts
NULL


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @importFrom anytime anydate
#' @export
anytime::anydate


#' @importFrom zoo index
#' @export
zoo::index


#' The sample quote data
#'
#' We include some small chunck of sample data `tf_quote` and
#' `tf_quotes` to illustrate what kind of data the package functions
#' are expected and they are used in the tests as well.
#'
#' @details
#'
#' * `tf_quote` is a China-A security `000002.SZ` (Wanke-A)'s
#' quote of 81 days in total, from 2018-01-02 ~ 2018-05-07.
#'
#' * `tf_quotes` is a list of quotes from 20140101 ~ 20180430,
#' containing `300333.SZ`,`601158.SH`,`002788.SZ`,`603101.SH`,
#' `600020.SH`,`601668.SH`,`600615.SH`,`002721.SZ`,`300517.SZ`,
#' `601567.SH`,`603477.SH`,`002297.SZ`,`600537.SH`,`603906.SH`,
#' `603183.SH`,`002884.SZ`,`300531.SZ`,`002641.SZ`,
#' `002851.SZ`,`600719.SH`.
#'
#' * The column `VWAP`
#' equals to `AMOUNT` / `VOLUME`. The columns `BMK_CLOSE` and
#' `BMK_OPEN` is the close and open price data of the index
#' _China-A All Shares_.
#'
#' @docType data
#' @keywords data
"tf_quote"

#' @rdname tf_quote
"tf_quotes"

