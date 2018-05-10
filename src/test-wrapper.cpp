#include <Rcpp.h>
#include "algo.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::newDateVector test_qt_tdates(SEXP quotes_ptr, const Rcpp::newDateVector from_to)
{
  Rcpp::XPtr<Quotes> xptr {quotes_ptr};
  const auto dates = xptr->tdates(from_to);
  return Rcpp::wrap(dates);
}


// [[Rcpp::export]]
Rcpp::Date test_qt_today(SEXP quotes_ptr, const Rcpp::Date today)
{
  Rcpp::XPtr<Quotes> xptr {quotes_ptr};
  xptr->set(today);
  return xptr->today();
}
