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

const std::map<std::string, std::function<double(const Quotes& qt, const int delay)>> tag_map {
  {"pclose", [](const Quotes& qt, const int delay) { return qt.pclose(delay); }},
  {"open", [](const Quotes& qt, const int delay) { return qt.open(delay); }},
  {"high", [](const Quotes& qt, const int delay) { return qt.high(delay); }},
  {"low", [](const Quotes& qt, const int delay) { return qt.low(delay); }},
  {"close", [](const Quotes& qt, const int delay) { return qt.close(delay); }},
  {"volume", [](const Quotes& qt, const int delay) { return qt.volume(delay); }},
  {"amount", [](const Quotes& qt, const int delay) { return qt.amount(delay); }},
  {"bmk_close", [](const Quotes& qt, const int delay) { return qt.bmk_close(delay); }},
  {"bmk_open", [](const Quotes& qt, const int delay) { return qt.bmk_open(delay); }}
};


// [[Rcpp::export]]
double test_qt_get(SEXP quotes_ptr, const Rcpp::Date today,
                   const std::string tag, const int delay)
{
  if (tag_map.count(tag) == 0) Rcpp::stop("tag %s is not valid.", tag);
  Rcpp::XPtr<Quotes> xptr {quotes_ptr};
  xptr->set(today);
  return tag_map.at(tag)(*xptr, delay);
}


const std::map<
  std::string,
  std::function<Timeseries(const Quotes& qt, const int n, const int delay)>
> tag_ts_map {
  {"pclose", [](const Quotes& qt, const int n, const int delay) { return qt.ts_pclose(n, delay); }},
  {"open", [](const Quotes& qt, const int n, const int delay) { return qt.ts_open(n, delay); }},
  {"high", [](const Quotes& qt, const int n, const int delay) { return qt.ts_high(n, delay); }},
  {"low", [](const Quotes& qt, const int n, const int delay) { return qt.ts_low(n, delay); }},
  {"close", [](const Quotes& qt, const int n, const int delay) { return qt.ts_close(n, delay); }},
  {"volume", [](const Quotes& qt, const int n, const int delay) { return qt.ts_volume(n, delay); }},
  {"amount", [](const Quotes& qt, const int n, const int delay) { return qt.ts_amount(n, delay); }},
  {"bmk_close", [](const Quotes& qt, const int n, const int delay) { return qt.ts_bmk_close(n, delay); }},
  {"bmk_open", [](const Quotes& qt, const int n, const int delay) { return qt.ts_bmk_open(n, delay); }}
};


// [[Rcpp::export]]
Timeseries test_qt_ts_get(
    SEXP quotes_ptr, const Rcpp::Date today, const std::string tag,
    const int n, const int delay)
{
  if (tag_ts_map.count(tag) == 0) Rcpp::stop("tag %s is not valid.", tag);
  Rcpp::XPtr<Quotes> xptr {quotes_ptr};
  xptr->set(today);
  return tag_ts_map.at(tag)(*xptr, n, delay);
}
