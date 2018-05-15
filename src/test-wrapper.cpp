#include <Rcpp.h>
#include "algo.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::newDateVector test_qt_tdates(SEXP quote_ptr, const Rcpp::newDateVector from_to)
{
  Rcpp::XPtr<Quote> xptr {quote_ptr};
  const auto dates = xptr->tdates(from_to);
  return Rcpp::wrap(dates);
}


// [[Rcpp::export]]
Rcpp::Date test_qt_today(SEXP quote_ptr, const Rcpp::Date today)
{
  Rcpp::XPtr<Quote> xptr {quote_ptr};
  xptr->set(today);
  return xptr->today();
}

const std::map<std::string, std::function<double(const Quote, const int)>> tag_map
{
  {"pclose", [](const Quote& qt, const int delay) { return qt.pclose(delay); }},
  {"open", [](const Quote& qt, const int delay) { return qt.open(delay); }},
  {"high", [](const Quote& qt, const int delay) { return qt.high(delay); }},
  {"low", [](const Quote& qt, const int delay) { return qt.low(delay); }},
  {"close", [](const Quote& qt, const int delay) { return qt.close(delay); }},
  {"vwap", [](const Quote& qt, const int delay) { return qt.vwap(delay); }},
  {"volume", [](const Quote& qt, const int delay) { return qt.volume(delay); }},
  {"amount", [](const Quote& qt, const int delay) { return qt.amount(delay); }},
  {"bmk_close", [](const Quote& qt, const int delay) { return qt.bmk_close(delay); }},
  {"bmk_open", [](const Quote& qt, const int delay) { return qt.bmk_open(delay); }},
  {"tr", [](const Quote& qt, const int delay) { return qt.tr(delay); }},
  {"ret", [](const Quote& qt, const int delay) { return qt.ret(); }},
  {"hd", [](const Quote& qt, const int delay) { return qt.hd(); }},
  {"ld", [](const Quote& qt, const int delay) { return qt.ld(); }},
  {"dtm", [](const Quote& qt, const int delay) { return qt.dtm(); }},
  {"dbm", [](const Quote& qt, const int delay) { return qt.dbm(); }}
};


// [[Rcpp::export]]
double test_qt_get(SEXP quote_ptr, const Rcpp::Date today,
                   const std::string tag, const int delay)
{
  if (tag_map.count(tag) == 0) Rcpp::stop("tag %s is not valid.", tag);
  Rcpp::XPtr<Quote> xptr {quote_ptr};
  xptr->set(today);
  return tag_map.at(tag)(*xptr, delay);
}


const std::map<
  std::string,
  std::function<Timeseries(const Quote, const int, const int)>
> tag_ts_map
{
  {"pclose", [](const Quote& qt, const int n, const int delay) { return qt.ts_pclose(n, delay); }},
  {"open", [](const Quote& qt, const int n, const int delay) { return qt.ts_open(n, delay); }},
  {"high", [](const Quote& qt, const int n, const int delay) { return qt.ts_high(n, delay); }},
  {"low", [](const Quote& qt, const int n, const int delay) { return qt.ts_low(n, delay); }},
  {"close", [](const Quote& qt, const int n, const int delay) { return qt.ts_close(n, delay); }},
  {"vwap", [](const Quote& qt, const int n, const int delay) { return qt.ts_vwap(n, delay); }},
  {"volume", [](const Quote& qt, const int n, const int delay) { return qt.ts_volume(n, delay); }},
  {"amount", [](const Quote& qt, const int n, const int delay) { return qt.ts_amount(n, delay); }},
  {"bmk_close", [](const Quote& qt, const int n, const int delay) { return qt.ts_bmk_close(n, delay); }},
  {"bmk_open", [](const Quote& qt, const int n, const int delay) { return qt.ts_bmk_open(n, delay); }}
};


// [[Rcpp::export]]
Timeseries test_qt_ts_get(
    SEXP quote_ptr, const Rcpp::Date today, const std::string tag,
    const int n, const int delay)
{
  if (tag_ts_map.count(tag) == 0) Rcpp::stop("tag %s is not valid.", tag);
  Rcpp::XPtr<Quote> xptr {quote_ptr};
  xptr->set(today);
  return tag_ts_map.at(tag)(*xptr, n, delay);
}


const std::map<
  std::string,
  std::function<Timeseries(const Timeseries&, const Timeseries&)>
> op_map
{
  {"+", [](const Timeseries& x, const Timeseries& y) { return x + y; }},
  {"-", [](const Timeseries& x, const Timeseries& y) { return x - y; }},
  {"*", [](const Timeseries& x, const Timeseries& y) { return x * y; }},
  {"/", [](const Timeseries& x, const Timeseries& y) { return x / y; }}
};


// [[Rcpp::export]]
Timeseries test_ts_op(const Timeseries& x, const Timeseries& y, const std::string op)
{
  if (op_map.count(op) == 0) Rcpp::stop("op %s is not valid.", op);
  return op_map.at(op)(x, y);
}


const std::map<
  std::string,
  std::function<Timeseries(const Timeseries&, const double)>
> op_scalar_map
{
  {"+", [](const Timeseries& x, const double y) { return x + y; }},
  {"-", [](const Timeseries& x, const double y) { return x - y; }},
  {"*", [](const Timeseries& x, const double y) { return x * y; }},
  {"/", [](const Timeseries& x, const double y) { return x / y; }},
  {">",
   [](const Timeseries& x, const double y) {
     const auto bool_res = x > y;
     Timeseries res(bool_res.size());
     std::transform(bool_res.cbegin(), bool_res.cend(), res.begin(),
                    [](const bool v) { return double(v); });
     return res;
   }
  },
  {"<",
   [](const Timeseries& x, const double y) {
     const auto bool_res = x < y;
     Timeseries res(bool_res.size());
     std::transform(bool_res.cbegin(), bool_res.cend(), res.begin(),
                    [](const bool v) { return double(v); });
     return res;
   }
  }
};


// [[Rcpp::export]]
Timeseries test_ts_scalar_op(const Timeseries& x, const double y, const std::string op)
{
  if (op_scalar_map.count(op) == 0) Rcpp::stop("op %s is not valid.", op);
  return op_scalar_map.at(op)(x, y);
}


// [[Rcpp::export]]
Timeseries test_ts(SEXP quote_ptr, const Rcpp::Date today, const int n)
{
  Rcpp::XPtr<Quote> xptr {quote_ptr};
  xptr->set(today);
  auto volumn = [xptr] (const int delay) {
    return tsrank(xptr->ts_volume(5, delay));
  };
  return ts<double>(n, volumn);
}