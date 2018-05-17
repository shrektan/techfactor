#include <Rcpp.h>
#include "algo.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::newDateVector test_qt_tdates(SEXP quote_ptr, const Rcpp::newDateVector from_to)
{
  Rcpp::XPtr<Quote_raw> xptr {quote_ptr};
  const auto dates = xptr->tdates(from_to);
  return Rcpp::wrap(dates);
}


// [[Rcpp::export]]
Rcpp::Date test_qt_today(SEXP quote_ptr, const Rcpp::Date today)
{
  Rcpp::XPtr<Quote_raw> xptr {quote_ptr};
  Quote qt {*xptr, int(today)};
  return qt.today();
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
  {"ret", [](const Quote& qt, const int delay) { return qt.ret(delay); }},
  {"hd", [](const Quote& qt, const int delay) { return qt.hd(delay); }},
  {"ld", [](const Quote& qt, const int delay) { return qt.ld(delay); }},
  {"dtm", [](const Quote& qt, const int delay) { return qt.dtm(delay); }},
  {"dbm", [](const Quote& qt, const int delay) { return qt.dbm(delay); }}
};


// [[Rcpp::export]]
double test_qt_get(SEXP quote_ptr, const Rcpp::Date today,
                   const std::string tag, const int delay)
{
  if (tag_map.count(tag) == 0) Rcpp::stop("tag %s is not valid.", tag);
  Rcpp::XPtr<Quote_raw> xptr {quote_ptr};
  Quote qt {*xptr, int(today)};
  return tag_map.at(tag)(qt, delay);
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
  {"bmk_open", [](const Quote& qt, const int n, const int delay) { return qt.ts_bmk_open(n, delay); }},
  {"tr", [](const Quote& qt, const int n, const int delay) { return qt.ts_tr(n, delay); }},
  {"hd", [](const Quote& qt, const int n, const int delay) { return qt.ts_hd(n, delay); }},
  {"ld", [](const Quote& qt, const int n, const int delay) { return qt.ts_ld(n, delay); }},
  {"ret", [](const Quote& qt, const int n, const int delay) { return qt.ts_ret(n, delay); }},
  {"dtm", [](const Quote& qt, const int n, const int delay) { return qt.ts_dtm(n, delay); }},
  {"dbm", [](const Quote& qt, const int n, const int delay) { return qt.ts_dbm(n, delay); }}
};


// [[Rcpp::export]]
Timeseries test_qt_ts_get(
    SEXP quote_ptr, const Rcpp::Date today, const std::string tag,
    const int n, const int delay)
{
  if (tag_ts_map.count(tag) == 0) Rcpp::stop("tag %s is not valid.", tag);
  Rcpp::XPtr<Quote_raw> xptr {quote_ptr};
  Quote qt {*xptr, int(today)};
  return tag_ts_map.at(tag)(qt, n, delay);
}


const std::map<
  std::string,
  std::function<Timeseries(const Timeseries&, const Timeseries&)>
> op_map
{
  {"+", [](const Timeseries& x, const Timeseries& y) { return x + y; }},
  {"-", [](const Timeseries& x, const Timeseries& y) { return x - y; }},
  {"*", [](const Timeseries& x, const Timeseries& y) { return x * y; }},
  {"/", [](const Timeseries& x, const Timeseries& y) { return x / y; }},
  {"^", [](const Timeseries& x, const Timeseries& y) { return pow(x, y); }},
  {">", [](const Timeseries& x, const Timeseries& y) { return x > y; }},
  {"<", [](const Timeseries& x, const Timeseries& y) { return x < y; }},
  {"==", [](const Timeseries& x, const Timeseries& y) { return x == y; }},
  {"pmin", [](const Timeseries& x, const Timeseries& y) { return pmin(x, y); }},
  {"pmax", [](const Timeseries& x, const Timeseries& y) { return pmax(x, y); }}
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
  {"^", [](const Timeseries& x, const double y) { return pow(x, y); }},
  {">",
   [](const Timeseries& x, const double y) {
     const auto bool_res = x > y;
     Timeseries res;
     for (auto x : bool_res) res.push_back(x);
     return res;
   }
  },
  {"<",
   [](const Timeseries& x, const double y) {
     const auto bool_res = x < y;
     Timeseries res;
     for (auto x : bool_res) res.push_back(x);
     return res;
   }
  },
  {"==",
   [](const Timeseries& x, const double y) {
     const auto bool_res = x == y;
     Timeseries res;
     for (auto x : bool_res) res.push_back(x);
     return res;
   }
  },
  {"pmin", [](const Timeseries& x, const double y) { return pmin(x, y); }},
  {"pmax", [](const Timeseries& x, const double y) { return pmax(x, y); }}
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
  Rcpp::XPtr<Quote_raw> xptr {quote_ptr};
  Quote qt {*xptr, int(today)};
  auto volumn = [&qt] (const int delay) {
    return tsrank(qt.ts_volume(5, delay));
  };
  return ts<double>(n, volumn);
}


Panel test_create_panel(const Rcpp::List x)
{
  const int n = x.size();
  Panel res;
  for (int i = 0; i < n; ++i) {
    Rcpp::NumericVector r_elem = x[i];
    Timeseries elem = Rcpp::as<std::vector<double>>(r_elem);
    res.push_back(elem);
  }
  return res;
}


// [[Rcpp::export]]
void test_assert_valid_panel(const Rcpp::List x)
{
  auto panel = test_create_panel(x);
  assert_valid(panel);
}


// [[Rcpp::export]]
Rcpp::NumericVector test_panel_sum(const Rcpp::List x)
{
  auto fun = [](const Timeseries& ts) {
    return sum(ts);
  };
  auto panel = test_create_panel(x);
  return Rcpp::wrap(apply(panel, fun));
}
