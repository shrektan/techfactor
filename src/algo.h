#ifndef __GCAMCTF_ALGO__
#define __GCAMCTF_ALGO__

#include <Rcpp.h>
#include <map>
#include "GCAMCTF_types.h"

template <typename T>
void assert_same_size(const T& x, const T& y)
{
  const int x_size = x.size();
  const int y_size = y.size();
  if (x_size != y_size) Rcpp::stop(
    "The length of x (%d) and y (%d) must be the same.",
    x_size, y_size
  );
}

void assert_valid(const Rcpp::newDateVector from_to);
bool any_na(const Timeseries& x);
void assert_no_na(const Timeseries& x);
Timeseries na_vector(const int length);

double delta(const Timeseries& x);
double corr(const Timeseries& x, const Timeseries& y);
Timeseries rank(const Timeseries& x);
double sum(const Timeseries& x);
double stdev(const Timeseries& x);
double mean(const Timeseries& x);
double tsmin(const Timeseries& x);
double tsmax(const Timeseries& x);
double tsrank(const Timeseries& x);
double sign(const double x);
double sma(const Timeseries& x, const int m);
double wma(const Timeseries& x);
double decaylinear(const Timeseries& x);
Timeseries sequence(const int n);
Timeseries sumac(const Timeseries& x);
Timeseries log(const Timeseries& x);
Timeseries abs(const Timeseries& x);
double covariance(const Timeseries& x, const Timeseries& y);
double prod(const Timeseries& x);
double count(const std::vector<bool>& x);
double regbeta(const Timeseries& x, const Timeseries& y);
double regresi(const Timeseries& x, const Timeseries& y);
Timeseries filter(const Timeseries& x, const std::vector<bool>& cond);
double highday(const Timeseries& x);
double lowday(const Timeseries& x);


template<typename T>
std::vector<T> ts(const int n, std::function<T(const int delay)> fun)
{
  if (n < 1) Rcpp::stop("n (%d) must be positive.", n);
  std::vector<T> res;
  for (int i {n - 1}; i >= 0; --i) {
    res.push_back(fun(i));
  }
  return res;
}


inline Timeseries col(const Rcpp::DataFrame tbl, const std::string field)
{
  Rcpp::NumericVector res = tbl[field];
  return Rcpp::as<Timeseries>(res);
}


inline std::vector<RDate> col_date(const Rcpp::DataFrame tbl, const std::string field)
{
  Rcpp::newDateVector res = tbl[field];
  return Rcpp::as<std::vector<RDate>>(res);
}

void assert_sorted(const std::vector<RDate>& x);

struct Quote_raw {
  Quote_raw() = default;
  explicit Quote_raw(const Rcpp::DataFrame tbl)
    : dates { col_date(tbl, "DATE") },
      pclose { col(tbl, "PCLOSE") },
      open { col(tbl, "OPEN") },
      high { col(tbl, "HIGH") },
      low { col(tbl, "LOW") },
      close { col(tbl, "CLOSE") },
      vwap { col(tbl, "VWAP") },
      volume { col(tbl, "VOLUME") },
      amount { col(tbl, "AMOUNT") },
      bmk_close { col(tbl, "BMK_CLOSE") },
      bmk_open { col(tbl, "BMK_OPEN") }
  {
        assert_sorted(dates);
  }
  std::vector<RDate> dates;
  Timeseries pclose;
  Timeseries open;
  Timeseries high;
  Timeseries low;
  Timeseries close;
  Timeseries vwap;
  Timeseries volume;
  Timeseries amount;
  Timeseries bmk_close;
  Timeseries bmk_open;
  std::vector<RDate> tdates(const Rcpp::newDateVector from_to) const
  {
    assert_valid(from_to);
    auto begin = std::lower_bound(dates.cbegin(), dates.cend(), int(from_to[0]));
    auto end = std::lower_bound(dates.cbegin(), dates.cend(), int(from_to[1]));
    if (end != dates.cend() && *end == int(from_to[1])) ++end;
    std::vector<RDate> res;
    std::copy(begin, end, std::back_inserter(res));
    return res;
  }
};



class Quote {
public:
  Quote() = default;
  explicit Quote (const Quote_raw& raw, const RDate today)
    : raw_ { raw }, today_index_ { match_(today) } { }

  // this constructor is supposed to be called only by clock_back()
  explicit Quote (const int today_index, const Quote_raw& raw)
    : raw_ { raw }, today_index_ { today_index } { }

  Quote clock_back(const int days) const
  {
    if (days < 0) Rcpp::stop("days (%d) must be non negative.", days);
    return Quote(today_index_ - days, raw_);
  }
  // it's mainly for tests
  RDate today()
  {
    if (today_index_ < 0) Rcpp::stop("negative today_index_ %d.", today_index_);
    return raw_.dates[today_index_];
  }
  int today_index() const noexcept { return today_index_; }

  double pclose(const int delay = 0) const { return get_(raw_.pclose, delay); }
  double open(const int delay = 0) const { return get_(raw_.open, delay); }
  double high(const int delay = 0) const { return get_(raw_.high, delay); }
  double low(const int delay = 0) const { return get_(raw_.low, delay); }
  double close(const int delay = 0) const { return get_(raw_.close, delay); }
  double vwap(const int delay = 0) const { return get_(raw_.vwap, delay); }
  double volume(const int delay = 0) const { return get_(raw_.volume, delay); }
  double amount(const int delay = 0) const { return get_(raw_.amount, delay); }
  double bmk_close(const int delay = 0) const { return get_(raw_.bmk_close, delay); }
  double bmk_open(const int delay = 0) const { return get_(raw_.bmk_open, delay); }

  Timeseries ts_pclose(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.pclose, n, delay);
  }
  Timeseries ts_open(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.open, n, delay);
  }
  Timeseries ts_high(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.high, n, delay);
  }
  Timeseries ts_low(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.low, n, delay);
  }
  Timeseries ts_close(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.close, n, delay);
  }
  Timeseries ts_vwap(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.vwap, n, delay);
  }
  Timeseries ts_volume(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.volume, n, delay);
  }
  Timeseries ts_amount(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.amount, n, delay);
  }
  Timeseries ts_bmk_close(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.bmk_close, n, delay);
  }
  Timeseries ts_bmk_open(const int n, const int delay = 0) const
  {
    return ts_get_(raw_.bmk_open, n, delay);
  }

  double hd(const int delay = 0) const { return high(delay) - high(delay + 1); }
  double ld(const int delay = 0) const { return low(delay + 1) - low(delay); }
  double tr(const int delay = 0) const
  {
    if (!in_bound_(delay + 1)) return NA_REAL;
    return std::max(
      std::max(high(delay) - low(delay), std::abs(high(delay) - close(delay + 1))),
      std::abs(low(delay) - close(delay + 1))
    );
  }
  double ret(const int delay = 0) const
  {
    if (pclose(delay) == 0.0) return NA_REAL;
    return close(delay) / pclose(delay) - 1.0;
  }
  double dtm(const int delay = 0) const
  {
    if (!in_bound_(delay + 1)) return NA_REAL;
    return open(delay) <= open(delay + 1) ? 0.0 :
      std::max(high(delay) - open(delay), open(delay) - open(delay + 1));
  }
  double dbm(const int delay = 0) const
  {
    if (!in_bound_(delay + 1)) return NA_REAL;
    return open(delay) >= open(delay + 1) ? 0.0 :
      std::max(open(delay) - low(delay), open(delay) - open(delay + 1));
  }

  Timeseries ts_hd(const int n, const int delay = 0) const
  {
    auto fun = [delay] (const Quote& qt) {
      return qt.hd(delay);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_ld(const int n, const int delay = 0) const
  {
    auto fun = [delay] (const Quote& qt) {
      return qt.ld(delay);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_tr(const int n, const int delay = 0) const
  {
    auto fun = [delay] (const Quote& qt) {
      return qt.tr(delay);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_ret(const int n, const int delay = 0) const
  {
    auto fun = [delay] (const Quote& qt) {
      return qt.ret(delay);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_dtm(const int n, const int delay = 0) const
  {
    auto fun = [delay] (const Quote& qt) {
      return qt.dtm(delay);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_dbm(const int n, const int delay = 0) const
  {
    auto fun = [delay] (const Quote& qt) {
      return qt.dbm(delay);
    };
    return ts<double>(n, fun);
  }

  std::vector<RDate> tdates(const Rcpp::newDateVector from_to) const
  {
    return raw_.tdates(from_to);
  }

  template<typename T>
  std::vector<T> ts(const int n, std::function<T(const Quote&)> fun) const
  {
    if (n < 1) Rcpp::stop("n (%d) must be positive.", n);
    std::vector<T> res;
    for (int i {n - 1}; i >= 0; --i) {
      res.push_back(fun(clock_back(i)));
    }
    return res;
  }

private:
  const Quote_raw& raw_;
  int today_index_ {0};
  int match_(const RDate today) noexcept
  {
    auto iter = std::lower_bound(raw_.dates.cbegin(), raw_.dates.cend(), today);
    if (iter == raw_.dates.cend() || *iter > today) {
      return -1;
    } else {
      return std::distance(raw_.dates.cbegin(), iter);
    }
  }
  int delayed_index_(const int delay) const
  {
    assert_valid_(delay);
    return today_index_ - delay;
  }
  double get_(const Timeseries& ts, const int delay) const
  {
    if (!in_bound_(delay)) return NA_REAL;
    return ts[delayed_index_(delay)];
  }
  Timeseries ts_get_(const Timeseries& ts, const int n, const int delay) const
  {
    int begin = delayed_index_(delay + n - 1);
    int end = delayed_index_(delay);
    Timeseries res;
    // how many NA: end < 0 => n; 0 <= end <= n - 1 => n - 1 - end; end >= n => 0;
    std::fill_n(
      std::back_inserter(res),
      n - 1 - std::min(std::max(end, -1), n - 1),
      NA_REAL
    );
    if (end >= 0) {
      auto iter_b = ts.cbegin() + std::max(0, begin);
      auto iter_e = ts.cbegin() + std::max(0, end) + 1;
      std::copy(iter_b, iter_e, std::back_inserter(res));
    }
    return res;
  }
  bool in_bound_(const int delay) const
  {
    assert_valid_(delay);
    return delayed_index_(delay) >= 0;
  }
  void assert_valid_(const int delay) const
  {
    if (delay < 0) Rcpp::stop(
        "delay must be a non-negative integer instead of %d.", delay
    );
  }
};


Timeseries operator+(const Timeseries& x, const Timeseries& y);
Timeseries operator-(const Timeseries& x, const Timeseries& y);
Timeseries operator*(const Timeseries& x, const Timeseries& y);
Timeseries operator/(const Timeseries& x, const Timeseries& y);
Timeseries operator>(const Timeseries& x, const Timeseries& y);
Timeseries operator<(const Timeseries& x, const Timeseries& y);
Timeseries operator==(const Timeseries& x, const Timeseries& y);
Timeseries pow(const Timeseries& base, const Timeseries& exp);
Timeseries pmin(const Timeseries& x, const double y);
Timeseries pmax(const Timeseries& x, const double y);
Timeseries pmin(const Timeseries& x, const Timeseries& y);
Timeseries pmax(const Timeseries& x, const Timeseries& y);
Timeseries operator+(const Timeseries& x, const double y);
Timeseries operator-(const Timeseries& x, const double y);
Timeseries operator*(const Timeseries& x, const double y);
Timeseries operator/(const Timeseries& x, const double y);
std::vector<bool> operator>(const Timeseries& x, const double y);
std::vector<bool> operator<(const Timeseries& x, const double y);
std::vector<bool> operator==(const Timeseries& x, const double y);
Timeseries pow(const Timeseries& base, const double exp);
void assert_valid(const Panel& x);
Timeseries apply(const Panel& x, std::function<double(const Timeseries&)> fun);
Timeseries apply(
    const Panel& x, const Panel& y,
    std::function<double(const Timeseries&, const Timeseries&)> fun
);


inline std::vector<Quote_raw> v_quote(Rcpp::List qt_tbls)
{
  std::vector<Quote_raw> res;
  const int n = qt_tbls.size();
  for (int i {0}; i < n; ++i) {
    Rcpp::DataFrame qt_tbl = Rcpp::wrap(qt_tbls[i]);
    res.emplace_back(qt_tbl);
  }
  return res;
}


inline std::vector<std::string> v_names(Rcpp::List qt_tbls)
{
  if (Rf_isNull(qt_tbls.attr("names"))) Rcpp::stop(
      "qt_tbls must have names attributes."
  );
  Rcpp::StringVector names = qt_tbls.attr("names");
  return Rcpp::as<std::vector<std::string>>(names);
}


struct Quotes_raw {
  Quotes_raw() = default;
  explicit Quotes_raw(Rcpp::List qt_tbls)
    : names (v_names(qt_tbls)),
      qts (v_quote(qt_tbls))
  { }
  std::vector<std::string> names;
  std::vector<Quote_raw> qts;
  std::vector<RDate> tdates(const Rcpp::newDateVector from_to) const
  {
    std::set<RDate> set;
    for (const auto& qt : qts) {
      auto dates = qt.tdates(from_to);
      for (auto date : dates) set.insert(date);
    }
    std::vector<RDate> res;
    std::copy(set.cbegin(), set.cend(), std::back_inserter(res));
    return res;
  }
  int size() const { return qts.size(); }
};


class Quotes {
public:
  Quotes() = default;
  explicit Quotes(const Quotes_raw& raw, const RDate today)
    : raw_ (raw),
      qts_ (gen_qts_(raw, today)) { }
  explicit Quotes(const Quotes_raw& raw, const std::vector<Quote>& qts, const int days)
    : raw_ (raw),
      qts_ (gen_qts_(qts, days)) { }

  Quotes clock_back(const int days) const
  {
    if (days < 0) Rcpp::stop("days (%d) must be non negative.", days);
    return Quotes(raw_, qts_, days);
  }

  Timeseries apply(std::function<double(const Quote&)> fun) const
  {
    Timeseries res;
    std::transform(qts_.cbegin(), qts_.cend(), std::back_inserter(res), fun);
    return res;
  }

  // template function is possible but not necessary
  std::vector<Timeseries> tsapply(
      const int n, std::function<Timeseries(const Quotes&)> fun) const
  {
    if (n < 1) Rcpp::stop("n (%d) must be positive.", n);
    std::vector<Timeseries> res;
    for (int i {n - 1}; i >= 0; --i) {
      res.push_back(fun(clock_back(i)));
    }
    return res;
  }
  std::vector<RDate> tdates(const Rcpp::newDateVector from_to) const
  {
    return raw_.tdates(from_to);
  }
  int size() const { return raw_.size(); }
  const std::vector<std::string>& names() const { return raw_.names; }
private:
  const Quotes_raw& raw_;
  std::vector<Quote> qts_;
  std::vector<Quote> gen_qts_(const Quotes_raw& raw, const RDate today) const
  {
    std::vector<Quote> res;
    for (const auto& qt : raw.qts) res.emplace_back(qt, today);
    return res;
  }
  std::vector<Quote> gen_qts_(const std::vector<Quote>& qts, const int days) const
  {
    std::vector<Quote> res;
    for (const auto& qt : qts) res.push_back(qt.clock_back(days));
    return res;
  }
};

#endif //__GCAMCTF_ALGO__
