#ifndef __GCAMCTF_ALGO__
#define __GCAMCTF_ALGO__

#include <Rcpp.h>
#include <map>
#include "GCAMCTF_types.h"

void assert_same_size(const Timeseries& x, const Timeseries& y);
void assert_valid(const Rcpp::newDateVector from_to);
bool any_na(const Timeseries& x);
void assert_no_na(const Timeseries& x);
Timeseries na_vector(const int length);

Timeseries delta(const Timeseries& x, const int n = 1);
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
Timeseries decaylinear(const Timeseries& x, const int days);
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

class Quote {
public:
  Quote() = default;
  explicit Quote(const Rcpp::DataFrame tbl)
    : dates_ { col_date(tbl, "DATE") },
      pclose_ { col(tbl, "PCLOSE") },
      open_ { col(tbl, "OPEN") },
      high_ { col(tbl, "HIGH") },
      low_ { col(tbl, "LOW") },
      close_ { col(tbl, "CLOSE") },
      vwap_ { col(tbl, "VWAP") },
      volume_ { col(tbl, "VOLUME") },
      amount_ { col(tbl, "AMOUNT") },
      bmk_close_ { col(tbl, "BMK_CLOSE") },
      bmk_open_ { col(tbl, "BMK_OPEN") }
  { assert_sorted(dates_); }

  // it's mainly for tests
  RDate today()
  {
    if (today_index_ < 0) Rcpp::stop("negative today_index_ %d.", today_index_);
    return dates_[today_index_];
  }

  void set(const RDate today) noexcept
  {
    auto iter = std::lower_bound(dates_.cbegin(), dates_.cend(), today);
    if (iter == dates_.cend() || *iter > today) {
      today_index_ = -1;
    } else {
      today_index_ = std::distance(dates_.cbegin(), iter);
    }
  }

  double pclose(const int delay = 0) const { return get_(pclose_, delay); }
  double open(const int delay = 0) const { return get_(open_, delay); }
  double high(const int delay = 0) const { return get_(high_, delay); }
  double low(const int delay = 0) const { return get_(low_, delay); }
  double close(const int delay = 0) const { return get_(close_, delay); }
  double vwap(const int delay = 0) const { return get_(vwap_, delay); }
  double volume(const int delay = 0) const { return get_(volume_, delay); }
  double amount(const int delay = 0) const { return get_(amount_, delay); }
  double bmk_close(const int delay = 0) const { return get_(bmk_close_, delay); }
  double bmk_open(const int delay = 0) const { return get_(bmk_open_, delay); }

  Timeseries ts_pclose(const int n, const int delay = 0) const
  {
    return ts_get_(pclose_, n, delay);
  }
  Timeseries ts_open(const int n, const int delay = 0) const
  {
    return ts_get_(open_, n, delay);
  }
  Timeseries ts_high(const int n, const int delay = 0) const
  {
    return ts_get_(high_, n, delay);
  }
  Timeseries ts_low(const int n, const int delay = 0) const
  {
    return ts_get_(low_, n, delay);
  }
  Timeseries ts_close(const int n, const int delay = 0) const
  {
    return ts_get_(close_, n, delay);
  }
  Timeseries ts_vwap(const int n, const int delay = 0) const
  {
    return ts_get_(vwap_, n, delay);
  }
  Timeseries ts_volume(const int n, const int delay = 0) const
  {
    return ts_get_(volume_, n, delay);
  }
  Timeseries ts_amount(const int n, const int delay = 0) const
  {
    return ts_get_(amount_, n, delay);
  }
  Timeseries ts_bmk_close(const int n, const int delay = 0) const
  {
    return ts_get_(bmk_close_, n, delay);
  }
  Timeseries ts_bmk_open(const int n, const int delay = 0) const
  {
    return ts_get_(bmk_open_, n, delay);
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
    auto fun = [this, delay] (const int i) {
      return this->hd(delay + i);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_ld(const int n, const int delay = 0) const
  {
    auto fun = [this, delay] (const int i) {
      return this->ld(delay + i);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_tr(const int n, const int delay = 0) const
  {
    auto fun = [this, delay] (const int i) {
      return this->tr(delay + i);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_ret(const int n, const int delay = 0) const
  {
    auto fun = [this, delay] (const int i) {
      return this->ret(delay + i);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_dtm(const int n, const int delay = 0) const
  {
    auto fun = [this, delay] (const int i) {
      return this->dtm(delay + i);
    };
    return ts<double>(n, fun);
  }
  Timeseries ts_dbm(const int n, const int delay = 0) const
  {
    auto fun = [this, delay] (const int i) {
      return this->dbm(delay + i);
    };
    return ts<double>(n, fun);
  }

  std::vector<RDate> tdates(const Rcpp::newDateVector from_to) const
  {
    assert_valid(from_to);
    auto begin = std::lower_bound(dates_.cbegin(), dates_.cend(), int(from_to[0]));
    auto end = std::lower_bound(dates_.cbegin(), dates_.cend(), int(from_to[1]));
    if (end != dates_.cend() && *end == int(from_to[1])) ++end;
    std::vector<RDate> res;
    std::copy(begin, end, std::back_inserter(res));
    return res;
  }

private:
  std::vector<RDate> dates_;
  Timeseries pclose_;
  Timeseries open_;
  Timeseries high_;
  Timeseries low_;
  Timeseries close_;
  Timeseries vwap_;
  Timeseries volume_;
  Timeseries amount_;
  Timeseries bmk_close_;
  Timeseries bmk_open_;
  int today_index_ {0};
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


inline Timeseries operator+(const Timeseries& x, const Timeseries& y)
{
  assert_same_size(x, y);
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), y.cbegin(), res.begin(), std::plus<double>());
  return res;
}


inline Timeseries operator-(const Timeseries& x, const Timeseries& y)
{
  assert_same_size(x, y);
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), y.cbegin(), res.begin(), std::minus<double>());
  return res;
}


inline Timeseries operator*(const Timeseries& x, const Timeseries& y)
{
  assert_same_size(x, y);
  Timeseries res(x.size());
  std::transform(
    x.cbegin(), x.cend(), y.cbegin(),
    res.begin(), std::multiplies<double>()
  );
  return res;
}


inline Timeseries operator/(const Timeseries& x, const Timeseries& y)
{
  assert_same_size(x, y);
  Timeseries res(x.size());
  std::transform(
    x.cbegin(), x.cend(), y.cbegin(),
    res.begin(), [](const double v1, const double v2) {
      if (v2 == 0) return NA_REAL;
      return v1 / v2;
    });
  return res;
}


inline Timeseries operator+(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  std::transform(
    x.cbegin(), x.cend(),
    res.begin(), [y](const double v) { return v + y; }
  );
  return res;
}


inline Timeseries operator-(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  std::transform(
    x.cbegin(), x.cend(),
    res.begin(), [y](const double v) { return v - y; }
  );
  return res;
}


inline Timeseries operator*(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  std::transform(
    x.cbegin(), x.cend(),
    res.begin(), [y](const double v) { return v * y; }
  );
  return res;
}


inline Timeseries operator/(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  std::transform(
    x.cbegin(), x.cend(),
    res.begin(), [y](const double v) {
      if (y == 0.0) return NA_REAL;
      return v / y;
    }
  );
  return res;
}


inline std::vector<bool> operator>(const Timeseries& x, const double y)
{
  std::vector<bool> res(x.size());
  std::transform(
    x.cbegin(), x.cend(),
    res.begin(), [y](const double v) { return v > y; }
  );
  return res;
}


inline std::vector<bool> operator<(const Timeseries& x, const double y)
{
  std::vector<bool> res(x.size());
  std::transform(
    x.cbegin(), x.cend(),
    res.begin(), [y](const double v) { return v < y; }
  );
  return res;
}


#endif //__GCAMCTF_ALGO__
