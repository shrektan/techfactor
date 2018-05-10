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

Timeseries delta(const Timeseries& x);
double corr(const Timeseries& x, const Timeseries& y);
Timeseries rank(const Timeseries& x);
double sum(const Timeseries& x);
double stdev(const Timeseries& x);
double mean(const Timeseries& x);
double tsmin(const Timeseries& x);
double tsmax(const Timeseries& x);
double tsrank(const Timeseries& x);
double sign(const double x);
double sma(const Timeseries& x, const int n, const int m);
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
Timeseries regresi(const Timeseries& x, const Timeseries& y);
Timeseries filter(const Timeseries& x, const std::vector<bool>& cond);
double highday(const Timeseries& x);
double lowday(const Timeseries& x);


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

class Quotes
{
public:
  Quotes() = default;
  explicit Quotes(const Rcpp::DataFrame tbl)
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
  RDate today() {
    if (today_index_ < 0) Rcpp::stop("negative today_index_ %d.", today_index_);
    return dates_[today_index_];
  }

  void set(const RDate today) noexcept {
    auto iter = std::lower_bound(dates_.cbegin(), dates_.cend(), today);
    if (iter == dates_.cend() || *iter > today) {
      today_index_ = -1;
    } else {
      today_index_ = std::distance(dates_.cbegin(), iter);
    }
  }

  double pclose(const int delay = 0) const noexcept { return get_(pclose_, delay); }
  double open(const int delay = 0) const noexcept { return get_(open_, delay); }
  double high(const int delay = 0) const noexcept { return get_(high_, delay); }
  double low(const int delay = 0) const noexcept { return get_(low_, delay); }
  double close(const int delay = 0) const noexcept { return get_(close_, delay); }
  double volume(const int delay = 0) const noexcept { return get_(volume_, delay); }
  double amount(const int delay = 0) const noexcept { return get_(amount_, delay); }
  double bmk_close(const int delay = 0) const noexcept { return get_(bmk_close_, delay); }
  double bmk_open(const int delay = 0) const noexcept { return get_(bmk_open_, delay); }

  Timeseries ts_pclose(const int n, const int delay = 0) const noexcept {
    return ts_get_(pclose_, n, delay);
  }
  Timeseries ts_open(const int n, const int delay = 0) const noexcept {
    return ts_get_(open_, n, delay);
  }
  Timeseries ts_high(const int n, const int delay = 0) const noexcept {
    return ts_get_(high_, n, delay);
  }
  Timeseries ts_low(const int n, const int delay = 0) const noexcept {
    return ts_get_(low_, n, delay);
  }
  Timeseries ts_close(const int n, const int delay = 0) const noexcept {
    return ts_get_(close_, n, delay);
  }
  Timeseries ts_volume(const int n, const int delay = 0) const noexcept {
    return ts_get_(volume_, n, delay);
  }
  Timeseries ts_amount(const int n, const int delay = 0) const noexcept {
    return ts_get_(amount_, n, delay);
  }
  Timeseries ts_bmk_close(const int n, const int delay = 0) const noexcept {
    return ts_get_(bmk_close_, n, delay);
  }
  Timeseries ts_bmk_open(const int n, const int delay = 0) const noexcept {
    return ts_get_(bmk_open_, n, delay);
  }

  double hd() const noexcept { return high() - high(1); }
  double ld() const noexcept { return low(1) - low(); }

  double tr(const int delay = 0) const noexcept
  {
    if (!valid_(1)) return NA_REAL;
    return std::max(
      std::max(high() - low(), std::abs(high() - close(1))),
      std::abs(low() - close(1))
    );
  }

  double dtm() const noexcept
  {
    if (!valid_(1)) return NA_REAL;
    return open() <= open(1) ? 0.0 : std::max(high() - open(), open() - open(1));
  }

  double dbm() const noexcept
  {
    if (!valid_(1)) return NA_REAL;
    return open() >= open(1) ? 0.0 : std::max(open() - low(), open() - open(1));
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
  int delayed_index_(const int delay) const noexcept { return today_index_ - delay; }
  double get_(const Timeseries& ts, const int delay) const noexcept
  {
    if (!valid_(delay)) return NA_REAL;
    return ts[delayed_index_(delay)];
  }
  Timeseries ts_get_(const Timeseries& ts, const int n, const int delay) const noexcept
  {
    int begin = delayed_index_(delay + n);
    int end = delayed_index_(delay);
    Timeseries res;
    if (n - 1 > end) std::fill_n(std::back_inserter(res), n - 1 - end, NA_REAL);
    if (end >= 0) {
      auto iter_b = ts.cbegin() + std::max(0, begin);
      auto iter_e = ts.cbegin() + std::max(0, end) + 1;
      std::copy(iter_b, iter_e, std::back_inserter(res));
    }
    return res;
  }
  bool valid_(const int delay) const noexcept { return delayed_index_(delay) >= 0; }
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
      if (v == 0.0) return NA_REAL;
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