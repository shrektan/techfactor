#ifndef __GCAMCTF_ALGO__
#define __GCAMCTF_ALGO__

#include <Rcpp.h>
#include <map>

enum class Quote_tag {
  prev_close, open, high, low, close, vwap, volume, amount,
  bmk_open, bmk_close
};

using Code = int;
using Date = int;
using Timeseries = std::vector<double>;
using Quote_elem = std::map<Quote_tag, Timeseries>;

void assert_same_size(const Timeseries& x, const Timeseries& y);
void assert_valid(const Rcpp::newDateVector from_to);

Timeseries delta(const Timeseries& x);
double corr(const Timeseries& x, const Timeseries& y);
Timeseries rank(const Timeseries& x);
double sum(const Timeseries& x);
double stdev(const Timeseries& x);
double mean(const Timeseries& x);
double tsmin(const Timeseries& x);
double tsmax(const Timeseries& x);
int tsrank(const Timeseries& x);
int sign(const double x);
double sma(const Timeseries& x, const int n, const int m);
double wma(const Timeseries& x);
Timeseries decaylinear(const Timeseries& x, const int days);
Timeseries sequence(const int n);
Timeseries sumac(const Timeseries& x);
Timeseries log(const Timeseries& x);
Timeseries abs(const Timeseries& x);
double covariance(const Timeseries& x, const Timeseries& y);
double prod(const Timeseries& x);
int count(const std::vector<bool>& x);
double regbeta(const Timeseries& x, const Timeseries& y);
double regresi(const Timeseries& x, const Timeseries& y);
Timeseries filter(const Timeseries& x, const std::vector<bool>& cond);
int highday(const Timeseries& x);
int lowday(const Timeseries& x);


inline Timeseries col(const Rcpp::DataFrame tbl, const std::string field)
{
  Rcpp::NumericVector res = tbl[field];
  return Rcpp::as<Timeseries>(res);
}


inline std::vector<Date> col_date(const Rcpp::DataFrame tbl, const std::string field)
{
  Rcpp::newDateVector res = tbl[field];
  return Rcpp::as<std::vector<Date>>(res);
}


class Quotes
{
public:
  Quotes() = default;
  explicit Quotes(const Rcpp::DataFrame tbl)
    : td_index_ { col_date(tbl, "DATE") },
      prev_close_ { col(tbl, "PREV_CLOSE") },
      open_ { col(tbl, "OPEN") },
      high_ { col(tbl, "HIGH") },
      low_ { col(tbl, "LOW") },
      close_ { col(tbl, "CLOSE") },
      vwap_ { col(tbl, "VWAP") },
      volume_ { col(tbl, "VOLUME") },
      amount_ { col(tbl, "AMOUNT") },
      bmk_close_ { col(tbl, "BMK_CLOSE") },
      bmk_open_ { col(tbl, "BMK_OPEN") } { }

  double prev_close(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return prev_close_[today_ - delay];
  }

  double open(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return open_[today_ - delay];
  }

  double high(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return high_[today_ - delay];
  }

  double low(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return low_[today_ - delay];
  }

  double close(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return close_[today_ - delay];
  }

  double volume(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return volume_[today_ - delay];
  }

  double amount(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return amount_[today_ - delay];
  }

  double bmk_close(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return bmk_close_[today_ - delay];
  }

  double bmk_open(const int delay = 0) const
  {
    if (!valid_(delay)) return NA_REAL;
    return bmk_open_[today_ - delay];
  }

  double tr(const int delay = 0) const
  {
    return std::max(
      std::max(high() - low(), std::abs(high() - close(1))),
      std::abs(low() - close(1))
    );
  }

  double dtm() const
  {
    return open() <= open(1) ? 0.0 : std::max(high() - open(), open() - open(1));
  }

  double dbm() const
  {
    return open() >= open(1) ? 0.0 : std::max(open() - low(), open() - open(1));
  }

  double hd() const
  {
    return high() - high(1);
  }

  double ld() const
  {
    return low(1) - low();
  }

  void set(const Date today)
  {
    today_ = today;
  }

private:
  std::vector<Date> td_index_;
  Timeseries prev_close_;
  Timeseries open_;
  Timeseries high_;
  Timeseries low_;
  Timeseries close_;
  Timeseries vwap_;
  Timeseries volume_;
  Timeseries amount_;
  Timeseries bmk_close_;
  Timeseries bmk_open_;
  Date today_ {0};
  bool valid_(const int delay) const { return today_ - delay >= 0; }
};


inline Timeseries close(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.close(i + delay));
  }
  return res;
}


inline Timeseries open(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.open(i + delay));
  }
  return res;
}


inline Timeseries high(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.high(i + delay));
  }
  return res;
}


inline Timeseries low(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.low(i + delay));
  }
  return res;
}


inline Timeseries prev_close(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.prev_close(i + delay));
  }
  return res;
}


inline Timeseries volume(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.volume(i + delay));
  }
  return res;
}


inline Timeseries amount(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.amount(i + delay));
  }
  return res;
}


inline Timeseries bmk_close(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.bmk_close(i + delay));
  }
  return res;
}


inline Timeseries bmk_open(const Quotes& quotes, const int n, const int delay = 0)
{
  Timeseries res;
  for (int i {n}; i > n; --i)
  {
    res.push_back(quotes.bmk_open(i + delay));
  }
  return res;
}


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
  std::transform(x.cbegin(), x.cend(), y.cbegin(), res.begin(), std::multiplies<double>());
  return res;
}


inline Timeseries operator/(const Timeseries& x, const Timeseries& y)
{
  assert_same_size(x, y);
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), y.cbegin(), res.begin(), [](const double v1, const double v2) {
    if (v2 == 0) return NA_REAL;
    return v1 / v2;
  });
  return res;
}


inline Timeseries operator+(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [y](const double v) { return v + y; });
  return res;
}


inline Timeseries operator-(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [y](const double v) { return v - y; });
  return res;
}


inline Timeseries operator*(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [y](const double v) { return v * y; });
  return res;
}


inline Timeseries operator/(const Timeseries& x, const double y)
{
  Timeseries res(x.size());
  if (y == 0.0) {
    std::transform(x.cbegin(), x.cend(), res.begin(), [](const double v) { return NA_REAL; });
  } else {
    std::transform(x.cbegin(), x.cend(), res.begin(), [y](const double v) { return v + y; });
  }
  return res;
}


inline std::vector<bool> operator>(const Timeseries& x, const double y)
{
  std::vector<bool> res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [y](const double v) { return v > y; });
  return res;
}


inline std::vector<bool> operator<(const Timeseries& x, const double y)
{
  std::vector<bool> res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [y](const double v) { return v < y; });
  return res;
}


#endif //__GCAMCTF_ALGO__