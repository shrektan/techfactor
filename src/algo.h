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
using TD_code = int;
using TD_index = std::map<Date, TD_code>;
using Timeseries = std::vector<double>;
using Quote_elem = std::map<Quote_tag, Timeseries>;



class TD_converter {
public:
  TD_converter() = default;
  TD_converter(Rcpp::newDateVector x);
  TD_code td_code(const Date date) const;
private:
  TD_index td_indexes_;
};


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


class Quotes
{
public:
  Quotes() = default;
  explicit Quotes(const Rcpp::DataFrame tbl);
  double prev_close(const int delay = 0) const;
  double open(const int delay = 0) const;
  double high(const int delay = 0) const;
  double low(const int delay = 0) const;
  double close(const int delay = 0) const;
  double volume(const int delay = 0) const;
  double amount(const int delay = 0) const;
  double bmk_close(const int delay = 0) const;
  double bmk_open(const int delay = 0) const;
  double tr(const int delay = 0) const;
  double dtm() const;
  double dbm() const;
  double hd() const;
  double ld() const;
  void set(const TD_code today) const;

private:
  TD_index& td_index_;
  Timeseries& prev_close_;
  Timeseries& open_;
  Timeseries& high_;
  Timeseries& low_;
  Timeseries& close_;
  Timeseries& vwap_;
  Timeseries& volume_;
  Timeseries& amount_;
  Timeseries& bmk_close_;
  Timeseries& bmk_open_;
  mutable TD_code today_ {0};
};


Timeseries close(const Quotes& quotes, const int n, const int delay = 0);
Timeseries open(const Quotes& quotes, const int n, const int delay = 0);
Timeseries high(const Quotes& quotes, const int n, const int delay = 0);
Timeseries low(const Quotes& quotes, const int n, const int delay = 0);
Timeseries prev_close(const Quotes& quotes, const int n, const int delay = 0);
Timeseries volume(const Quotes& quotes, const int n, const int delay = 0);
Timeseries amount(const Quotes& quotes, const int n, const int delay = 0);
Timeseries bmk_close(const Quotes& quotes, const int n, const int delay = 0);
Timeseries bmk_open(const Quotes& quotes, const int n, const int delay = 0);
Timeseries operator+(const Timeseries& x, const Timeseries& y);
Timeseries operator-(const Timeseries& x, const Timeseries& y);
Timeseries operator*(const Timeseries& x, const Timeseries& y);
Timeseries operator/(const Timeseries& x, const Timeseries& y);
Timeseries operator+(const Timeseries& x, const double y);
Timeseries operator-(const Timeseries& x, const double y);
Timeseries operator*(const Timeseries& x, const double y);
Timeseries operator/(const Timeseries& x, const double y);
std::vector<bool> operator>(const Timeseries& x, const double y);
std::vector<bool> operator<(const Timeseries& x, const double y);

void assert_valid(const Rcpp::newDateVector from_to);
std::vector<int> td_seq(const Rcpp::newDateVector from_to);

#endif //__GCAMCTF_ALGO__