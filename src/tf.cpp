/*
 * Design:
 *
 * - the date sequences here must be contious trading date sequence, we also have a converter
 *   so that locate a special date is fast and easy
 * - the option to use single pass algo to boost the performance
 *   __UPDATE__: difficult to do it generally and may not neccessary since many calculations
 *   are very small except for the regression stuff.
 * - any NA in the input will lead to NA in the output
 * - support concurrency for different codes. It means we can calculate different codes at the
 *   same time. However, you can't calculate the same codes for different date parrellely.
 */

#include <map>
#include <Rcpp.h>


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


auto alpha1 = [](const Quotes& quotes, const TD_code today) -> double {
  quotes.set(today);
  const auto rk_delta_log_vol = rank(delta(log(close(quotes, 7))));
  const auto rk_close_open = rank((close(quotes, 6) - open(quotes, 6)) / open(quotes, 6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};


auto alpha3 = [](const Quotes& quotes, const TD_code today) -> double {
  quotes.set(today);
  const auto sum_close_8 = sum(close(quotes, 8));
  const auto std_close_8 = stdev(close(quotes, 8));
  const auto sum_close_2 = sum(close(quotes, 2));
  if (sum_close_8 / 8 + std_close_8 < sum_close_2 / 2) {
    return -1.0;
  } else if (sum_close_2 / 2 < sum_close_8 / 8 - std_close_8) {
    return 1.0;
  } else {
    const auto vol = quotes.volume();
    const auto mean_vol_20 = mean(volume(quotes, 20));
    if (vol / mean_vol_20 >= 1) {
      return 1.0;
    } else {
      return -1.0;
    }
  }
};


auto alpha5 = [](const Quotes& quotes, const TD_code today) -> double {
  Timeseries ts;
  for (int i {2}; i >= 0; --i)
  {
    Timeseries rk_vol_5, rk_high_5;
    for (int k {5}; k >= 0; --k)
    {
      quotes.set(today - i - k);
      rk_vol_5.push_back(tsrank(volume(quotes, 5)));
      rk_high_5.push_back(tsrank(high(quotes, 5)));
    }
    ts.push_back(corr(rk_vol_5, rk_high_5));
  }
  return -tsmax(ts);
};


auto alpha14 = [](const Quotes& quotes, const TD_code today) -> double {
  quotes.set(today);
  return quotes.close() - quotes.close(5);
};


auto alpha53 = [](const Quotes& quotes, const TD_code today) -> double {
  std::vector<bool> cond;
  for (int i {11}; i >= 0; --i)
  {
    quotes.set(today - i);
    cond.push_back(quotes.close() > quotes.close(1));
  }
  return count(cond);
};


auto alpha149 = [](const Quotes& quotes, const TD_code today) -> double {
  quotes.set(today);
  const auto dr = close(quotes, 252) / close(quotes, 252, 1) - 1.0;
  const auto bmk_dr = bmk_close(quotes, 252) / bmk_close(quotes, 252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  return regbeta(x, y);
};



std::map<std::string, std::function<
  double(const Quotes& quotes, const TD_code today)>> tf_funs = {
  {"alpha1", alpha1},
  {"alpha3", alpha3},
  {"alpha5", alpha5},
  {"alpha14", alpha14},
  {"alpha53", alpha53},
  {"alpha149", alpha149}
};


std::map<std::string, std::function<
  Rcpp::DataFrame(const Quotes& quotes, const Rcpp::newDateVector from_to)>> tf_fast_funs = {

};

// [[Rcpp::export]]
SEXP tf_quotes_ptr(Rcpp::DataFrame raw)
{
  Quotes* ptr = new Quotes {raw};
  return  Rcpp::XPtr<Quotes>(ptr, true);
}


void assert_valid(const Rcpp::newDateVector from_to);

std::vector<int> td_seq(const Rcpp::newDateVector from_to);


// [[Rcpp::export]]
Rcpp::List tf_run_cpp(SEXP quotes_ptr,
                      const Rcpp::StringVector factors,
                      const Rcpp::newDateVector from_to)
{
  using namespace Rcpp;
  assert_valid(from_to);
  Rcpp::XPtr<Quotes> xptr {quotes_ptr};
  const auto& quotes = *xptr;
  const int n_factor = factors.length();
  Rcpp::List res(n_factor);
  const auto dates = td_seq(from_to);
  const int n_dates = dates.size();
  for (int i {0}; i < n_factor; ++i)
  {
    const auto factor = std::string {factors[i]};
    if (tf_fast_funs.count(factor))
    {
      res[i] = tf_fast_funs.at(factor)(quotes, from_to);
    }
    else if (tf_funs.count(factor))
    {
      const auto fun = tf_funs.at(factor);
      NumericVector res_i(n_dates);
      for (int j {0}; i < n_dates; ++j) res_i[j] = fun(quotes, dates[j]);
      res[i] = DataFrame::create(
        _["DATE"] = wrap(dates),
        _["VALUE"] = res_i
      );
    } else {
      stop("factor %s must be defined before using.", factor);
    }
  }
  return res;
}
