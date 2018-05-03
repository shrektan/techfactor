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


#include <Rcpp.h>
#include "algo.h"


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
