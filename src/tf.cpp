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


auto alpha1 = [](const Quotes& quotes) -> double {
  const auto rk_delta_log_vol = rank(delta(log(quotes.ts_close(7))));
  const auto rk_close_open = rank((quotes.ts_close(6) - quotes.ts_open(6)) / quotes.ts_open(6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};


auto alpha3 = [](const Quotes& quotes) -> double {
  const auto sum_close_8 = sum(quotes.ts_close(8));
  const auto std_close_8 = stdev(quotes.ts_close(8));
  const auto sum_close_2 = sum(quotes.ts_close(2));
  if (sum_close_8 / 8 + std_close_8 < sum_close_2 / 2) {
    return -1.0;
  } else if (sum_close_2 / 2 < sum_close_8 / 8 - std_close_8) {
    return 1.0;
  } else {
    const auto vol = quotes.volume();
    const auto mean_vol_20 = mean(quotes.ts_volume(20));
    if (vol / mean_vol_20 >= 1) {
      return 1.0;
    } else {
      return -1.0;
    }
  }
};


auto alpha5 = [](const Quotes& quotes) -> double {
  Timeseries ts;
  for (int i {2}; i >= 0; --i)
  {
    Timeseries rk_vol_5, rk_high_5;
    for (int k {5}; k >= 0; --k)
    {
      rk_vol_5.push_back(tsrank(quotes.ts_volume(5, i + k)));
      rk_high_5.push_back(tsrank(quotes.ts_high(5, i + k)));
    }
    ts.push_back(corr(rk_vol_5, rk_high_5));
  }
  return -tsmax(ts);
};


auto alpha14 = [](const Quotes& quotes) -> double {
  return quotes.close() - quotes.close(5);
};


auto alpha53 = [](const Quotes& quotes) -> double {
  std::vector<bool> cond;
  for (int i {11}; i >= 0; --i)
  {
    cond.push_back(quotes.close() > quotes.close(1));
  }
  return count(cond);
};


auto alpha149 = [](const Quotes& quotes) -> double {
  const auto dr = quotes.ts_close(252) / quotes.ts_close(252, 1) - 1.0;
  const auto bmk_dr = quotes.ts_bmk_close(252) / quotes.ts_bmk_close(252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  return regbeta(x, y);
};



std::map<std::string, std::function<double(const Quotes&)>> tf_funs
{
  {"alpha1", alpha1},
  {"alpha3", alpha3},
  {"alpha5", alpha5},
  {"alpha14", alpha14},
  {"alpha53", alpha53},
  {"alpha149", alpha149}
};


std::map<
  std::string,
  std::function<Rcpp::DataFrame(Quotes&, const Rcpp::newDateVector)>
> tf_fast_funs
{

};


//' @export
// [[Rcpp::export]]
SEXP tf_quotes_xptr(Rcpp::DataFrame qt_tbl)
{
  Quotes* ptr = new Quotes {qt_tbl};
  return Rcpp::XPtr<Quotes>(ptr, true);
}


//' @export
// [[Rcpp::export]]
Rcpp::List tf_factor(SEXP qt_ptr, std::string name, Rcpp::newDateVector from_to)
{
  using namespace Rcpp;
  assert_valid(from_to);
  XPtr<Quotes> qt_xptr {qt_ptr};
  auto& qt = *qt_xptr;
  if (tf_fast_funs.count(name))
  {
    return tf_fast_funs.at(name)(qt, from_to);
  }
  else if (tf_funs.count(name))
  {
    const auto& calculator = tf_funs.at(name);
    const auto dates = qt.tdates(from_to);
    NumericVector vec(dates.size());
    std::transform(
      dates.cbegin(), dates.cend(), vec.begin(),
      [&qt, &calculator] (const RDate date) {
        qt.set(date);
        return calculator(qt);
      }
    );
    newDateVector r_dates(dates.size());
    std::copy(dates.cbegin(), dates.cend(), r_dates.begin());
    return DataFrame::create(_["DATE"] = r_dates, _["VALUE"] = vec);
  }
  else
  {
    stop("factor %s must be defined before using.", name);
  }
}
