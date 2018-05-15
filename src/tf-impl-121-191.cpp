#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
// ((RANK((VWAP -MIN(VWAP, 12)))^
// TSRANK(CORR(TSRANK(VWAP, 20), TSRANK(MEAN(VOLUME,60), 2), 18), 3)) * -1)
Alpha_mfun alpha121 = [](const Quotes& qts) -> Timeseries {
  auto fun_base = [](const Quote& qt) {
    return qt.vwap() - tsmin(qt.ts_vwap(12));
  };
  auto base = rank(qts.apply(fun_base));
  auto fun_exp = [](const Quote& qt) {
    auto tsrank_cor = [&qt](const int delay1) {
      auto cor_x = [&qt, delay1](const int delay2) {
        return tsrank(qt.ts_vwap(delay1 + delay2 + 20));
      };
      auto cor_y = [&qt, delay1](const int delay2) {
        auto mean_vol_60 = [&qt, delay1, delay2] (const int delay3) {
          return mean(qt.ts_volume(60, delay1 + delay2 + delay3));
        };
        return tsrank(ts<double>(2, mean_vol_60));
      };
      return corr(ts<double>(18, cor_x), ts<double>(18, cor_y));
    };
    return tsrank(ts<double>(3, tsrank_cor));
  };
  auto exp = qts.apply(fun_exp);
  return pow(base, exp) * -1.0;
};

Alpha_fun alpha149 = [](const Quote& quote) -> double {
  const auto dr = quote.ts_close(252) / quote.ts_close(252, 1) - 1.0;
  const auto bmk_dr = quote.ts_bmk_close(252) / quote.ts_bmk_close(252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  if (x.size() < 2) return NA_REAL;
  return regbeta(x, y);
};

}
