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
    auto tsrank_cor = [](const Quote& qt) {
      auto cor_x = [](const Quote& qt) {
        return tsrank(qt.ts_vwap(20));
      };
      auto cor_y = [](const Quote& qt) {
        auto mean_vol_60 = [] (const Quote& qt) {
          return mean(qt.ts_volume(60));
        };
        return tsrank(qt.ts<double>(2, mean_vol_60));
      };
      return corr(qt.ts<double>(18, cor_x), qt.ts<double>(18, cor_y));
    };
    return tsrank(qt.ts<double>(3, tsrank_cor));
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
