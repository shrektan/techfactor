#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

// ((RANK((VWAP -MIN(VWAP, 12)))^TSRANK(CORR(TSRANK(VWAP, 20), TSRANK(MEAN(VOLUME,60), 2), 18), 3)) * -1)
Alpha_fun alpha121 = [](const Quotes& quotes) -> double {

};


// (SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2)-DELAY(SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2),1))/
// DELAY(SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2),1)
Alpha_fun alpha122 = [](const Quotes& quotes) -> double {
  return sma(log(quotes.ts_close(13)), 2);
};


Alpha_fun alpha149 = [](const Quotes& quotes) -> double {
  const auto dr = quotes.ts_close(252) / quotes.ts_close(252, 1) - 1.0;
  const auto bmk_dr = quotes.ts_bmk_close(252) / quotes.ts_bmk_close(252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  return regbeta(x, y);
};

}