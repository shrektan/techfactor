#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
// (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE - OPEN) / OPEN)), 6))
Alpha_fun alpha001 = [](const Quote& quote) -> double {
  const auto rk_delta_log_vol = rank(delta(log(quote.ts_close(7))));
  const auto rk_close_open =
    rank((quote.ts_close(6) - quote.ts_open(6)) / quote.ts_open(6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};

// SUM((CLOSE=DELAY(CLOSE,1)?0:CLOSE-(CLOSE>DELAY(CLOSE,1)?
// MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),6)
Alpha_fun alpha003 = [](const Quote& quote) -> double {
  const auto sum_close_8 = sum(quote.ts_close(8));
  const auto std_close_8 = stdev(quote.ts_close(8));
  const auto sum_close_2 = sum(quote.ts_close(2));
  if (sum_close_8 / 8 + std_close_8 < sum_close_2 / 2) {
    return -1.0;
  } else if (sum_close_2 / 2 < sum_close_8 / 8 - std_close_8) {
    return 1.0;
  } else {
    const auto vol = quote.volume();
    const auto mean_vol_20 = mean(quote.ts_volume(20));
    if (vol / mean_vol_20 >= 1) {
      return 1.0;
    } else {
      return -1.0;
    }
  }
};


// (-1 * TSMAX(CORR(TSRANK(VOLUME, 5), TSRANK(HIGH, 5), 5), 3))
Alpha_fun alpha005 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay1) {
    auto volumn = [&quote, delay1] (const int delay2) {
      return tsrank(quote.ts_volume(5, delay2 + delay1));
    };
    auto high = [&quote, delay1] (const int delay2) {
      return tsrank(quote.ts_high(5, delay2 + delay1));
    };
    return corr(ts<double>(5, volumn), ts<double>(5, high));
  };
  return -tsmax(ts<double>(3, fun));
};


// CLOSE-DELAY(CLOSE,5)
Alpha_fun alpha014 = [](const Quote& quote) -> double {
  return quote.close() - quote.close(5);
};


// COUNT(CLOSE>DELAY(CLOSE,1),12)/12*100
Alpha_fun alpha053 = [](const Quote& quote) -> double {
  auto fun = [&quote] (const int delay) {
    return quote.close(delay + 0) > quote.close(delay + 1);
  };
  return count(ts<bool>(12, fun)) / 12.0 * 100.0;
};

}
