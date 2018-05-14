#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
// (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE - OPEN) / OPEN)), 6))
Alpha_fun alpha001 = [](const Quotes& quotes) -> double {
  const auto rk_delta_log_vol = rank(delta(log(quotes.ts_close(7))));
  const auto rk_close_open =
    rank((quotes.ts_close(6) - quotes.ts_open(6)) / quotes.ts_open(6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};

// SUM((CLOSE=DELAY(CLOSE,1)?0:CLOSE-(CLOSE>DELAY(CLOSE,1)?
// MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),6)
Alpha_fun alpha003 = [](const Quotes& quotes) -> double {
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


// (-1 * TSMAX(CORR(TSRANK(VOLUME, 5), TSRANK(HIGH, 5), 5), 3))
Alpha_fun alpha005 = [](const Quotes& quotes) -> double {
  auto fun = [&quotes](const int delay1) {
    auto volumn = [&quotes, delay1] (const int delay2) {
      return tsrank(quotes.ts_volume(5, delay2 + delay1));
    };
    auto high = [&quotes, delay1] (const int delay2) {
      return tsrank(quotes.ts_high(5, delay2 + delay1));
    };
    return corr(ts<double>(5, volumn), ts<double>(5, high));
  };
  return -tsmax(ts<double>(3, fun));
};


// CLOSE-DELAY(CLOSE,5)
Alpha_fun alpha014 = [](const Quotes& quotes) -> double {
  return quotes.close() - quotes.close(5);
};


// COUNT(CLOSE>DELAY(CLOSE,1),12)/12*100
Alpha_fun alpha053 = [](const Quotes& quotes) -> double {
  auto fun = [&quotes] (const int delay) {
    return quotes.close(delay + 0) > quotes.close(delay + 1);
  };
  return count(ts<bool>(12, fun)) / 12.0 * 100.0;
};

}
