#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

// (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE -OPEN) / OPEN)), 6))
Alpha_fun alpha001 = [](const Quotes& quotes) -> double {
  const auto rk_delta_log_vol = rank(delta(log(quotes.ts_close(7)), 1));
  const auto rk_close_open = rank((quotes.ts_close(6) - quotes.ts_open(6)) / quotes.ts_open(6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};


// (-1* DELTA((((CLOSE -LOW) -(HIGH -CLOSE)) / (HIGH -LOW)), 1))
Alpha_fun alpha002 = [](const Quotes& quotes) -> double {
  const auto close = quotes.ts_close(2);
  const auto open = quotes.ts_open(2);
  const auto low = quotes.ts_low(2);
  const auto high = quotes.ts_high(2);
  const auto rk_price = ((close - low) - (high - close)) / (high - low);
  return sum(delta(rk_price, 1)) * -1;
};


// SUM((CLOSE=DELAY(CLOSE,1)?0:CLOSE-(CLOSE>DELAY(CLOSE,1)?
// MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),6)
Alpha_fun alpha003 = [](const Quotes& quotes) -> double {
  Timeseries ts;
  for (int i {5}; i >= 0; --i)
  {
    if (quotes.close(i) == quotes.pclose(i)) {
      ts.push_back(0.0);
    } else if (quotes.close(i) > quotes.pclose(i)) {
      ts.push_back(quotes.close(i) - std::min(quotes.low(i), quotes.pclose(i)));
    } else {
      ts.push_back(quotes.close(i) - std::min(quotes.high(i), quotes.pclose(i)));
    }
  }
  return sum(ts);
};


// ((((SUM(CLOSE, 8) / 8) + STD(CLOSE, 8)) < (SUM(CLOSE, 2) / 2)) ?
// (-1 * 1) : (((SUM(CLOSE, 2) / 2) < ((SUM(CLOSE, 8) / 8) -STD(CLOSE, 8))) ?
// 1 : (((1 < (VOLUME / MEAN(VOLUME,20))) || ((VOLUME / MEAN(VOLUME,20)) == 1)) ? 1 : (-1 * 1))))
Alpha_fun alpha004 = [](const Quotes& quotes) -> double {
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


// (RANK(SIGN(DELTA((((OPEN * 0.85) + (HIGH * 0.15))), 4)))* -1)
Alpha_fun alpha006 = [](const Quotes& quotes) -> double {
  const auto rk_delta_compose_price = delta(quotes.ts_open(5) * 0.85 + quotes.ts_high(5) * 0.15, 4);
  return sign(sum(rk_delta_compose_price));
};


// (RANK(MAX((VWAP -CLOSE), 3)) + RANK(MIN((VWAP -CLOSE), 3))) * RANK(DELTA(VOLUME, 3))
Alpha_fun alpha007 = [](const Quotes& quotes) -> double {
  return 1;
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
