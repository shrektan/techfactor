#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
Alpha_fun alpha001 = [](const Quotes& quotes) -> double {
  // (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE -OPEN) / OPEN)), 6))
  const auto rk_delta_log_vol = rank(delta(log(quotes.ts_close(7)), 1));
  const auto rk_close_open = rank((quotes.ts_close(6) - quotes.ts_open(6)) / quotes.ts_open(6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};


Alpha_fun alpha002 = [](const Quotes& quotes) -> double {
  // (-1* DELTA((((CLOSE -LOW) -(HIGH -CLOSE)) / (HIGH -LOW)), 1))
  const auto close = quotes.ts_close(2);
  const auto open = quotes.ts_open(2);
  const auto low = quotes.ts_low(2);
  const auto high = quotes.ts_high(2);
  const auto rk_price = ((close - low) - (high - close)) / (high - low);
  return sum(delta(rk_price, 1)) * -1;
};


Alpha_fun alpha003 = [](const Quotes& quotes) -> double {
  // SUM((CLOSE=DELAY(CLOSE,1)?0:CLOSE-(CLOSE>DELAY(CLOSE,1)?MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),6)
  Timeseries ts;
  for (int i {5}; i >= 0; --i)
  {
    double delay_price;
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


Alpha_fun alpha004 = [](const Quotes& quotes) -> double {
  // ((((SUM(CLOSE, 8) / 8) + STD(CLOSE, 8)) < (SUM(CLOSE, 2) / 2)) ? (-1 * 1) :
  // (((SUM(CLOSE, 2) / 2) < ((SUM(CLOSE, 8) / 8) -STD(CLOSE, 8))) ? 1 :
  // (((1 < (VOLUME / MEAN(VOLUME,20))) || ((VOLUME / MEAN(VOLUME,20)) == 1)) ? 1 : (-1 * 1))))
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


Alpha_fun alpha005 = [](const Quotes& quotes) -> double {
  // (-1 * TSMAX(CORR(TSRANK(VOLUME, 5), TSRANK(HIGH, 5), 5), 3))
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


Alpha_fun alpha006 = [](const Quotes& quotes) -> double {
  // (RANK(SIGN(DELTA((((OPEN * 0.85) + (HIGH * 0.15))), 4)))* -1)
  const auto rk_delta_price = delta(quotes.ts_open(4) * 0.85 + quotes.ts_high(4) * 0.15, 4);
  return sign(rk_delta_price);
};


Alpha_fun alpha014 = [](const Quotes& quotes) -> double {
  return quotes.close() - quotes.close(5);
};


Alpha_fun alpha053 = [](const Quotes& quotes) -> double {
  std::vector<bool> cond;
  for (int i {11}; i >= 0; --i)
  {
    cond.push_back(quotes.close() > quotes.close(1));
  }
  return count(cond);
};

}
