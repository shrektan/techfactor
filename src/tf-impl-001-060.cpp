#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
Alpha_fun alpha001 = [](const Quotes& quotes) -> double {
  const auto rk_delta_log_vol = rank(delta(log(quotes.ts_close(7))));
  const auto rk_close_open = rank((quotes.ts_close(6) - quotes.ts_open(6)) / quotes.ts_open(6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};


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


Alpha_fun alpha005 = [](const Quotes& quotes) -> double {
  auto fun = [&quotes](const int delay1) {
    auto volumn = [&quotes, delay1] (const int delay2) {
      return tsrank(quotes.ts_volume(5, delay2 + delay1));
    };
    auto high = [&quotes, delay1] (const int delay2) {
      return tsrank(quotes.ts_high(5, delay2 + delay1));
    };
    return corr(ts(5, volumn), ts(5, high));
  };
  return -tsmax(ts(3, fun));
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
