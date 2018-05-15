#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

// (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE -OPEN) / OPEN)), 6))
Alpha_fun alpha001 = [](const Quote& quote) -> double {
  const auto rk_delta_log_vol = rank(delta(log(quote.ts_close(7)), 1));
  const auto rk_close_open = rank((quote.ts_close(6) - quote.ts_open(6)) / quote.ts_open(6));
  return corr(rk_delta_log_vol, rk_close_open) * -1;
};


// (-1* DELTA((((CLOSE -LOW) -(HIGH -CLOSE)) / (HIGH -LOW)), 1))
Alpha_fun alpha002 = [](const Quote& quote) -> double {
  const auto close = quote.ts_close(2);
  const auto open = quote.ts_open(2);
  const auto low = quote.ts_low(2);
  const auto high = quote.ts_high(2);
  const auto rk_price = ((close - low) - (high - close)) / (high - low);
  return sum(delta(rk_price, 1)) * -1;
};


// SUM((CLOSE=DELAY(CLOSE,1)?0:CLOSE-(CLOSE>DELAY(CLOSE,1)?
// MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),6)
Alpha_fun alpha003 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int i) {
    if (quote.close(i) == quote.pclose(i)) {
      return 0.0;
    } else if (quote.close(i) > quote.pclose(i)) {
      return quote.close(i) - std::min(quote.low(i), quote.pclose(i));
    } else {
      return quote.close(i) - std::min(quote.high(i), quote.pclose(i));
    }
  };
  return sum(ts<double>(6, fun));
};


// ((((SUM(CLOSE, 8) / 8) + STD(CLOSE, 8)) < (SUM(CLOSE, 2) / 2)) ?
// (-1 * 1) : (((SUM(CLOSE, 2) / 2) < ((SUM(CLOSE, 8) / 8) -STD(CLOSE, 8))) ?
// 1 : (((1 < (VOLUME / MEAN(VOLUME,20))) || ((VOLUME / MEAN(VOLUME,20)) == 1)) ? 1 : (-1 * 1))))
Alpha_fun alpha004 = [](const Quote& quote) -> double {
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


// (RANK(SIGN(DELTA((((OPEN * 0.85) + (HIGH * 0.15))), 4)))* -1)
Alpha_fun alpha006 = [](const Quote& quote) -> double {
  const auto rk_delta_compose_price = delta(quote.ts_open(5) * 0.85 + quote.ts_high(5) * 0.15, 4);
  return sign(sum(rk_delta_compose_price));
};


// (RANK(MAX((VWAP -CLOSE), 3)) + RANK(MIN((VWAP -CLOSE), 3))) * RANK(DELTA(VOLUME, 3))
Alpha_fun alpha007 = [](const Quote& quote) -> double {
  return 1;
};


// RANK(DELTA(((((HIGH + LOW) / 2) * 0.2) + (VWAP * 0.8)), 4) * -1)
Alpha_fun alpha008 = [](const Quote& quote) -> double {
  const auto rk_compose_price = (quote.ts_high(5) + quote.ts_low(5)) / 2 * 0.2 + quote.ts_vwap(5) * 0.8;
  const auto rk_delta_compose_price = sum(delta(rk_compose_price, 4));
  return -rk_delta_compose_price;
};


// SMA(((HIGH+LOW)/2-(DELAY(HIGH,1)+DELAY(LOW,1))/2)*(HIGH-LOW)/VOLUME,7,2)
Alpha_fun alpha009 = [](const Quote& quote) -> double {
  const auto rk_compose_price1 = (quote.ts_high(7) + quote.ts_low(7)) / 2 -
                                 (quote.ts_high(7, 1) + quote.ts_low(7, 1)) / 2;
  const auto rk_compose_price2 = (quote.ts_high(7) - quote.ts_low(7)) / quote.ts_volume(7);
  return sma((rk_compose_price1 * rk_compose_price2), 2);
};


// RANK(MAX(((RET < 0) ? STD(RET, 20) : CLOSE)^2, 5))
Alpha_fun alpha010 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int i) {
    if (quote.ret() < 0) {
      return quote.ret();
    } else {
      return std::pow(quote.close(i), 2.0);
    }
  };
  return tsmax(ts<double>(5, fun));
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
