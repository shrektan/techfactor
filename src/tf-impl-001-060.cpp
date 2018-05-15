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
    if (quote.ret(i) < 0) {
      return std::pow(stdev(quote.ts_ret(20, i)), 2.0);
    } else {
      return std::pow(quote.close(i), 2.0);
    }
  };
  return tsmax(ts<double>(5, fun));
};


// SUM(((CLOSE-LOW)-(HIGH-CLOSE))./(HIGH-LOW).*VOLUME,6)
Alpha_fun alpha011 = [](const Quote& quote) -> double {
  auto rk_compose_pv = ((quote.ts_close(6) - quote.ts_low(6)) - (quote.ts_high(6) - quote.ts_close(6))) /
                       (quote.ts_high(6) - quote.ts_low(6)) * quote.ts_volume(6);
  return sum(rk_compose_pv);
};


// (RANK((OPEN -(SUM(VWAP, 10) / 10)))) * (-1 * (RANK(ABS((CLOSE -VWAP)))))
Alpha_fun alpha012 = [](const Quote& quote) -> double {
  return 1;
};


// (((HIGH * LOW)^0.5) -VWAP)
Alpha_fun alpha013 = [](const Quote& quote) -> double {
  return std::pow((quote.high() * quote.low()), 0.5) - quote.vwap();
};


// CLOSE-DELAY(CLOSE,5)
Alpha_fun alpha014 = [](const Quote& quote) -> double {
  return quote.close() - quote.close(5);
};


// OPEN/DELAY(CLOSE,1)-1
Alpha_fun alpha015 = [](const Quote& quote) -> double {
  return quote.open() / quote.close(1) - 1.0;
};


// (-1 * TSMAX(RANK(CORR(RANK(VOLUME), RANK(VWAP), 5)), 5))
Alpha_fun alpha016 = [](const Quote& quote) -> double {
  return 1;
};


// RANK((VWAP -MAX(VWAP, 15)))^DELTA(CLOSE, 5)
Alpha_fun alpha017 = [](const Quote& quote) -> double {
  return 1;
};


// CLOSE/DELAY(CLOSE,5)
Alpha_fun alpha018 = [](const Quote& quote) -> double {
  return quote.close() / quote.close(5);
};


// (CLOSE<DELAY(CLOSE,5)?
// (CLOSE-DELAY(CLOSE,5))/DELAY(CLOSE,5):(CLOSE=DELAY(CLOSE,5)?
// 0:(CLOSE-DELAY(CLOSE,5))/CLOSE))
Alpha_fun alpha019 = [](const Quote& quote) -> double {
  if (quote.close() < quote.close(5)) {
    return (quote.close() - quote.close(5)) / quote.close(5);
  } else if (quote.close() == quote.close(5)) {
    return 0.0;
  } else {
    return (quote.close() - quote.close(5)) / quote.close();
  }
};


// (CLOSE-DELAY(CLOSE,6))/DELAY(CLOSE,6)*100
Alpha_fun alpha020 = [](const Quote& quote) -> double {
  return (quote.close() - quote.close(6)) / quote.close(6) * 100;
};


// REGBETA(MEAN(CLOSE,6),SEQUENCE(6))
Alpha_fun alpha021 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int i) {
    return mean(quote.ts_close(6, i));
  };
  Timeseries quote_mean = ts<double>(6, fun);
  return regbeta(quote_mean, sequence(6));
};


// SMA(((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6)-DELAY((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6),3)),12,1)
Alpha_fun alpha022 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay) {
    double param1 = quote.close(delay) - mean(quote.ts_close(6, delay));
    double param2 = quote.close(delay + 3) - mean(quote.ts_close(6, delay + 3));
    return param1 - param2;
  };
  return sma(ts<double>(12, fun), 1);
};


// SMA((CLOSE>DELAY(CLOSE,1)? STD(CLOSE,20):0),20,1)/
// (SMA((CLOSE>DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1)+SMA((CLOSE<=DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1))*
// 100
Alpha_fun alpha023 = [](const Quote& quote) -> double {
  auto fun1 = [&quote](const int delay) {
    if (quote.close(delay) > quote.close(delay + 1)) {
      return stdev(quote.ts_close(20, delay));
    } else {
      return 0.0;
    }
  };
  auto fun2 = [&quote](const int delay) {
    if (quote.close(delay) <= quote.close(delay + 1)) {
      return stdev(quote.ts_close(20, delay));
    } else {
      return 0.0;
    }
  };
  double sma1 = sma(ts<double>(20, fun1), 1);
  double sma2 = sma(ts<double>(20, fun2), 1);
  return sma1 / (sma1 + sma2) * 100;
};


// SMA(CLOSE-DELAY(CLOSE,5),5,1)
Alpha_fun alpha024 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay) {
    return quote.close(delay) - mean(quote.ts_close(5, delay));
  };
  return sma(ts<double>(5, fun), 1);
};


// (-1 * RANK((DELTA(CLOSE, 7) * (1 -RANK(DECAYLINEAR((VOLUME / MEAN(VOLUME,20)), 9)))))) *
// (1 + RANK(SUM(RET, 250)))
Alpha_fun alpha025 = [](const Quote& quote) -> double {
  return 1;
};


// ((SUM(CLOSE, 7) / 7) -CLOSE) + CORR(VWAP, DELAY(CLOSE, 5), 230)
Alpha_fun alpha026 = [](const Quote& quote) -> double {
  double rk_close_diff = mean(quote.ts_close(7)) - quote.close();
  double rk_corr = corr(quote.ts_vwap(230), quote.ts_close(230, 5));
  return rk_close_diff + rk_corr;
};


// WMA((CLOSE-DELAY(CLOSE,3))/DELAY(CLOSE,3)*100+(CLOSE-DELAY(CLOSE,6))/DELAY(CLOSE,6)*100,12)
Alpha_fun alpha027 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay) {
    double rk_close_ret_d3 = (quote.close(delay) - quote.close(delay + 3)) / quote.close(delay + 3);
    double rk_close_ret_d6 = (quote.close(delay) - quote.close(delay + 6)) / quote.close(delay + 6);
    return rk_close_ret_d3 * 100 + rk_close_ret_d6 * 100;
  };
  return wma(ts<double>(12, fun));
};


// 3*SMA((CLOSE-TSMIN(LOW,9))/(TSMAX(HIGH,9)-TSMIN(LOW,9))*100,3,1)-
// 2*SMA(SMA((CLOSE-TSMIN(LOW,9))/(TSMAX(HIGH,9)-TSMAX(LOW,9))*100,3,1),3,1)
Alpha_fun alpha028 = [](const Quote& quote) -> double {
  auto fun1 = [&quote](const int delay1) {
    double param1 = quote.close(delay1) - tsmin(quote.ts_low(9, delay1));
    double param2 = tsmax(quote.ts_high(9, delay1)) - tsmin(quote.ts_low(9, delay1));
    return param1 / param2 * 100;
  };
  auto fun2 = [&quote](const int delay1) {
    auto fun3 = [&quote, delay1](const int delay2) {
      double param1 = quote.close(delay1 + delay2) - tsmin(quote.ts_low(9, delay1 + delay2));
      double param2 = tsmax(quote.ts_high(9, delay1 + delay2)) - tsmin(quote.ts_low(9, delay1 + delay2));
      return param1 / param2 * 100;
    };
    return sma(ts<double>(3, fun3), 1);
  };
  return sma(ts<double>(3, fun1), 1) * 3 - sma(ts<double>(3, fun2), 1) * 2;
};




// COUNT(CLOSE>DELAY(CLOSE,1),12)/12*100
Alpha_fun alpha053 = [](const Quote& quote) -> double {
  auto fun = [&quote] (const int delay) {
    return quote.close(delay + 0) > quote.close(delay + 1);
  };
  return count(ts<bool>(12, fun)) / 12.0 * 100.0;
};

}
