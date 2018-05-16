#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

// (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE -OPEN) / OPEN)), 6))
Alpha_mfun alpha001 = [](const Quotes& quotes) -> Timeseries {

  const int num_day = 6;
  auto rk_d_log_vol = [&quotes](const int delay) {
    auto d_log_vol = [delay](const Quote& qt) {
      return log(qt.volume(delay)) - log(qt.volume(delay + 1));
    };
    return rank(quotes.apply(d_log_vol));
  };
  auto rk1 = ts<Timeseries>(num_day, rk_d_log_vol);

  auto rk_c_p = [&quotes](const int delay) {
    auto c_p = [delay](const Quote& qt) {
      return (qt.close(delay) - qt.close(delay)) / qt.open(delay);
    };
    return rank(quotes.apply(c_p));
  };
  auto rk2 = ts<Timeseries>(num_day, rk_c_p);

  Timeseries res;
  const int num_secu = quotes.size();
  for (int i = 0; i < num_secu; ++i) {
    Timeseries sub_rk1, sub_rk2;
    for (int j = 0; j < num_day; ++j) {
      sub_rk1.push_back(rk1[j][i]);
      sub_rk2.push_back(rk2[j][i]);
    }
    res.push_back(corr(sub_rk1, sub_rk2) * -1);
  }
  return res;
};


// (-1* DELTA((((CLOSE -LOW) -(HIGH -CLOSE)) / (HIGH -LOW)), 1))
Alpha_fun alpha002 = [](const Quote& quote) -> double {
  const auto rk_price = ((quote.ts_close(2) - quote.ts_low(2)) - (quote.ts_high(2) - quote.ts_close(2))) /
                        (quote.ts_high(2) - quote.ts_low(2));
  return sum(delta(rk_price, 1)) * -1;
};


// SUM((CLOSE=DELAY(CLOSE,1)?0:CLOSE-(CLOSE>DELAY(CLOSE,1)?
// MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),6)
Alpha_fun alpha003 = [](const Quote& quote) -> double {
  auto base_fun = [&quote](const int delay) {
    if (quote.close(delay) == quote.pclose(delay)) {
      return 0.0;
    } else if (quote.close(delay) > quote.pclose(delay)) {
      return quote.close(delay) - std::min(quote.low(delay), quote.pclose(delay));
    } else {
      return quote.close(delay) - std::min(quote.high(delay), quote.pclose(delay));
    }
  };
  return sum(ts<double>(6, base_fun));
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
  auto base_fun = [&quote](const int delay1) {
    auto volumn = [&quote, delay1] (const int delay2) {
      return tsrank(quote.ts_volume(5, delay2 + delay1));
    };
    auto high = [&quote, delay1] (const int delay2) {
      return tsrank(quote.ts_high(5, delay2 + delay1));
    };
    return corr(ts<double>(5, volumn), ts<double>(5, high));
  };
  return -tsmax(ts<double>(3, base_fun));
};


// (RANK(SIGN(DELTA((((OPEN * 0.85) + (HIGH * 0.15))), 4)))* -1)
Alpha_mfun alpha006 = [](const Quotes& quotes) -> Timeseries {
  auto base_fun = [](const Quote& qt) {
    double d_price = sum(delta(qt.ts_open(5) * 0.85 + qt.ts_high(5) * 0.15, 4));
    return sign(d_price);
  };
  return rank(quotes.apply(base_fun)) * 0.01;
};


// (RANK(MAX((VWAP -CLOSE), 3)) + RANK(MIN((VWAP -CLOSE), 3))) * RANK(DELTA(VOLUME, 3))
Alpha_mfun alpha007 = [](const Quotes& quotes) -> Timeseries {
  auto max_p = [](const Quote& qt) {
    return std::max(qt.vwap() - qt.close(), 3.0);
  };
  auto min_p = [](const Quote& qt) {
    return std::min(qt.vwap() - qt.close(), 3.0);
  };
  auto d_volume = [](const Quote& qt) {
    return qt.volume() - qt.volume(3);
  };
  return (rank(quotes.apply(max_p)) + rank(quotes.apply(min_p))) * quotes.apply(d_volume);
};


// RANK(DELTA(((((HIGH + LOW) / 2) * 0.2) + (VWAP * 0.8)), 4) * -1)
Alpha_mfun alpha008 = [](const Quotes& quotes) -> Timeseries {
  auto delta_p = [](const Quote& qt) {
    const auto price = (qt.ts_high(5) + qt.ts_low(5)) / 2 * 0.2 + qt.ts_vwap(5) * 0.8;
    const auto delta_price = sum(delta(price, 4));
    return -delta_price;
  };
  return rank(quotes.apply(delta_p));
};


// SMA(((HIGH+LOW)/2-(DELAY(HIGH,1)+DELAY(LOW,1))/2)*(HIGH-LOW)/VOLUME,7,2)
Alpha_fun alpha009 = [](const Quote& quote) -> double {
  const auto compose_price1 = (quote.ts_high(7) + quote.ts_low(7)) / 2 -
                              (quote.ts_high(7, 1) + quote.ts_low(7, 1)) / 2;
  const auto compose_price2 = (quote.ts_high(7) - quote.ts_low(7)) / quote.ts_volume(7);
  return sma((compose_price1 * compose_price2), 2);
};


// RANK(MAX(((RET < 0) ? STD(RET, 20) : CLOSE)^2, 5))
Alpha_mfun alpha010 = [](const Quotes& quotes) -> Timeseries {
  auto base_fun = [](const Quote& qt) {
    double comp;
    if (qt.ret() < 0) {
      comp = std::pow(stdev(qt.ts_ret(20)), 2.0);
    } else {
      comp =  std::pow(qt.close(), 2.0);
    }
    return std::max(comp, 5.0);
  };
  return rank(quotes.apply(base_fun));
};


// SUM(((CLOSE-LOW)-(HIGH-CLOSE))./(HIGH-LOW).*VOLUME,6)
Alpha_fun alpha011 = [](const Quote& quote) -> double {
  auto compose_pv = ((quote.ts_close(6) - quote.ts_low(6)) - (quote.ts_high(6) - quote.ts_close(6))) /
                    (quote.ts_high(6) - quote.ts_low(6)) * quote.ts_volume(6);
  return sum(compose_pv);
};


// (RANK((OPEN -(SUM(VWAP, 10) / 10)))) * (-1 * (RANK(ABS((CLOSE -VWAP)))))
Alpha_mfun alpha012 = [](const Quotes& quotes) -> Timeseries {
  auto vwap_o = [](const Quote& qt) {
    return qt.open() - mean(qt.ts_vwap(10));
  };
  auto vwap_c = [](const Quote& qt) {
    return std::abs(qt.close() - qt.vwap());
  };
  return rank(quotes.apply(vwap_o)) * (rank(quotes.apply(vwap_c)) * -1);
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
Alpha_mfun alpha016 = [](const Quotes& quotes) -> Timeseries {

  auto rk_corr = [&quotes](const int delay) {
    auto rk_volume = [delay, &quotes](const int delay2) {
      auto volume = [delay, delay2](const Quote& qt) {
        return qt.volume(delay + delay2);
      };
      return rank(quotes.apply(volume));
    };
    auto rk_vwap = [delay, &quotes](const int delay2) {
      auto vwap = [delay, delay2](const Quote& qt) {
        return qt.vwap(delay + delay2);
      };
      return rank(quotes.apply(vwap));
    };
    auto rk_v = ts<Timeseries>(5, rk_volume);
    auto rk_p = ts<Timeseries>(5, rk_vwap);
    Timeseries corr_res;
    const int num_secu = quotes.size();
    for (int i = 0; i < num_secu; ++i) {
      Timeseries sub_rk_v, sub_rk_p;
      for (int j = 0; j < 5; ++j) {
        sub_rk_v.push_back(rk_v[j][i]);
        sub_rk_p.push_back(rk_p[j][i]);
      }
      corr_res.push_back(corr(sub_rk_v, sub_rk_p));
    }
    Timeseries rank_corr_res = rank(corr_res);
    return rank_corr_res;
  };
  auto rank_corr_res = ts<Timeseries>(5, rk_corr);

  Timeseries tsmax_res;
  const int num_secu = quotes.size();
  for (int i = 0; i < num_secu; ++i) {
    Timeseries tsmax_res_t;
    for (int j = 0; j < 5; ++j) {
      tsmax_res_t.push_back(rank_corr_res[j][i]);
    }
    tsmax_res.push_back(tsmax(tsmax_res_t));
  }
  return tsmax_res * -1;
};


// RANK((VWAP -MAX(VWAP, 15)))^DELTA(CLOSE, 5)
Alpha_mfun alpha017 = [](const Quotes& quotes) -> Timeseries {
  auto vwap = [](const Quote& qt) {
    return qt.vwap() - std::max(qt.vwap(), 15.0);
  };
  auto rk_d_vwap = rank(quotes.apply(vwap));
  auto close = [](const Quote& qt) {
    return qt.close() - qt.close(5);
  };
  auto d_close = quotes.apply(close);
  return pow(rk_d_vwap, d_close);
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
  auto base_fun = [&quote](const int delay) {
    return mean(quote.ts_close(6, delay));
  };
  Timeseries quote_mean = ts<double>(6, base_fun);
  return regbeta(quote_mean, sequence(6));
};


// SMA(((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6)-DELAY((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6),3)),12,1)
Alpha_fun alpha022 = [](const Quote& quote) -> double {
  auto base_fun = [&quote](const int delay) {
    double price = quote.close(delay) - mean(quote.ts_close(6, delay));
    double d_price = quote.close(delay + 3) - mean(quote.ts_close(6, delay + 3));
    return price - d_price;
  };
  return sma(ts<double>(12, base_fun), 1);
};


// SMA((CLOSE>DELAY(CLOSE,1)? STD(CLOSE,20):0),20,1)/
// (SMA((CLOSE>DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1)+SMA((CLOSE<=DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1))*
// 100
Alpha_fun alpha023 = [](const Quote& quote) -> double {
  auto p_con1 = [&quote](const int delay) {
    if (quote.close(delay) > quote.close(delay + 1)) {
      return stdev(quote.ts_close(20, delay));
    } else {
      return 0.0;
    }
  };
  auto p_con2 = [&quote](const int delay) {
    if (quote.close(delay) <= quote.close(delay + 1)) {
      return stdev(quote.ts_close(20, delay));
    } else {
      return 0.0;
    }
  };
  double sma1 = sma(ts<double>(20, p_con1), 1);
  double sma2 = sma(ts<double>(20, p_con2), 1);
  return sma1 / (sma1 + sma2) * 100;
};


// SMA(CLOSE-DELAY(CLOSE,5),5,1)
Alpha_fun alpha024 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay) {
    return quote.close(delay) - quote.close(delay + 5);
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


// (CLOSE-DELAY(CLOSE,6))/DELAY(CLOSE,6)*VOLUME
Alpha_fun alpha029 = [](const Quote& quote) -> double {
  return (quote.close() - quote.close(6)) / quote.close(6) * quote.volume();
};


// WMA((REGRESI(CLOSE/DELAY(CLOSE)-1,MKT，60))^2,20)
Alpha_fun alpha030 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay) {
    Timeseries close_ret = quote.ts_close(60, delay) / quote.ts_close(60, 1 + delay) - 1;
    Timeseries bmk_close_ret = quote.ts_bmk_close(60, delay) / quote.ts_bmk_close(60, 1 + delay) - 1;
    return std::pow(regresi(close_ret, bmk_close_ret), 2.0);
  };
  return wma(ts<double>(20, fun));
};


// (CLOSE-MEAN(CLOSE,12))/MEAN(CLOSE,12)*100
Alpha_fun alpha031 = [](const Quote& quote) -> double {
  return (quote.close() - mean(quote.ts_close(12))) / mean(quote.ts_close(12)) * 100;
};


// (-1 * SUM(RANK(CORR(RANK(HIGH), RANK(VOLUME), 3)), 3))
Alpha_fun alpha032 = [](const Quote& quote) -> double {
  return 1;
};


// ((((-1 * TSMIN(LOW, 5)) + DELAY(TSMIN(LOW, 5), 5)) * RANK(((SUM(RET, 240) -SUM(RET, 20)) / 220))) * TSRANK(VOLUME, 5))
Alpha_fun alpha033 = [](const Quote& quote) -> double {
  return 1;
};


// MEAN(CLOSE,12)/CLOSE
Alpha_fun alpha034 = [](const Quote& quote) -> double {
  return mean(quote.ts_close(12)) / quote.close();
};


// (MIN(RANK(DECAYLINEAR(DELTA(OPEN, 1), 15)),
// RANK(DECAYLINEAR(CORR((VOLUME), ((OPEN * 0.65) + (OPEN *0.35)), 17),7))) * -1)
Alpha_fun alpha035 = [](const Quote& quote) -> double {
  return 1;
};


// RANK(SUM(CORR(RANK(VOLUME), RANK(VWAP)),6), 2)
Alpha_fun alpha036 = [](const Quote& quote) -> double {
  return 1;
};


// (-1 * RANK(((SUM(OPEN, 5) * SUM(RET, 5)) -DELAY((SUM(OPEN, 5) * SUM(RET, 5)), 10))))
Alpha_fun alpha037 = [](const Quote& quote) -> double {
  return 1;
};


// (((SUM(HIGH, 20) / 20) < HIGH) ? (-1 * DELTA(HIGH, 2)) : 0)
Alpha_fun alpha038 = [](const Quote& quote) -> double {
  if (mean(quote.ts_high(20)) < quote.high()) {
    return -sum(delta(quote.ts_high(3), 2));
  } else {
    return 0.0;
  }
};


// ((RANK(DECAYLINEAR(DELTA((CLOSE), 2),8)) -RANK(DECAYLINEAR(CORR(((VWAP * 0.3) + (OPEN * 0.7)),
// SUM(MEAN(VOLUME,180), 37), 14), 12))) * -1)
Alpha_fun alpha039 = [](const Quote& quote) -> double {
  return 1;
};


// SUM((CLOSE>DELAY(CLOSE,1)?VOLUME:0),26)/SUM((CLOSE<=DELAY(CLOSE,1)?VOLUME:0),26)*100
Alpha_fun alpha040 = [](const Quote& quote) -> double {
  auto fun1 = [&quote](const int delay) {
    if (quote.close(delay) > quote.close(1 + delay)) {
      return quote.volume(delay);
    } else {
      return 0.0;
    }
  };
  auto fun2 = [&quote](const int delay) {
    if (quote.close(delay) <= quote.close(1 + delay)) {
      return quote.volume(delay);
    } else {
      return 0.0;
    }
  };
  return sum(ts<double>(26, fun1)) / sum(ts<double>(26, fun2)) * 100;
};


// RANK(MAX(DELTA((VWAP), 3), 5))* -1
Alpha_fun alpha041 = [](const Quote& quote) -> double {
  return std::max(sum(delta(quote.ts_vwap(4), 3)), 5.0);
};


// (-1 * RANK(STD(HIGH, 10))) * CORR(HIGH, VOLUME, 10)
Alpha_fun alpha042 = [](const Quote& quote) -> double {
  return 1;
};


// SUM((CLOSE>DELAY(CLOSE,1)?VOLUME:(CLOSE<DELAY(CLOSE,1)?-VOLUME:0)),6)
Alpha_fun alpha043 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay) {
    if (quote.close(delay) > quote.close(delay + 1)) {
      return quote.volume(delay);
    } else if (quote.close(delay) < quote.close(delay + 1)) {
      return -quote.volume(delay);
    } else {
      return 0.0;
    }
  };
  return sum(ts<double>(6, fun));
};


// TSRANK(DECAYLINEAR(CORR((LOW), MEAN(VOLUME,10), 7), 6),4) +
// TSRANK(DECAYLINEAR(DELTA(VWAP, 3), 10), 15)
Alpha_fun alpha044 = [](const Quote& quote) -> double {
  auto fun1 = [&quote](const int delay1) {
    auto fun2 = [&quote, delay1](const int delay2) {
      Timeseries ts_low = quote.ts_low(7, delay1 + delay2);
      auto volume = [&quote, delay1, delay2](const int delay3) {
        return mean(quote.ts_volume(10, delay1 + delay2 + delay3));
      };
      Timeseries ts_volume = ts<double>(7, volume);
      return corr(ts_low, ts_volume);
    };
    return decaylinear(ts<double>(6, fun2));
  };
  auto fun3 = [&quote](const int delay1) {
    auto fun4 = [&quote, delay1](const int delay2) {
      return quote.vwap(delay1 + delay2) - quote.vwap(delay1 + delay2 + 3);
    };
    return decaylinear(ts<double>(10, fun4));
  };

  return tsrank(ts<double>(4, fun1)) + tsrank(ts<double>(15, fun3));
};


// COUNT(CLOSE>DELAY(CLOSE,1),12)/12*100
Alpha_fun alpha053 = [](const Quote& quote) -> double {
  auto fun = [&quote] (const int delay) {
    return quote.close(delay + 0) > quote.close(delay + 1);
  };
  return count(ts<bool>(12, fun)) / 12.0 * 100.0;
};





}
