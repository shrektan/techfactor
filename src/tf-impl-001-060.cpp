#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

// (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE -OPEN) / OPEN)), 6))
Alpha_mfun alpha001 = [](const Quotes& qts) -> Timeseries {

  const int num_day = 6;
  auto d_log_vol = [](const Quote& qt) {
    return log(qt.volume()) - log(qt.volume(1));
  };
  auto rk_d_log_vol = [d_log_vol](const Quotes& qts) {
    return rank(qts.apply(d_log_vol));
  };
  auto c_p = [](const Quote& qt) {
    return (qt.close() - qt.close()) / qt.open();
  };
  auto rk_c_p = [c_p](const Quotes& qts) {
    return rank(qts.apply(c_p));
  };
  auto rk1 = qts.tsapply(num_day, rk_d_log_vol);
  auto rk2 = qts.tsapply(num_day, rk_c_p);

  Timeseries res;
  const int num_secu = qts.size();
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
  const auto left =
    (quote.ts_close(2) - quote.ts_low(2)) -
    (quote.ts_high(2) - quote.ts_close(2));
  const auto right = quote.ts_high(2) - quote.ts_low(2);
  return delta(left / right) * -1;
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
  auto open_high = [](const Quote& qt) {
    return qt.open() * 0.85 + qt.high() * 0.15;
  };
  auto sign_delta = [open_high](const Quote& qt) {
    return sign(delta(qt.ts<double>(5, open_high)));
  };
  return rank(quotes.apply(sign_delta)) * -1.0;
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
  const auto left = rank(quotes.apply(max_p)) + rank(quotes.apply(min_p));
  const auto right = quotes.apply(d_volume);
  return left * right;
};


// RANK(DELTA(((((HIGH + LOW) / 2) * 0.2) + (VWAP * 0.8)), 4) * -1)
Alpha_mfun alpha008 = [](const Quotes& quotes) -> Timeseries {
  auto delta_p = [](const Quote& qt) {
    const auto price = (qt.ts_high(5) + qt.ts_low(5)) / 2 * 0.2 + qt.ts_vwap(5) * 0.8;
    const auto delta_price = delta(price);
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
  const auto left =
    (quote.ts_close(6) - quote.ts_low(6)) -
    (quote.ts_high(6) - quote.ts_close(6));
  const auto right = quote.ts_high(6) - quote.ts_low(6);
  return sum(left / right * quote.ts_volume(6));
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
  if (ISNA(quote.high()) || ISNA(quote.low()) || ISNA(quote.vwap()) ||
      quote.high() * quote.low() <= 0.0) {
    return NA_REAL;
  }
  return std::pow(quote.high() * quote.low(), 0.5) - quote.vwap();
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


// SMA(((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6)-
// DELAY((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6),3)),12,1)
Alpha_fun alpha022 = [](const Quote& quote) -> double {
  auto base_fun = [&quote](const int delay) {
    double price = quote.close(delay) - mean(quote.ts_close(6, delay));
    double d_price = quote.close(delay + 3) - mean(quote.ts_close(6, delay + 3));
    return price - d_price;
  };
  return sma(ts<double>(12, base_fun), 1);
};


// SMA((CLOSE>DELAY(CLOSE,1)? STD(CLOSE,20):0),20,1)/
// (SMA((CLOSE>DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1)+
// SMA((CLOSE<=DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1))*
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
  if (sma1 + sma2 == 0.0) return NA_REAL;
  return sma1 / (sma1 + sma2) * 100;
};


// SMA(CLOSE-DELAY(CLOSE,5),5,1)
Alpha_fun alpha024 = [](const Quote& quote) -> double {
  auto base_fun = [&quote](const int delay) {
    return quote.close(delay) - quote.close(delay + 5);
  };
  return sma(ts<double>(5, base_fun), 1);
};


// (-1 * RANK((DELTA(CLOSE, 7) *
// (1 -RANK(DECAYLINEAR((VOLUME / MEAN(VOLUME,20)), 9)))))) *
// (1 + RANK(SUM(RET, 250)))
Alpha_mfun alpha025 = [](const Quotes& quotes) -> Timeseries {
  auto d_close = [](const Quote& qt) {
    return qt.close() - qt.close(7);
  };
  auto sum_ret = [](const Quote& qt) {
    return sum(qt.ts_ret(250));
  };
  auto decay_linear = [](const Quote& qt) {
    auto volume = [&qt](const int delay) {
      return qt.volume(delay) - mean(qt.ts_volume(20, delay));
    };
    return decaylinear(ts<double>(9, volume));
  };
  Timeseries delta_close = quotes.apply(d_close);
  Timeseries rk_sum_ret = rank(quotes.apply(sum_ret));
  Timeseries rk_decay_linear = rank(quotes.apply(decay_linear));
  return rank(delta_close * (rk_decay_linear * -1.0 + 1.0)) *
    (rk_sum_ret * -1.0 + 1.0) * -1.0;
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
    double rk_close_ret_d3 =
      (quote.close(delay) - quote.close(delay + 3)) /
        quote.close(delay + 3);
    double rk_close_ret_d6 =
      (quote.close(delay) - quote.close(delay + 6)) /
      quote.close(delay + 6);
    return rk_close_ret_d3 * 100 + rk_close_ret_d6 * 100;
  };
  return wma(ts<double>(12, fun));
};


// 3*SMA((CLOSE-TSMIN(LOW,9))/(TSMAX(HIGH,9)-TSMIN(LOW,9))*100,3,1)-
// 2*SMA(SMA((CLOSE-TSMIN(LOW,9))/(TSMAX(HIGH,9)-TSMAX(LOW,9))*100,3,1),3,1)
Alpha_fun alpha028 = [](const Quote& quote) -> double {
  auto ts_p = [&quote](const int delay1) {
    double param1 = quote.close(delay1) - tsmin(quote.ts_low(9, delay1));
    double param2 = tsmax(quote.ts_high(9, delay1)) - tsmin(quote.ts_low(9, delay1));
    return param1 / param2 * 100;
  };
  auto ts_p_sma = [&quote](const int delay1) {
    auto p_sma = [&quote, delay1](const int delay2) {
      double param1 =
        quote.close(delay1 + delay2) -
        tsmin(quote.ts_low(9, delay1 + delay2));
      double param2 =
        tsmax(quote.ts_high(9, delay1 + delay2)) -
        tsmin(quote.ts_low(9, delay1 + delay2));
      return param1 / param2 * 100;
    };
    return sma(ts<double>(3, p_sma), 1);
  };
  return sma(ts<double>(3, ts_p), 1) * 3 - sma(ts<double>(3, ts_p_sma), 1) * 2;
};


// (CLOSE-DELAY(CLOSE,6))/DELAY(CLOSE,6)*VOLUME
Alpha_fun alpha029 = [](const Quote& quote) -> double {
  return (quote.close() - quote.close(6)) / quote.close(6) * quote.volume();
};


// WMA((REGRESI(CLOSE/DELAY(CLOSE)-1,MKT，60))^2,20)
Alpha_fun alpha030 = [](const Quote& quote) -> double {
  auto base_fun = [&quote](const int delay) {
    Timeseries close_ret =
      quote.ts_close(60, delay) / quote.ts_close(60, 1 + delay) - 1;
    Timeseries bmk_close_ret =
      quote.ts_bmk_close(60, delay) / quote.ts_bmk_close(60, 1 + delay) - 1;
    return std::pow(regresi(close_ret, bmk_close_ret), 2.0);
  };
  return wma(ts<double>(20, base_fun));
};


// (CLOSE-MEAN(CLOSE,12))/MEAN(CLOSE,12)*100
Alpha_fun alpha031 = [](const Quote& quote) -> double {
  return (quote.close() - mean(quote.ts_close(12))) /
    mean(quote.ts_close(12)) * 100;
};


// (-1 * SUM(RANK(CORR(RANK(HIGH), RANK(VOLUME), 3)), 3))
Alpha_mfun alpha032 = [](const Quotes& quotes) -> Timeseries {
  auto base_fun = [&quotes](const int delay) {
    auto rk_high = [&quotes, delay](const int delay2) {
      auto high = [delay, delay2](const Quote& qt) {
        return qt.high(delay + delay2);
      };
      return rank(quotes.apply(high));
    };
    auto rk_volume = [&quotes, delay](const int delay2) {
      auto volume = [delay, delay2](const Quote& qt) {
        return qt.volume(delay + delay2);
      };
      return rank(quotes.apply(volume));
    };
    auto rk1 = ts<Timeseries>(3, rk_high);
    auto rk2 = ts<Timeseries>(3, rk_volume);

    Timeseries res;
    const int num_secu = quotes.size();
    for (int i = 0; i < num_secu; ++i) {
      Timeseries sub_rk1, sub_rk2;
      for (int j = 0; j < 3; ++j) {
        sub_rk1.push_back(rk1[j][i]);
        sub_rk2.push_back(rk2[j][i]);
      }
      res.push_back(corr(sub_rk1, sub_rk2));
    }
    return rank(res);
  };
  auto rk_corr = ts<Timeseries>(3, base_fun);

  Timeseries rk_sum;
  const int num_secu = quotes.size();
  for (int i = 0; i < num_secu; ++i) {
    Timeseries sub_rk_sum;
    for (int j = 0; j < 3; ++j) {
      sub_rk_sum.push_back(rk_corr[j][i]);
    }
    rk_sum.push_back(sum(sub_rk_sum));
  }
  return rk_sum * -1.0;
};


// ((-1 * TSMIN(LOW, 5)) + DELAY(TSMIN(LOW, 5), 5)) *
// RANK((SUM(RET, 240) -SUM(RET, 20)) / 220) * TSRANK(VOLUME, 5)
Alpha_mfun alpha033 = [](const Quotes& quotes) -> Timeseries {
  auto ts_rk_volume = [](const Quote& qt) {
    return tsrank(qt.ts_volume(5));
  };
  auto ret = [](const Quote& qt) {
    return (sum(qt.ts_ret(240)) - sum(qt.ts_ret(20))) / 220;
  };
  auto ts_min_low = [](const Quote& qt) {
    double min_low = tsmin(qt.ts_low(5));
    double d_min_low = tsmin(qt.ts_low(5)) - tsmin(qt.ts_low(5, 5));
    return -1.0 * min_low + d_min_low;
  };
  auto ts_rank_v = quotes.apply(ts_rk_volume);
  auto rank_ret = rank(quotes.apply(ret));
  auto ts_min_dl = quotes.apply(ts_min_low);
  return ts_min_dl * rank_ret * ts_rank_v;
};


// MEAN(CLOSE,12)/CLOSE
Alpha_fun alpha034 = [](const Quote& quote) -> double {
  return mean(quote.ts_close(12)) / quote.close();
};


// (MIN(RANK(DECAYLINEAR(DELTA(OPEN, 1), 15)),
// RANK(DECAYLINEAR(CORR((VOLUME),
// ((OPEN * 0.65) + (OPEN *0.35)), 17),7))) * -1)
Alpha_mfun alpha035 = [](const Quotes& quotes) -> Timeseries {
  auto base_fun1 = [](const Quote& qt) {
    auto d_open = [&qt](const int delay) {
      return qt.open(delay) - qt.open(delay + 1);
    };
    return decaylinear(ts<double>(15, d_open));
  };
  auto base_fun2 = [](const Quote& qt) {
    auto corr_open_volume = [&qt](const int delay) {
      return corr(qt.ts_volume(17), qt.ts_open(17));
    };
    return decaylinear(ts<double>(7, corr_open_volume));
  };
  auto rk_decay1 = rank(quotes.apply(base_fun1));
  auto rk_decay2 = rank(quotes.apply(base_fun2));

  return std::min(rk_decay1, rk_decay2) * -1.0;
};


// RANK(SUM(CORR(RANK(VOLUME), RANK(VWAP),6), 2))
Alpha_mfun alpha036 = [](const Quotes& quotes) -> Timeseries {
  auto base_fun = [&quotes](const int delay) {
    auto rk_volume = [&quotes, delay](const int delay2) {
      auto volume = [delay, delay2](const Quote& qt) {
        return qt.volume(delay + delay2);
      };
      return rank(quotes.apply(volume));
    };
    auto rk_vwap = [&quotes, delay](const int delay2) {
      auto vwap = [delay, delay2](const Quote& qt) {
        return qt.vwap(delay + delay2);
      };
      return rank(quotes.apply(vwap));
    };
    auto rk1 = ts<Timeseries>(6, rk_volume);
    auto rk2 = ts<Timeseries>(6, rk_vwap);

    Timeseries res;
    const int num_secu = quotes.size();
    for (int i = 0; i < num_secu; ++i) {
      Timeseries sub_rk1, sub_rk2;
      for (int j = 0; j < 3; ++j) {
        sub_rk1.push_back(rk1[j][i]);
        sub_rk2.push_back(rk2[j][i]);
      }
      res.push_back(corr(sub_rk1, sub_rk2));
    }
    return res;
  };
  auto rk_corr = ts<Timeseries>(2, base_fun);

  Timeseries rk_sum;
  const int num_secu = quotes.size();
  for (int i = 0; i < num_secu; ++i) {
    Timeseries sub_rk_sum;
    for (int j = 0; j < 2; ++j) {
      sub_rk_sum.push_back(rk_corr[j][i]);
    }
    rk_sum.push_back(sum(sub_rk_sum));
  }
  return rank(rk_sum);
};


// -1 * RANK((SUM(OPEN, 5) * SUM(RET, 5)) -DELAY((SUM(OPEN, 5) * SUM(RET, 5)), 10))
Alpha_mfun alpha037 = [](const Quotes& quotes) -> Timeseries {
  auto sum_open_ret = [](const Quote& qt) {
    double open_ret = sum(qt.ts_open(5)) * sum(qt.ts_ret(5));
    double d_open_ret =
      sum(qt.ts_open(5)) * sum(qt.ts_ret(5)) -
      (sum(qt.ts_open(5, 10)) * sum(qt.ts_ret(5, 10)));
    return open_ret - d_open_ret;
  };
  return rank(quotes.apply(sum_open_ret)) * -1.0;
};


// (((SUM(HIGH, 20) / 20) < HIGH) ? (-1 * DELTA(HIGH, 2)) : 0)
Alpha_fun alpha038 = [](const Quote& quote) -> double {
  if (mean(quote.ts_high(20)) < quote.high()) {
    return -delta(quote.ts_high(3));
  } else {
    return 0.0;
  }
};


// (RANK(DECAYLINEAR(DELTA((CLOSE), 2),8)) -
// RANK(DECAYLINEAR(CORR(((VWAP * 0.3) + (OPEN * 0.7)),
// SUM(MEAN(VOLUME,180), 37), 14), 12))) * -1
Alpha_mfun alpha039 = [](const Quotes& quotes) -> Timeseries {
  auto decay_p = [](const Quote& qt) {
    auto d_close = [&qt](const int delay) {
      return qt.close(delay) - qt.close(delay + 2);
    };
    return decaylinear(ts<double>(8, d_close));
  };
  auto decay_v = [](const Quote& qt) {
    auto corr_p = [&qt](const int delay) {
      auto vo = [&qt, delay](const int delay2) {
        return qt.vwap(delay + delay2) * 0.3 +
          qt.open(delay + delay2) * 0.7;
      };
      auto sum_v = [&qt, delay](const int delay2) {
        auto volume = [&qt, delay, delay2](const int delay3) {
          return mean((qt.ts_volume(180, delay + delay2 + delay3)));
        };
        return sum(ts<double>(37, volume));
      };
      auto ts_vo = ts<double>(14, vo);
      auto ts_sum_v = ts<double>(14, sum_v);
      return corr(ts_vo, ts_sum_v);
    };
    return decaylinear(ts<double>(12, corr_p));
  };
  auto rk1 = rank(quotes.apply(decay_p));
  auto rk2 = rank(quotes.apply(decay_v));
  return (rk1 - rk2) * -1.0;
};


// SUM((CLOSE>DELAY(CLOSE,1)?VOLUME:0),26)/
// SUM((CLOSE<=DELAY(CLOSE,1)?VOLUME:0),26)*100
Alpha_fun alpha040 = [](const Quote& quote) -> double {
  auto base_fun1 = [&quote](const int delay) {
    if (quote.close(delay) > quote.close(1 + delay)) {
      return quote.volume(delay);
    } else {
      return 0.0;
    }
  };
  auto base_fun2 = [&quote](const int delay) {
    if (quote.close(delay) <= quote.close(1 + delay)) {
      return quote.volume(delay);
    } else {
      return 0.0;
    }
  };

  auto right = sum(ts<double>(26, base_fun2));
  if (right == 0.0) return NA_REAL;
  auto left = sum(ts<double>(26, base_fun1));
  return left / right * 100;
};


// RANK(MAX(DELTA((VWAP), 3), 5))* -1
Alpha_mfun alpha041 = [](const Quotes& quotes) -> Timeseries {
  auto max_d_vwap = [](const Quote& qt) {
    return std::max(delta(qt.ts_vwap(4)), 5.0);
  };
  return rank(quotes.apply(max_d_vwap)) * -1.0;
};


// (-1 * RANK(STD(HIGH, 10))) * CORR(HIGH, VOLUME, 10)
Alpha_mfun alpha042 = [](const Quotes& quotes) -> Timeseries {
  auto std_high = [](const Quote& qt) {
    return stdev(qt.ts_high(10));
  };
  auto corr_high_vol = [](const Quote& qt) {
    return corr(qt.ts_high(10), qt.ts_volume(10));
  };
  return (rank(quotes.apply(std_high)) * quotes.apply(corr_high_vol)) * -1.0;
};


// SUM((CLOSE>DELAY(CLOSE,1)?VOLUME:(CLOSE<DELAY(CLOSE,1)?-VOLUME:0)),6)
Alpha_fun alpha043 = [](const Quote& quote) -> double {
  auto base_fun = [&quote](const int delay) {
    if (quote.close(delay) > quote.close(delay + 1)) {
      return quote.volume(delay);
    } else if (quote.close(delay) < quote.close(delay + 1)) {
      return -quote.volume(delay);
    } else {
      return 0.0;
    }
  };
  return sum(ts<double>(6, base_fun));
};


// TSRANK(DECAYLINEAR(CORR((LOW), MEAN(VOLUME,10), 7), 6),4) +
// TSRANK(DECAYLINEAR(DELTA(VWAP, 3), 10), 15)
Alpha_fun alpha044 = [](const Quote& quote) -> double {
  auto decay_corr = [&quote](const int delay1) {
    auto muti_low_vol = [&quote, delay1](const int delay2) {
      Timeseries ts_low = quote.ts_low(7, delay1 + delay2);
      auto volume = [&quote, delay1, delay2](const int delay3) {
        return mean(quote.ts_volume(10, delay1 + delay2 + delay3));
      };
      Timeseries ts_volume = ts<double>(7, volume);
      return corr(ts_low, ts_volume);
    };
    return decaylinear(ts<double>(6, muti_low_vol));
  };
  auto decay_delta = [&quote](const int delay1) {
    auto vwap = [&quote, delay1](const int delay2) {
      return quote.vwap(delay1 + delay2) - quote.vwap(delay1 + delay2 + 3);
    };
    return decaylinear(ts<double>(10, vwap));
  };

  return tsrank(ts<double>(4, decay_corr)) + tsrank(ts<double>(15, decay_delta));
};


// RANK(DELTA((CLOSE * 0.6) + (OPEN*0.4), 1)) * RANK(CORR(VWAP, MEAN(VOLUME,150), 15))
Alpha_mfun alpha045 = [](const Quotes& quotes) -> Timeseries {
  auto d_close_open = [](const Quote& qt) {
    return qt.close() * 0.6 + qt.open() * 0.4 - (qt.close(1) * 0.6 + qt.open(1) * 0.4);
  };
  auto corr_vwap_vol = [](const Quote& qt) {
    auto volume = [&qt](const int delay) {
      return mean(qt.ts_volume(150, delay));
    };
    return corr(qt.ts_vwap(15), ts<double>(15, volume));
  };
  return rank(quotes.apply(d_close_open)) * rank(quotes.apply(corr_vwap_vol));
};


// (MEAN(CLOSE,3)+MEAN(CLOSE,6)+MEAN(CLOSE,12)+MEAN(CLOSE,24))/(4*CLOSE)
Alpha_fun alpha046 = [](const Quote& quote) -> double {
  double mean_p = mean(quote.ts_close(3)) + mean(quote.ts_close(6)) +
                  mean(quote.ts_close(12)) + mean(quote.ts_close(24));
  return mean_p / (4 * quote.close());
};


// SMA((TSMAX(HIGH,6)-CLOSE)/(TSMAX(HIGH,6)-TSMIN(LOW,6))*100,9,1)
Alpha_fun alpha047 = [](const Quote& quote) -> double {
  auto base_fun = [&quote](const int delay) {
    return (tsmax(quote.ts_high(6, delay)) - quote.close(delay)) /
           (tsmax(quote.ts_high(6, delay)) - tsmin(quote.ts_low(6, delay))) * 100;
  };
  return sma(ts<double>(9, base_fun), 1);
};


// -1*(RANK((SIGN((CLOSE -DELAY(CLOSE,1))) +
// SIGN((DELAY(CLOSE, 1) -DELAY(CLOSE, 2)))) +
// SIGN((DELAY(CLOSE, 2) -DELAY(CLOSE, 3))))) *
// SUM(VOLUME, 5) / SUM(VOLUME, 20)
Alpha_mfun alpha048 = [](const Quotes& quotes) -> Timeseries {
  auto sum_vol = [](const Quote& qt) {
    return sum(qt.ts_volume(5)) / sum(qt.ts_volume(20));
  };
  auto sign_p = [](const Quote& qt) {
    double sign1 = sign(qt.close() - qt.close(1));
    double sign2 = sign(qt.close(1) - qt.close(2));
    double sign3 = sign(qt.close(2) - qt.close(3));
    return (sign1 + sign2 + sign3);
  };
  return (rank(quotes.apply(sign_p)) * -1.0) * quotes.apply(sum_vol);
};


// SUM(((HIGH+LOW)>=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12) /
// (SUM(((HIGH+LOW)>=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12)+
// SUM(((HIGH+LOW)<=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12))
Alpha_fun alpha049 = [](const Quote& qt) -> double {
  auto sum1 = [](const Quote& qt) {
    if ((qt.high() + qt.low()) <= (qt.high(1) + qt.low(1))) {
      return 0.0;
    } else {
      double hd1 = std::abs(qt.high() - qt.high(1));
      double hd2 = std::abs(qt.low() - qt.low(1));
      return std::max(hd1, hd2);
    };
  };
  auto sum2 = [](const Quote& qt) {
    if ((qt.high() + qt.low()) >= (qt.high(1) + qt.low(1))) {
      return 0.0;
    } else {
      double hd1 = std::abs(qt.high() - qt.high(1));
      double hd2 = std::abs(qt.low() - qt.low(1));
      return std::max(hd1, hd2);
    };
  };
  auto sum_ts1 = sum(qt.ts<double>(12, sum1));
  auto sum_ts2 = sum(qt.ts<double>(12, sum2));
  return sum_ts2 / (sum_ts1 + sum_ts2);
};


// SUM(((HIGH+LOW)<=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12) /
// (SUM(((HIGH+LOW)<=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12)+
// SUM(((HIGH+LOW)>=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12))-
// SUM(((HIGH+LOW)>=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12)/
// (SUM(((HIGH+LOW)>=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12)+
// SUM(((HIGH+LOW)<=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12))
Alpha_fun alpha050 = [](const Quote& qt) -> double {
  auto sum1 = [](const Quote& qt) {
    if ((qt.high() + qt.low()) <= (qt.high(1) + qt.low(1))) {
      return 0.0;
    } else {
      double hd1 = std::abs(qt.high() - qt.high(1));
      double hd2 = std::abs(qt.low() - qt.low(1));
      return std::max(hd1, hd2);
    };
  };
  auto sum2 = [](const Quote& qt) {
    if ((qt.high() + qt.low()) >= (qt.high(1) + qt.low(1))) {
      return 0.0;
    } else {
      double hd1 = std::abs(qt.high() - qt.high(1));
      double hd2 = std::abs(qt.low() - qt.low(1));
      return std::max(hd1, hd2);
    };
  };
  auto sum_ts1 = sum(qt.ts<double>(12, sum1));
  auto sum_ts2 = sum(qt.ts<double>(12, sum2));
  return (sum_ts1 - sum_ts2) / (sum_ts1 + sum_ts2);
};


// SUM(((HIGH+LOW)<=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12) /
// (SUM(((HIGH+LOW)<=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12)+
// SUM(((HIGH+LOW)>=(DELAY(HIGH,1)+DELAY(LOW,1))?
// 0:MAX(ABS(HIGH-DELAY(HIGH,1)),ABS(LOW-DELAY(LOW,1)))),12))
Alpha_fun alpha051 = [](const Quote& qt) -> double {
  auto sum1 = [](const Quote& qt) {
    if ((qt.high() + qt.low()) <= (qt.high(1) + qt.low(1))) {
      return 0.0;
    } else {
      double hd1 = std::abs(qt.high() - qt.high(1));
      double hd2 = std::abs(qt.low() - qt.low(1));
      return std::max(hd1, hd2);
    };
  };
  auto sum2 = [](const Quote& qt) {
    if ((qt.high() + qt.low()) >= (qt.high(1) + qt.low(1))) {
      return 0.0;
    } else {
      double hd1 = std::abs(qt.high() - qt.high(1));
      double hd2 = std::abs(qt.low() - qt.low(1));
      return std::max(hd1, hd2);
    };
  };
  auto sum_ts1 = sum(qt.ts<double>(12, sum1));
  auto sum_ts2 = sum(qt.ts<double>(12, sum2));
  return sum_ts1 / (sum_ts1 + sum_ts2);
};


// SUM(MAX(0,HIGH-DELAY((HIGH+LOW+CLOSE)/3,1)),26)/
// SUM(MAX(0,DELAY((HIGH+LOW+CLOSE)/3,1)-LOW),26)*100
Alpha_fun alpha052 = [](const Quote& qt) -> double {
  auto sum1 = [](const Quote& qt) {
    return std::max(0.0, qt.high() - (qt.high(1) + qt.low(1) + qt.close(1)) / 3);
  };
  auto sum2 = [](const Quote& qt) {
    return std::max(0.0, (qt.high(1) + qt.low(1) + qt.close(1)) / 3 - qt.low());
  };
  auto sum_ts1 = sum(qt.ts<double>(26, sum1));
  auto sum_ts2 = sum(qt.ts<double>(26, sum2));
  return sum_ts1 / sum_ts2 * 100;
};



// COUNT(CLOSE>DELAY(CLOSE,1),12)/12*100
Alpha_fun alpha053 = [](const Quote& qt) -> double {
  auto bool_fun = [] (const Quote& qt) {
    return qt.close() > qt.close(1);
  };
  return count(qt.ts<bool>(12, bool_fun)) / 12.0 * 100.0;
};



// -1 * RANK((STD(ABS(CLOSE -OPEN), 10) + (CLOSE -OPEN)) + CORR(CLOSE, OPEN,10))
Alpha_mfun alpha054 = [](const Quotes& qts) -> Timeseries {
  auto muti_stat = [](const Quote& qt) {
    double stat1 = stdev(abs(qt.ts_close(10) - qt.ts_open(10)));
    double stat2 = qt.close() - qt.open();
    double stat3 = corr(qt.ts_close(10), qt.ts_open(10));
    return stat1 + stat2 + stat3;
  };
  return rank(qts.apply(muti_stat)) * -1.0;
};


// SUM(16*(CLOSE-DELAY(CLOSE,1)+(CLOSE-OPEN)/2+DELAY(CLOSE,1)-DELAY(OPEN,1))/
// ((ABS(HIGH-DELAY(CLOSE,1))>ABS(LOW-DELAY(CLOSE,1)) &
// ABS(HIGH-DELAY(CLOSE,1))>ABS(HIGH-DELAY(LOW,1))?
// ABS(HIGH-DELAY(CLOSE,1))+ABS(LOW-DELAY(CLOSE,1))/2+ABS(DELAY(CLOSE,1)-DELAY(OPEN,1))/4:
// (ABS(LOW-DELAY(CLOSE,1))>ABS(HIGH-DELAY(LOW,1)) & ABS(LOW-DELAY(CLOSE,1))>ABS(HIGH-DELAY(CLOSE,1))?
// ABS(LOW-DELAY(CLOSE,1))+ABS(HIGH-DELAY(CLOSE,1))/2+ABS(DELAY(CLOSE,1)-DELAY(OPEN,1))/4:
// ABS(HIGH-DELAY(LOW,1))+ABS(DELAY(CLOSE,1)-DELAY(OPEN,1))/4)))*MAX(ABS(HIGH-DELAY(CLOSE,1)),ABS(LOW-DELAY(CLOSE,1))),20)


}
