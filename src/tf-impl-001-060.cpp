#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

// (-1 * CORR(RANK(DELTA(LOG(VOLUME), 1)), RANK(((CLOSE -OPEN) / OPEN)), 6))
Alpha_mfun alpha001 = [](const Quotes& qts) -> Timeseries {
  auto d_log_vol = [](const Quote& qt) {
    return log(qt.volume()) - log(qt.volume(1));
  };
  auto rk_d_log_vol = [d_log_vol](const Quotes& qts) {
    return rank(qts.apply(d_log_vol));
  };
  auto c_p = [](const Quote& qt) {
    return (qt.close() - qt.open()) / qt.open();
  };
  auto rk_c_p = [c_p](const Quotes& qts) {
    return rank(qts.apply(c_p));
  };
  auto rk1 = qts.tsapply(6, rk_d_log_vol);
  auto rk2 = qts.tsapply(6, rk_c_p);
  auto corr_rk = apply(rk1, rk2, corr);
  return corr_rk * -1.0;
};


// (-1* DELTA((((CLOSE -LOW) -(HIGH -CLOSE)) / (HIGH -LOW)), 1))
Alpha_fun alpha002 = [](const Quote& qt) -> double {
  const auto left =
    (qt.ts_close(2) - qt.ts_low(2)) -
    (qt.ts_high(2) - qt.ts_close(2));
  const auto right = qt.ts_high(2) - qt.ts_low(2);
  return delta(left / right) * -1;
};


// SUM((CLOSE=DELAY(CLOSE,1)?0:CLOSE-(CLOSE>DELAY(CLOSE,1)?
// MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),6)
Alpha_fun alpha003 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    if (qt.close() == qt.pclose()) {
      return 0.0;
    } else if (qt.close() > qt.pclose()) {
      return qt.close() - std::min(qt.low(), qt.pclose());
    } else {
      return qt.close() - std::min(qt.high(), qt.pclose());
    }
  };
  return sum(qt.ts<double>(6, base_fun));
};


// ((((SUM(CLOSE, 8) / 8) + STD(CLOSE, 8)) < (SUM(CLOSE, 2) / 2)) ?
// (-1 * 1) : (((SUM(CLOSE, 2) / 2) < ((SUM(CLOSE, 8) / 8) -STD(CLOSE, 8))) ?
// 1 : (((1 < (VOLUME / MEAN(VOLUME,20))) || ((VOLUME / MEAN(VOLUME,20)) == 1)) ? 1 : (-1 * 1))))
Alpha_fun alpha004 = [](const Quote& qt) -> double {
  const auto sum_close_8 = sum(qt.ts_close(8));
  const auto std_close_8 = stdev(qt.ts_close(8));
  const auto sum_close_2 = sum(qt.ts_close(2));
  if (sum_close_8 / 8 + std_close_8 < sum_close_2 / 2) {
    return -1.0;
  } else if (sum_close_2 / 2 < sum_close_8 / 8 - std_close_8) {
    return 1.0;
  } else {
    const auto vol = qt.volume();
    const auto mean_vol_20 = mean(qt.ts_volume(20));
    if (vol / mean_vol_20 >= 1) {
      return 1.0;
    } else {
      return -1.0;
    }
  }
};


// (-1 * TSMAX(CORR(TSRANK(VOLUME, 5), TSRANK(HIGH, 5), 5), 3))
Alpha_fun alpha005 = [](const Quote& qt) -> double {
  auto volumn = [] (const Quote& qt) {
    return tsrank(qt.ts_volume(5));
  };
  auto high = [] (const Quote& qt) {
    return tsrank(qt.ts_high(5));
  };
  auto base_fun = [volumn, high](const Quote& qt) {
    return corr(qt.ts<double>(5, volumn), qt.ts<double>(5, high));
  };
  return -tsmax(qt.ts<double>(3, base_fun));
};


// (RANK(SIGN(DELTA((((OPEN * 0.85) + (HIGH * 0.15))), 4)))* -1)
Alpha_mfun alpha006 = [](const Quotes& qts) -> Timeseries {
  auto open_high = [](const Quote& qt) {
    return qt.open() * 0.85 + qt.high() * 0.15;
  };
  auto sign_delta = [open_high](const Quote& qt) {
    return sign(delta(qt.ts<double>(5, open_high)));
  };
  return rank(qts.apply(sign_delta)) * -1.0;
};


// (RANK(MAX((VWAP -CLOSE), 3)) + RANK(MIN((VWAP -CLOSE), 3))) * RANK(DELTA(VOLUME, 3))
Alpha_mfun alpha007 = [](const Quotes& qts) -> Timeseries {
  auto max_p = [](const Quote& qt) {
    return std::max(qt.vwap() - qt.close(), 3.0);
  };
  auto min_p = [](const Quote& qt) {
    return std::min(qt.vwap() - qt.close(), 3.0);
  };
  auto d_volume = [](const Quote& qt) {
    return qt.volume() - qt.volume(3);
  };
  const auto left = rank(qts.apply(max_p)) + rank(qts.apply(min_p));
  const auto right = qts.apply(d_volume);
  return left * right;
};


// RANK(DELTA(((((HIGH + LOW) / 2) * 0.2) + (VWAP * 0.8)), 4) * -1)
Alpha_mfun alpha008 = [](const Quotes& qts) -> Timeseries {
  auto delta_p = [](const Quote& qt) {
    const auto price = (qt.ts_high(5) + qt.ts_low(5)) / 2 * 0.2 + qt.ts_vwap(5) * 0.8;
    const auto delta_price = delta(price);
    return -delta_price;
  };
  return rank(qts.apply(delta_p));
};


// SMA(((HIGH+LOW)/2-(DELAY(HIGH,1)+DELAY(LOW,1))/2)*(HIGH-LOW)/VOLUME,7,2)
Alpha_fun alpha009 = [](const Quote& qt) -> double {
  const auto muti_p1 = (qt.ts_high(7) + qt.ts_low(7)) / 2 -
                       (qt.ts_high(7, 1) + qt.ts_low(7, 1)) / 2;
  const auto muti_p2 = (qt.ts_high(7) - qt.ts_low(7)) / qt.ts_volume(7);
  return sma((muti_p1 * muti_p2), 2);
};


// RANK(MAX(((RET < 0) ? STD(RET, 20) : CLOSE)^2, 5))
Alpha_mfun alpha010 = [](const Quotes& qts) -> Timeseries {
  auto base_fun = [](const Quote& qt) {
    double comp;
    if (qt.ret() < 0) {
      comp = std::pow(stdev(qt.ts_ret(20)), 2.0);
    } else {
      comp =  std::pow(qt.close(), 2.0);
    }
    return std::max(comp, 5.0);
  };
  return rank(qts.apply(base_fun));
};


// SUM(((CLOSE-LOW)-(HIGH-CLOSE))./(HIGH-LOW).*VOLUME,6)
Alpha_fun alpha011 = [](const Quote& qt) -> double {
  const auto left =
    (qt.ts_close(6) - qt.ts_low(6)) -
    (qt.ts_high(6) - qt.ts_close(6));
  const auto right = qt.ts_high(6) - qt.ts_low(6);
  return sum(left / right * qt.ts_volume(6));
};


// (RANK((OPEN -(SUM(VWAP, 10) / 10)))) * (-1 * (RANK(ABS((CLOSE -VWAP)))))
Alpha_mfun alpha012 = [](const Quotes& qts) -> Timeseries {
  auto vwap_o = [](const Quote& qt) {
    return qt.open() - mean(qt.ts_vwap(10));
  };
  auto vwap_c = [](const Quote& qt) {
    return std::abs(qt.close() - qt.vwap());
  };
  return rank(qts.apply(vwap_o)) * (rank(qts.apply(vwap_c)) * -1);
};


// (((HIGH * LOW)^0.5) -VWAP)
Alpha_fun alpha013 = [](const Quote& qt) -> double {
  if (ISNA(qt.high()) || ISNA(qt.low()) || ISNA(qt.vwap()) ||
      qt.high() * qt.low() <= 0.0) {
    return NA_REAL;
  }
  return std::pow(qt.high() * qt.low(), 0.5) - qt.vwap();
};


// CLOSE-DELAY(CLOSE,5)
Alpha_fun alpha014 = [](const Quote& qt) -> double {
  return qt.close() - qt.close(5);
};


// OPEN/DELAY(CLOSE,1)-1
Alpha_fun alpha015 = [](const Quote& qt) -> double {
  if (ISNA(qt.close(1)) || qt.close(1) == 0.0) {
    return NA_REAL;
  }
  return qt.open() / qt.close(1) - 1.0;
};


// (-1 * TSMAX(RANK(CORR(RANK(VOLUME), RANK(VWAP), 5)), 5))
Alpha_mfun alpha016 = [](const Quotes& qts) -> Timeseries {
  auto rk_volume = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };
  auto rk_vwap = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.vwap(); }));
  };
  auto rk_corr = [rk_volume, rk_vwap](const Quotes& qts) {
    auto rk_volumes = qts.tsapply(5, rk_volume);
    auto rk_vwaps = qts.tsapply(5, rk_vwap);
    return apply(rk_volumes, rk_vwaps, corr);
  };
  auto rks = qts.tsapply(5, rk_corr);
  return apply(rks, tsmax) * -1.0;
};


// RANK((VWAP -MAX(VWAP, 15)))^DELTA(CLOSE, 5)
Alpha_mfun alpha017 = [](const Quotes& qts) -> Timeseries {
  auto vwap = [](const Quote& qt) {
    return qt.vwap() - std::max(qt.vwap(), 15.0);
  };
  auto rk_d_vwap = rank(qts.apply(vwap));
  auto close = [](const Quote& qt) {
    return qt.close() - qt.close(5);
  };
  auto d_close = qts.apply(close);
  return pow(rk_d_vwap, d_close);
};


// CLOSE/DELAY(CLOSE,5)
Alpha_fun alpha018 = [](const Quote& qt) -> double {
  if (ISNA(qt.close(5)) || qt.close(5) == 0.0) {
    return NA_REAL;
  }
  return qt.close() / qt.close(5);
};


// (CLOSE<DELAY(CLOSE,5)?
// (CLOSE-DELAY(CLOSE,5))/DELAY(CLOSE,5):(CLOSE=DELAY(CLOSE,5)?
// 0:(CLOSE-DELAY(CLOSE,5))/CLOSE))
Alpha_fun alpha019 = [](const Quote& qt) -> double {
  if (ISNA(qt.close()) || ISNA(qt.close(5)) || qt.close() == 0.0 || qt.close(5) == 0.0) {
    return NA_REAL;
  }
  if (qt.close() < qt.close(5)) {
    return (qt.close() - qt.close(5)) / qt.close(5);
  } else if (qt.close() == qt.close(5)) {
    return 0.0;
  } else {
    return (qt.close() - qt.close(5)) / qt.close();
  }
};


// (CLOSE-DELAY(CLOSE,6))/DELAY(CLOSE,6)*100
Alpha_fun alpha020 = [](const Quote& qt) -> double {
  if (ISNA(qt.close(6)) || qt.close(6) == 0.0) {
    return NA_REAL;
  }
  return (qt.close() - qt.close(6)) / qt.close(6) * 100;
};


// REGBETA(MEAN(CLOSE,6),SEQUENCE(6))
Alpha_fun alpha021 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    return mean(qt.ts_close(6));
  };
  Timeseries quote_mean = qt.ts<double>(6, base_fun);
  return regbeta(quote_mean, sequence(6));
};


// SMA(((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6)-
// DELAY((CLOSE-MEAN(CLOSE,6))/MEAN(CLOSE,6),3)),12,1)
Alpha_fun alpha022 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    double ret_p = qt.close() / mean(qt.ts_close(6)) - 1.0;
    double ret_d_p = qt.close(3) / mean(qt.ts_close(6, 3)) - 1.0;
    return ret_p - ret_d_p;
  };
  return sma(qt.ts<double>(12, base_fun), 1);
};


// SMA((CLOSE>DELAY(CLOSE,1)? STD(CLOSE,20):0),20,1)/
// (SMA((CLOSE>DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1)+
// SMA((CLOSE<=DELAY(CLOSE,1)?STD(CLOSE,20):0),20,1))*
// 100
Alpha_fun alpha023 = [](const Quote& qt) -> double {
  auto p_con1 = [](const Quote& qt) {
    if (qt.close() > qt.close(1)) {
      return stdev(qt.ts_close(20));
    } else {
      return 0.0;
    }
  };
  auto p_con2 = [](const Quote& qt) {
    if (qt.close() <= qt.close(1)) {
      return stdev(qt.ts_close(20));
    } else {
      return 0.0;
    }
  };
  double sma1 = sma(qt.ts<double>(20, p_con1), 1);
  double sma2 = sma(qt.ts<double>(20, p_con2), 1);
  if ((sma1 + sma2 == 0.0) || (ISNA(sma1 + sma2))) {
    return NA_REAL;
  }
  return sma1 / (sma1 + sma2) * 100;
};


// SMA(CLOSE-DELAY(CLOSE,5),5,1)
Alpha_fun alpha024 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    return qt.close() - qt.close(5);
  };
  return sma(qt.ts<double>(5, base_fun), 1);
};


// (-1 * RANK((DELTA(CLOSE, 7) *
// (1 -RANK(DECAYLINEAR((VOLUME / MEAN(VOLUME,20)), 9)))))) *
// (1 + RANK(SUM(RET, 250)))
Alpha_mfun alpha025 = [](const Quotes& qts) -> Timeseries {
  auto d_close = [](const Quote& qt) {
    return qt.close() - qt.close(7);
  };
  auto sum_ret = [](const Quote& qt) {
    return sum(qt.ts_ret(250));
  };
  auto volume = [](const Quote& qt) {
    return qt.volume() - mean(qt.ts_volume(20));
  };
  auto decay_linear = [volume](const Quote& qt) {
    return decaylinear(qt.ts<double>(9, volume));
  };
  Timeseries delta_close = qts.apply(d_close);
  Timeseries rk_sum_ret = rank(qts.apply(sum_ret));
  Timeseries rk_decay_linear = rank(qts.apply(decay_linear));
  return rank(delta_close * (rk_decay_linear * -1.0 + 1.0)) *
    (rk_sum_ret * -1.0 + 1.0) * -1.0;
};


// ((SUM(CLOSE, 7) / 7) -CLOSE) + CORR(VWAP, DELAY(CLOSE, 5), 230)
Alpha_fun alpha026 = [](const Quote& qt) -> double {
  double rk_close_diff = mean(qt.ts_close(7)) - qt.close();
  double rk_corr = corr(qt.ts_vwap(230), qt.ts_close(230, 5));
  return rk_close_diff + rk_corr;
};


// WMA((CLOSE-DELAY(CLOSE,3))/DELAY(CLOSE,3)*100+(CLOSE-DELAY(CLOSE,6))/DELAY(CLOSE,6)*100,12)
Alpha_fun alpha027 = [](const Quote& qt) -> double {
  auto fun = [](const Quote& qt) {
    double rk_close_ret_d3 =
      (qt.close() - qt.close(3)) /
        qt.close(3);
    double rk_close_ret_d6 =
      (qt.close() - qt.close(6)) /
        qt.close(6);
    return rk_close_ret_d3 * 100 + rk_close_ret_d6 * 100;
  };
  return wma(qt.ts<double>(12, fun));
};


// 3*SMA((CLOSE-TSMIN(LOW,9))/(TSMAX(HIGH,9)-TSMIN(LOW,9))*100,3,1)-
// 2*SMA(SMA((CLOSE-TSMIN(LOW,9))/(TSMAX(HIGH,9)-TSMAX(LOW,9))*100,3,1),3,1)
Alpha_fun alpha028 = [](const Quote& qt) -> double {
  auto ts_p = [](const Quote& qt) {
    double param1 = qt.close() - tsmin(qt.ts_low(9));
    double param2 = tsmax(qt.ts_high(9)) - tsmin(qt.ts_low(9));
    return param1 / param2 * 100;
  };
  auto p_sma = [](const Quote& qt) {
    double param1 = qt.close() - tsmin(qt.ts_low(9));
    double param2 = tsmax(qt.ts_high(9)) - tsmin(qt.ts_low(9));
    return param1 / param2 * 100;
  };
  auto ts_p_sma = [p_sma](const Quote& qt) {
    return sma(qt.ts<double>(3, p_sma), 1);
  };
  return sma(qt.ts<double>(3, ts_p), 1) * 3 - sma(qt.ts<double>(3, ts_p_sma), 1) * 2;
};


// (CLOSE-DELAY(CLOSE,6))/DELAY(CLOSE,6)*VOLUME
Alpha_fun alpha029 = [](const Quote& qt) -> double {
  if (ISNA(qt.close(6)) || qt.close(6) == 0.0) {
    return NA_REAL;
  }
  return (qt.close() - qt.close(6)) / qt.close(6) * qt.volume();
};


// WMA((REGRESI(CLOSE/DELAY(CLOSE)-1,MKT，60))^2,20)
Alpha_fun alpha030 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    Timeseries close_ret =
      qt.ts_close(60) / qt.ts_close(60, 1) - 1;
    Timeseries bmk_close_ret =
      qt.ts_bmk_close(60) / qt.ts_bmk_close(60, 1) - 1;
    return std::pow(regresi(close_ret, bmk_close_ret), 2.0);
  };
  return wma(qt.ts<double>(20, base_fun));
};


// (CLOSE-MEAN(CLOSE,12))/MEAN(CLOSE,12)*100
Alpha_fun alpha031 = [](const Quote& qt) -> double {
  if (ISNA(mean(qt.ts_close(12))) || mean(qt.ts_close(12)) == 0.0) {
    return NA_REAL;
  }
  return (qt.close() - mean(qt.ts_close(12))) /
    mean(qt.ts_close(12)) * 100;
};


// (-1 * SUM(RANK(CORR(RANK(HIGH), RANK(VOLUME), 3)), 3))
Alpha_mfun alpha032 = [](const Quotes& qts) -> Timeseries {
  auto rk_high = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {return qt.high();}));
  };
  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {return qt.volume();}));
  };

  auto rk_corr = [rk_high, rk_vol](const Quotes& qts) {
    auto rk1 = qts.tsapply(3, rk_high);
    auto rk2 = qts.tsapply(3, rk_vol);
    return rank(apply(rk1, rk2, corr));
  };

  auto rk = qts.tsapply(3, rk_corr);
  return apply(rk, sum) * -1.0;
};


// ((-1 * TSMIN(LOW, 5)) + DELAY(TSMIN(LOW, 5), 5)) *
// RANK((SUM(RET, 240) -SUM(RET, 20)) / 220) * TSRANK(VOLUME, 5)
Alpha_mfun alpha033 = [](const Quotes& qts) -> Timeseries {
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
  auto ts_rank_v = qts.apply(ts_rk_volume);
  auto rank_ret = rank(qts.apply(ret));
  auto ts_min_dl = qts.apply(ts_min_low);
  return ts_min_dl * rank_ret * ts_rank_v;
};


// MEAN(CLOSE,12)/CLOSE
Alpha_fun alpha034 = [](const Quote& qt) -> double {
  if (ISNA(qt.close()) || qt.close() == 0.0) {
    return NA_REAL;
  }
  return mean(qt.ts_close(12)) / qt.close();
};


// (MIN(RANK(DECAYLINEAR(DELTA(OPEN, 1), 15)),
// RANK(DECAYLINEAR(CORR((VOLUME),
// ((OPEN * 0.65) + (OPEN *0.35)), 17),7))) * -1)
Alpha_mfun alpha035 = [](const Quotes& qts) -> Timeseries {
  auto d_open = [](const Quote& qt) {
    return qt.open() - qt.open(1);
  };
  auto base_fun1 = [d_open](const Quote& qt) {
    return decaylinear(qt.ts<double>(15, d_open));
  };
  auto corr_open_volume = [](const Quote& qt) {
    return corr(qt.ts_volume(17), qt.ts_open(17));
  };
  auto base_fun2 = [corr_open_volume](const Quote& qt) {
    return decaylinear(qt.ts<double>(7, corr_open_volume));
  };
  auto rk_decay1 = rank(qts.apply(base_fun1));
  auto rk_decay2 = rank(qts.apply(base_fun2));

  return std::min(rk_decay1, rk_decay2) * -1.0;
};


// RANK(SUM(CORR(RANK(VOLUME), RANK(VWAP),6), 2))
Alpha_mfun alpha036 = [](const Quotes& qts) -> Timeseries {
  auto rk_volume = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {return qt.volume();}));
  };
  auto rk_vwap = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {return qt.vwap();}));
  };

  auto rk_corr = [rk_volume, rk_vwap](const Quotes& qts) {
    auto rk1 = qts.tsapply(6, rk_volume);
    auto rk2 = qts.tsapply(6, rk_vwap);
    return rank(apply(rk1, rk2, corr));
  };

  auto rk = qts.tsapply(2, rk_corr);
  return rank(apply(rk, sum));
};


// -1 * RANK((SUM(OPEN, 5) * SUM(RET, 5)) -DELAY((SUM(OPEN, 5) * SUM(RET, 5)), 10))
Alpha_mfun alpha037 = [](const Quotes& qts) -> Timeseries {
  auto sum_open_ret = [](const Quote& qt) {
    double open_ret = sum(qt.ts_open(5)) * sum(qt.ts_ret(5));
    double d_open_ret =
      sum(qt.ts_open(5)) * sum(qt.ts_ret(5)) -
      (sum(qt.ts_open(5, 10)) * sum(qt.ts_ret(5, 10)));
    return open_ret - d_open_ret;
  };
  return rank(qts.apply(sum_open_ret)) * -1.0;
};


// (((SUM(HIGH, 20) / 20) < HIGH) ? (-1 * DELTA(HIGH, 2)) : 0)
Alpha_fun alpha038 = [](const Quote& qt) -> double {
  if (mean(qt.ts_high(20)) < qt.high()) {
    return -delta(qt.ts_high(3));
  } else {
    return 0.0;
  }
};


// (RANK(DECAYLINEAR(DELTA((CLOSE), 2),8)) -
// RANK(DECAYLINEAR(CORR(((VWAP * 0.3) + (OPEN * 0.7)),
// SUM(MEAN(VOLUME,180), 37), 14), 12))) * -1
Alpha_mfun alpha039 = [](const Quotes& qts) -> Timeseries {
  auto d_close = [](const Quote& qt) {
    return qt.close() - qt.close(2);
  };
  auto decay_p = [d_close](const Quote& qt) {
    return decaylinear(qt.ts<double>(8, d_close));
  };

  auto vo = [](const Quote& qt) {
    return qt.vwap() * 0.3 + qt.open() * 0.7;
  };
  auto volume = [](const Quote& qt) {
    return mean((qt.ts_volume(180)));
  };
  auto sum_v = [volume](const Quote& qt) {
    return sum(qt.ts<double>(37, volume));
  };
  auto corr_p = [vo, sum_v](const Quote& qt) {
    auto ts_vo = qt.ts<double>(14, vo);
    auto ts_sum_v = qt.ts<double>(14, sum_v);
    return corr(ts_vo, ts_sum_v);
  };
  auto decay_v = [corr_p](const Quote& qt) {
    return decaylinear(qt.ts<double>(12, corr_p));
  };
  auto rk1 = rank(qts.apply(decay_p));
  auto rk2 = rank(qts.apply(decay_v));
  return (rk1 - rk2) * -1.0;
};


// SUM((CLOSE>DELAY(CLOSE,1)?VOLUME:0),26)/
// SUM((CLOSE<=DELAY(CLOSE,1)?VOLUME:0),26)*100
Alpha_fun alpha040 = [](const Quote& qt) -> double {
  auto base_fun1 = [](const Quote& qt) {
    if (qt.close() > qt.close(1)) {
      return qt.volume();
    } else {
      return 0.0;
    }
  };
  auto base_fun2 = [](const Quote& qt) {
    if (qt.close() <= qt.close(1)) {
      return qt.volume();
    } else {
      return 0.0;
    }
  };

  auto right = sum(qt.ts<double>(26, base_fun2));
  if (right == 0.0 || ISNA(right)) return NA_REAL;
  auto left = sum(qt.ts<double>(26, base_fun1));
  return left / right * 100;
};


// RANK(MAX(DELTA((VWAP), 3), 5))* -1
Alpha_mfun alpha041 = [](const Quotes& qts) -> Timeseries {
  auto max_d_vwap = [](const Quote& qt) {
    return std::max(delta(qt.ts_vwap(4)), 5.0);
  };
  return rank(qts.apply(max_d_vwap)) * -1.0;
};


// (-1 * RANK(STD(HIGH, 10))) * CORR(HIGH, VOLUME, 10)
Alpha_mfun alpha042 = [](const Quotes& qts) -> Timeseries {
  auto std_high = [](const Quote& qt) {
    return stdev(qt.ts_high(10));
  };
  auto corr_high_vol = [](const Quote& qt) {
    return corr(qt.ts_high(10), qt.ts_volume(10));
  };
  return (rank(qts.apply(std_high)) * qts.apply(corr_high_vol)) * -1.0;
};


// SUM((CLOSE>DELAY(CLOSE,1)?VOLUME:(CLOSE<DELAY(CLOSE,1)?-VOLUME:0)),6)
Alpha_fun alpha043 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    if (qt.close() > qt.close(1)) {
      return qt.volume();
    } else if (qt.close() < qt.close(1)) {
      return -qt.volume();
    } else {
      return 0.0;
    }
  };
  return sum(qt.ts<double>(6, base_fun));
};


// TSRANK(DECAYLINEAR(CORR((LOW), MEAN(VOLUME,10), 7), 6),4) +
// TSRANK(DECAYLINEAR(DELTA(VWAP, 3), 10), 15)
Alpha_fun alpha044 = [](const Quote& qt) -> double {
  auto volume = [](const Quote& qt) {
    return mean(qt.ts_volume(10));
  };
  auto muti_low_vol = [volume](const Quote& qt) {
    Timeseries ts_low = qt.ts_low(7);
    Timeseries ts_volume = qt.ts<double>(7, volume);
    return corr(ts_low, ts_volume);
  };
  auto decay_corr = [muti_low_vol](const Quote& qt) {
    return decaylinear(qt.ts<double>(6, muti_low_vol));
  };
  auto vwap = [](const Quote& qt) {
    return qt.vwap() - qt.vwap(3);
  };
  auto decay_delta = [vwap](const Quote& qt) {
    return decaylinear(qt.ts<double>(10, vwap));
  };

  return tsrank(qt.ts<double>(4, decay_corr)) + tsrank(qt.ts<double>(15, decay_delta));
};


// RANK(DELTA((CLOSE * 0.6) + (OPEN*0.4), 1)) * RANK(CORR(VWAP, MEAN(VOLUME,150), 15))
Alpha_mfun alpha045 = [](const Quotes& qts) -> Timeseries {
  auto d_close_open = [](const Quote& qt) {
    return qt.close() * 0.6 + qt.open() * 0.4 - (qt.close(1) * 0.6 + qt.open(1) * 0.4);
  };
  auto volume = [](const Quote& qt) {
    return mean(qt.ts_volume(150));
  };
  auto corr_vwap_vol = [volume](const Quote& qt) {
    return corr(qt.ts_vwap(15), qt.ts<double>(15, volume));
  };
  return rank(qts.apply(d_close_open)) * rank(qts.apply(corr_vwap_vol));
};


// (MEAN(CLOSE,3)+MEAN(CLOSE,6)+MEAN(CLOSE,12)+MEAN(CLOSE,24))/(4*CLOSE)
Alpha_fun alpha046 = [](const Quote& qt) -> double {
  double mean_p = mean(qt.ts_close(3)) + mean(qt.ts_close(6)) +
                  mean(qt.ts_close(12)) + mean(qt.ts_close(24));
  if (ISNA(qt.close()) || qt.close() == 0.0) {
    return NA_REAL;
  }
  return mean_p / (4 * qt.close());
};


// SMA((TSMAX(HIGH,6)-CLOSE)/(TSMAX(HIGH,6)-TSMIN(LOW,6))*100,9,1)
Alpha_fun alpha047 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    return (tsmax(qt.ts_high(6)) - qt.close()) /
           (tsmax(qt.ts_high(6)) - tsmin(qt.ts_low(6))) * 100;
  };
  return sma(qt.ts<double>(9, base_fun), 1);
};


// -1*(RANK((SIGN((CLOSE -DELAY(CLOSE,1))) +
// SIGN((DELAY(CLOSE, 1) -DELAY(CLOSE, 2)))) +
// SIGN((DELAY(CLOSE, 2) -DELAY(CLOSE, 3))))) *
// SUM(VOLUME, 5) / SUM(VOLUME, 20)
Alpha_mfun alpha048 = [](const Quotes& qts) -> Timeseries {
  auto sum_vol = [](const Quote& qt) {
    return sum(qt.ts_volume(5)) / sum(qt.ts_volume(20));
  };
  auto sign_p = [](const Quote& qt) {
    double sign1 = sign(qt.close() - qt.close(1));
    double sign2 = sign(qt.close(1) - qt.close(2));
    double sign3 = sign(qt.close(2) - qt.close(3));
    return (sign1 + sign2 + sign3);
  };
  return (rank(qts.apply(sign_p)) * -1.0) * qts.apply(sum_vol);
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
  if (ISNA(sum_ts1 + sum_ts2) || (sum_ts1 + sum_ts2) == 0.0) {
    return NA_REAL;
  }
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
  if (ISNA(sum_ts1 + sum_ts2) || (sum_ts1 + sum_ts2) == 0.0) {
    return NA_REAL;
  }
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
  if (ISNA(sum_ts1 + sum_ts2) || (sum_ts1 + sum_ts2) == 0.0) {
    return NA_REAL;
  }
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
  if (ISNA(sum_ts2) || sum_ts2 == 0.0) {
    return NA_REAL;
  }
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
// (ABS(HIGH-DELAY(CLOSE,1))>ABS(LOW-DELAY(CLOSE,1)) &
// ABS(HIGH-DELAY(CLOSE,1))>ABS(HIGH-DELAY(LOW,1))?
// ABS(HIGH-DELAY(CLOSE,1))+ABS(LOW-DELAY(CLOSE,1))/2+ABS(DELAY(CLOSE,1)-DELAY(OPEN,1))/4:
// (ABS(LOW-DELAY(CLOSE,1))>ABS(HIGH-DELAY(LOW,1)) &
// ABS(LOW-DELAY(CLOSE,1))>ABS(HIGH-DELAY(CLOSE,1))?
// ABS(LOW-DELAY(CLOSE,1))+ABS(HIGH-DELAY(CLOSE,1))/2+ABS(DELAY(CLOSE,1)-DELAY(OPEN,1))/4:
// ABS(HIGH-DELAY(LOW,1))+ABS(DELAY(CLOSE,1)-DELAY(OPEN,1))/4))*
// MAX(ABS(HIGH-DELAY(CLOSE,1)),ABS(LOW-DELAY(CLOSE,1))),20)
Alpha_fun alpha055 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    double param1;
    double param2;
    double param3;
    param1 = qt.close() - qt.close(1) + (qt.close() - qt.open()) / 2 +
             qt.close(1) - qt.open(1);
    param3 = std::max(std::abs(qt.high() - qt.close(1)),
                      std::abs(qt.low() - qt.close(1)));
    if ((std::abs(qt.high() - qt.close(1)) > std::abs(qt.low() - qt.close(1))) &&
        (std::abs(qt.high() - qt.close(1)) > std::abs(qt.high() - qt.low(1)))) {
      param2 = std::abs(qt.high() - qt.close(1)) + std::abs(qt.low() - qt.close(1)) / 2 +
               std::abs(qt.close(1) - qt.open(1)) / 4;
    } else if ((std::abs(qt.low() - qt.close(1)) > std::abs(qt.high() - qt.low(1))) &&
               (std::abs(qt.low() - qt.close(1)) > std::abs(qt.high() - qt.close(1)))) {
      param2 = std::abs(qt.low() - qt.close(1)) + std::abs(qt.high() - qt.close(1)) / 2 +
               std::abs(qt.close(1) - qt.open(1)) / 4;
    } else {
      param2 = std::abs(qt.high() - qt.low(1)) + std::abs(qt.close(1) - qt.open(1)) / 4;
    }
    return 16.0 * param1 / param2 * param3;
  };
  return sum(qt.ts<double>(20, base_fun));
};


// RANK(OPEN -TSMIN(OPEN, 12)) < RANK(RANK(CORR(SUM((HIGH+ LOW) / 2, 19), SUM(MEAN(VOLUME,40), 19), 13))^5)
Alpha_mfun alpha056 = [](const Quotes& qts) -> Timeseries {
  auto fun_left = [](const Quote& qt) {
    return qt.open() - tsmin(qt.ts_open(12));
  };
  auto hl_sum = [](const Quote& qt) {
    return sum((qt.ts_high(19) + qt.ts_low(19)) / 2);
  };
  auto vol = [](const Quote& qt) {
    return mean(qt.ts_volume(40));
  };
  auto vol_sum = [vol](const Quote& qt) {
    return sum(qt.ts<double>(19, vol));
  };
  auto fun_right = [hl_sum, vol_sum](const Quote& qt) {
    auto corr1 = qt.ts<double>(13, hl_sum);
    auto corr2 = qt.ts<double>(13, vol_sum);
    return corr(corr1, corr2);
  };
  auto rk_left = rank(qts.apply(fun_left));
  auto rk_right = rank(pow(rank(qts.apply(fun_right)), 5.0));
  return rk_left < rk_right;
};


// SMA((CLOSE-TSMIN(LOW,9))/(TSMAX(HIGH,9)-TSMIN(LOW,9))*100,3,1)
Alpha_fun alpha057 = [](const Quote& qt) -> double {
  auto muti_p = [](const Quote& qt) {
    double left = qt.close() - tsmin(qt.ts_low(9));
    double right = tsmax(qt.ts_high(9)) - tsmin(qt.ts_low(9));
    return left / right * 100;
  };
  return sma(qt.ts<double>(3, muti_p), 1);
};



// COUNT(CLOSE>DELAY(CLOSE,1),20)/20*100
Alpha_fun alpha058 = [](const Quote& qt) -> double {
  auto bool_fun = [](const Quote& qt) {
    return qt.close() > qt.close(1);
  };
  return count(qt.ts<bool>(20.0, bool_fun)) / 20.0 * 100.0;
};


// SUM((CLOSE=DELAY(CLOSE,1)?
// 0:CLOSE-(CLOSE>DELAY(CLOSE,1)?
// MIN(LOW,DELAY(CLOSE,1)):MAX(HIGH,DELAY(CLOSE,1)))),20)
Alpha_fun alpha059 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    if (qt.close() == qt.close(1)) {
      return 0.0;
    } else if (qt.close() > qt.close(1)) {
      return qt.close() - std::min(qt.low(), qt.close(1));
    } else {
      return qt.close() - std::max(qt.high(), qt.close(1));
    }
  };
  return sum(qt.ts<double>(20, base_fun));
};


// SUM(((CLOSE-LOW)-(HIGH-CLOSE))./(HIGH-LOW).*VOLUME,20)
Alpha_fun alpha060 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    double left = (qt.close() - qt.low()) - (qt.high() - qt.close());
    double right = (qt.high() - qt.low()) / qt.volume();
    return left / right;
  };
  return sum(qt.ts<double>(20, base_fun));
};


}
