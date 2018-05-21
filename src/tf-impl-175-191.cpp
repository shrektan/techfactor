#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

// MEAN(MAX(MAX((HIGH-LOW),ABS(DELAY(CLOSE,1)-HIGH)),ABS(DELAY(CLOSE,1)-LOW)),6)
Alpha_fun alpha175 = [](const Quote& qt) -> double {
  auto max_fun = [](const Quote& qt) {
    auto left = std::max(qt.high() - qt.low(), std::abs(qt.close(1) - qt.high()));
    auto right = std::abs(qt.close(1) - qt.low());
    return std::max(left, right);
  };
  auto ts = qt.ts<double>(6, max_fun);
  return mean(ts);
};


// CORR(RANK(((CLOSE -TSMIN(LOW, 12)) /
// (TSMAX(HIGH, 12) -TSMIN(LOW,12)))), RANK(VOLUME), 6)
Alpha_mfun alpha176 = [](const Quotes& qts) -> Timeseries {
  auto muti_p = [](const Quote& qt) {
    double left = qt.close() - tsmin(qt.ts_low(12));
    double right = tsmax(qt.ts_high(12)) - tsmin(qt.ts_low(12));
    return left / right;
  };
  auto rk_left = [muti_p](const Quotes& qts) {
    return rank(qts.apply(muti_p));
  };
  auto rk_right = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {return qt.volume();}));
  };
  auto rk1 = qts.tsapply(6, rk_left);
  auto rk2 = qts.tsapply(6, rk_right);
  return apply(rk1, rk2, corr);
};


// ((20-HIGHDAY(HIGH,20))/20)*100
Alpha_fun alpha177 = [](const Quote& qt) -> double {
  return (20.0 - highday(qt.ts_high(20))) / 20.0 * 100.0;
};


// (CLOSE-DELAY(CLOSE,1))/DELAY(CLOSE,1)*VOLUME
Alpha_fun alpha178 = [](const Quote& qt) -> double {
  auto close1 = qt.close(1);
  if (ISNAN(close1)) return NA_REAL;
  return (qt.close() - close1) / close1 * qt.volume();
};


// RANK(CORR(VWAP, VOLUME, 4)) *
// RANK(CORR(RANK(LOW), RANK(MEAN(VOLUME,50)), 12))
Alpha_mfun alpha179 = [](const Quotes& qts) -> Timeseries {
  auto left_fun = [](const Quote& qt) {
    return corr(qt.ts_volume(4), qt.ts_vwap(4));
  };
  auto left = rank(qts.apply(left_fun));

  auto rk_left = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {return qt.low();}));
  };
  auto m_vol = [](const Quote& qt) {
    return mean(qt.ts_volume(50));
  };
  auto rk_right = [m_vol](const Quotes& qts) {
    return rank(qts.apply(m_vol));
  };
  auto rk1 = qts.tsapply(12, rk_left);
  auto rk2 = qts.tsapply(12, rk_right);
  auto right = rank(apply(rk1, rk2, corr));

  return left * right;
};


// (MEAN(VOLUME,20) < VOLUME) ?
// ((-1 * TSRANK(ABS(DELTA(CLOSE, 7)), 60)) *
// SIGN(DELTA(CLOSE, 7)) : (-1 *  VOLUME))
Alpha_fun alpha180 = [](const Quote& qt) -> double {
  auto abs_fun = [](const Quote& qt) {
    return std::abs(qt.close() - qt.close(7));
  };
  if (mean(qt.ts_volume(20)) < qt.volume()) {
    return tsrank(qt.ts<double>(60, abs_fun)) *
      sign(qt.close() - qt.close(7)) *
      -1.0;
  } else {
    return qt.volume() * -1.0;
  }
};


// SUM(((CLOSE/DELAY(CLOSE,1)-1)-MEAN((CLOSE/DELAY(CLOSE,1)-1),20))-
// (BANCHMARKINDEXCLOSE-MEAN(BANCHMARKINDEXCLOSE,20))^2,20)/
// SUM((BANCHMARKINDEXCLOSE-MEAN(BANCHMARKINDEXCLOSE,20))^3, 20)
Alpha_fun alpha181 = [](const Quote& qt) -> double {
  auto d_close = [](const Quote& qt) {
    return qt.close() / qt.close(1) - 1.0;
  };
  auto muti_p1 = [d_close](const Quote& qt) {
    double stk_p = qt.close() / qt.close(1) - 1.0 - mean(qt.ts<double>(20, d_close));
    double bmk_p = std::pow(qt.bmk_close() - mean(qt.ts_bmk_close(20)), 2.0);
    return stk_p - bmk_p;
  };
  auto sum_left = sum(qt.ts<double>(20, muti_p1));

  auto muti_p2 = [](const Quote& qt) {
    return std::pow(qt.bmk_close() - mean(qt.ts_bmk_close(20)), 3.0);
  };
  auto sum_right = sum(qt.ts<double>(20, muti_p2));
  if (ISNAN(sum_right) || near(sum_right, 0.0)) {
    return NA_REAL;
  }
  return sum_left / sum_right;
};


// COUNT((CLOSE>OPEN & BANCHMARKINDEXCLOSE>BANCHMARKINDEXOPEN) OR
// (CLOSE<OPEN & BANCHMARKINDEXCLOSE<BANCHMARKINDEXOPEN),20)/20
Alpha_fun alpha182 = [](const Quote& qt) -> double {
  auto base_fun = [] (const Quote& qt) {
    bool left = (qt.close() > qt.open()) && (qt.bmk_close() > qt.bmk_open());
    bool right = (qt.close() < qt.open()) && (qt.bmk_close() < qt.bmk_open());
    return left || right;
  };
  return count(qt.ts<bool>(20, base_fun)) / 20.0;
};


// MAX(SUMAC(CLOSE-MEAN(CLOSE,24)))-MIN(SUMAC(CLOSE-MEAN(CLOSE,24)))/STD(CLOSE,24)
Alpha_fun alpha183 = [](const Quote& qt) -> double {
  auto m_close = [](const Quote& qt) {
    return mean(qt.ts_close(24));
  };
  double left = tsmax(sumac(qt.ts_close(24) - qt.ts<double>(24, m_close)));
  auto std_c24 = stdev(qt.ts_close(24));
  if (ISNAN(std_c24) || near(std_c24, 0.0)) return NA_REAL;
  double right =
    tsmin(sumac(qt.ts_close(24) - qt.ts<double>(24, m_close))) / std_c24;
  return left - right;
};


// RANK(CORR(DELAY((OPEN -CLOSE), 1), CLOSE, 200)) + RANK((OPEN -CLOSE))
Alpha_mfun alpha184 = [](const Quotes& qts) -> Timeseries {
  auto corr_fun = [](const Quote& qt) {
    auto left = qt.ts_open(200, 1) - qt.ts_close(200, 1);
    auto right = qt.ts_close(200);
    return corr(left, right);
  };
  auto p_fun = [](const Quote& qt) {
    return qt.open() - qt.close();
  };
  return rank(qts.apply(corr_fun)) + rank(qts.apply(p_fun));
};


// RANK((-1 * ((1 -(OPEN / CLOSE))^2)))
Alpha_mfun alpha185 = [](const Quotes& qts) -> Timeseries {
  auto base_fun = [](const Quote& qt) {
    return std::pow(1 - qt.open() / qt.close(), 2.0) * -1.0;
  };
  return rank(qts.apply(base_fun));
};


// (MEAN(ABS(SUM((LD>0 & LD>HD)?
// LD:0,14)*100/SUM(TR,14)-SUM((HD>0 & HD>LD)?
// HD:0,14)*100/SUM(TR,14))/(SUM((LD>0 & LD>HD)?
// LD:0,14)*100/SUM(TR,14)+SUM((HD>0 & HD>LD)?
// HD:0,14)*100/SUM(TR,14))*100,6)+DELAY(MEAN(ABS(SUM((LD>0 & LD>HD)?
// LD:0,14)*100/SUM(TR,14)-SUM((HD>0 & HD>LD)?
// HD:0,14)*100/SUM(TR,14))/(SUM((LD>0 & LD>HD)?
// LD:0,14)*100/SUM(TR,14)+SUM((HD>0 & HD>LD)?
// HD:0,14)*100/SUM(TR,14))*100,6),6))/2
Alpha_fun alpha186 = [](const Quote& qt) -> double {
  auto choose_fun1 = [](const Quote& qt) {
    return (qt.ld() > 0 && qt.ld() > qt.hd()) ? qt.ld() : 0.0;
  };
  auto choose_fun2 = [](const Quote& qt) {
    return (qt.hd() > 0 && qt.hd() > qt.ld()) ? qt.hd() : 0.0;
  };
  auto abs_sum1 = [choose_fun1, choose_fun2](const Quote& qt) {
    double param1 = sum(qt.ts<double>(14, choose_fun1));
    double param2 = sum(qt.ts_tr(14));
    double param3 = sum(qt.ts<double>(14, choose_fun2));
    double abs1 = std::abs(param1 * 100 / param2 - param3 * 100 / param2);
    double sum1 = param1 * 100 / param2 + param3 * 100 / param2;
    if (near(sum1, 0.0)) return NA_REAL;
    return abs1 / sum1 * 100;
  };
  auto left =  mean(qt.ts<double>(6, abs_sum1));

  auto delay_choose_fun1 = [](const Quote& qt) {
    return (qt.ld(6) > 0 && qt.ld(6) > qt.hd(6)) ? qt.ld(6) : 0.0;
  };
  auto delay_choose_fun2 = [](const Quote& qt) {
    return (qt.hd(6) > 0 && qt.hd(6) > qt.ld(6)) ? qt.hd(6) : 0.0;
  };
  auto abs_sum2 = [delay_choose_fun1, delay_choose_fun2](const Quote& qt) {
    double param1 = sum(qt.ts<double>(14, delay_choose_fun1));
    double param2 = sum(qt.ts_tr(14, 6));
    double param3 = sum(qt.ts<double>(14, delay_choose_fun2));
    double abs1 = std::abs(param1 * 100 / param2 - param3 * 100 / param2);
    double sum1 = param1 * 100 / param2 + param3 * 100 / param2;
    if (near(sum1, 0.0)) return NA_REAL;
    return abs1 / sum1 * 100;
  };
  auto right =  mean(qt.ts<double>(6, abs_sum2));

  return (left + right) / 2.0;
};


// SUM((OPEN<=DELAY(OPEN,1)?0:MAX((HIGH-OPEN),(OPEN-DELAY(OPEN,1)))),20)
Alpha_fun alpha187 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    if (qt.open() <= qt.open(1)) return 0.0;
    return std::max(qt.high() - qt.open(), qt.open() - qt.open(1));
  };
  return sum(qt.ts<double>(20, base_fun));
};


// ((HIGH-LOW–SMA(HIGH-LOW,11,2))/SMA(HIGH-LOW,11,2))*100
Alpha_fun alpha188 = [](const Quote& qt) -> double {
  auto left = qt.high() - qt.low() - sma(qt.ts_high(11) - qt.ts_low(11), 2);
  auto right = sma(qt.ts_high(11) - qt.ts_low(11), 2);
  if (ISNAN(right) || near(right, 0.0)) return NA_REAL;
  return left / right * 100;
};


// MEAN(ABS(CLOSE-MEAN(CLOSE,6)),6)
Alpha_fun alpha189 = [](const Quote& qt) -> double {
  auto base_fun = [](const Quote& qt) {
    return std::abs(qt.close() - mean(qt.ts_close(6)));
  };
  return mean(qt.ts<double>(6, base_fun));
};


// LOG((COUNT(CLOSE/DELAY(CLOSE)-1>
// ((CLOSE/DELAY(CLOSE,19))^(1/20)-1),20)-1)*
// (SUMIF(((CLOSE/DELAY(CLOSE)-1-
// (CLOSE/DELAY(CLOSE,19))^(1/20)-1))^2,20,CLOSE/DELAY(CLOSE)-1<
// (CLOSE/DELAY(CLOSE,19))^(1/20)-1)) /
// ((COUNT((CLOSE/DELAY(CLOSE)-1<(CLOSE/DELAY(CLOSE,19))^(1/20)-1),20))*
// (SUMIF((CLOSE/DELAY(CLOSE)-1-
// ((CLOSE/DELAY(CLOSE,19))^(1/20)-1))^2,20,CLOSE/DELAY(CLOSE)-1>
// (CLOSE/DELAY(CLOSE,19))^(1/20)-1))))
Alpha_fun alpha190 = [](const Quote& qt) -> double {
  auto count_fun1 = [] (const Quote& qt) {
    auto left = qt.close() / qt.close(1) - 1.0;
    auto right = std::pow(qt.close() / qt.close(19), 1/20) - 1.0;
    return left > right;
  };
  double param1 = count(qt.ts<bool>(20, count_fun1)) - 1.0;

  auto double_muti_p = [](const Quote& qt) {
    auto param1 = qt.close() / qt.close(1) - 1.0;
    auto param2 = std::pow(qt.close() - qt.close(19), 1/20) - 1.0;
    return std::pow(param1 - param2, 2.0);
  };
  auto bool_muti_p1 = [](const Quote& qt) {
    auto left = qt.close() / qt.close(1) - 1.0;
    auto right = std::pow(qt.close() / qt.close(19), 1/20) - 1.0;
    return left < right;
  };
  auto bool_muti_p2 = [](const Quote& qt) {
    auto left = qt.close() / qt.close(1) - 1.0;
    auto right = std::pow(qt.close() / qt.close(19), 1/20) - 1.0;
    return left > right;
  };
  double param2 = sum(filter(
    qt.ts<double>(20, double_muti_p),
    qt.ts<bool>(20, bool_muti_p1)
  ));

  auto count_fun2 = [] (const Quote& qt) {
    auto left = qt.close() / qt.close(1) - 1.0;
    auto right = pow(qt.close() / qt.close(19), 1/20) - 1.0;
    return left < right;
  };
  double param3 = count(qt.ts<bool>(20, count_fun2));
  double param4 = sum(filter(
    qt.ts<double>(20, double_muti_p),
    qt.ts<bool>(20, bool_muti_p2)
  ));

  if ((near(param1 * param2, 0.0)) || (near(param3 * param4, 0.0))) {
    return NA_REAL;
  }
  return std::log(param1 * param2 / (param3 * param4));
};


// CORR(MEAN(VOLUME,20), LOW, 5) + ((HIGH + LOW) / 2) - CLOSE
Alpha_fun alpha191 = [](const Quote& qt) -> double {
  auto m_vol = [](const Quote& qt) {
    return mean(qt.ts_volume(20));
  };
  return corr(qt.ts<double>(5, m_vol), qt.ts_low(5)) +
    (qt.high() + qt.low()) / 2 -
    qt.close();
};



}
