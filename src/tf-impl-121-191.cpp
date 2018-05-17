#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
// ((RANK((VWAP -MIN(VWAP, 12)))^
// TSRANK(CORR(TSRANK(VWAP, 20), TSRANK(MEAN(VOLUME,60), 2), 18), 3)) * -1)
Alpha_mfun alpha121 = [](const Quotes& qts) -> Timeseries {
  auto fun_base = [](const Quote& qt) {
    return qt.vwap() - tsmin(qt.ts_vwap(12));
  };
  auto base = rank(qts.apply(fun_base));

  auto cor_x = [](const Quote& qt) {
    return tsrank(qt.ts_vwap(20));
  };
  auto mean_vol_60 = [] (const Quote& qt) {
    return mean(qt.ts_volume(60));
  };
  auto cor_y = [mean_vol_60](const Quote& qt) {
    return tsrank(qt.ts<double>(2, mean_vol_60));
  };
  auto tsrank_cor = [cor_x, cor_y](const Quote& qt) {
    return corr(qt.ts<double>(18, cor_x), qt.ts<double>(18, cor_y));
  };
  auto fun_exp = [tsrank_cor](const Quote& qt) {
    return tsrank(qt.ts<double>(3, tsrank_cor));
  };
  auto exp = qts.apply(fun_exp);
  return pow(base, exp) * -1.0;
};


// (SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2)-DELAY(SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2),1))/
// DELAY(SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2),1)
Alpha_fun alpha122 = [](const Quote& qt) -> double {
  auto sma_1 = [](const Quote& qt) {
    return sma(qt.ts_close(13), 2);
  };
  auto sma_2 = [sma_1](const Quote& qt) {
    return sma(qt.ts<double>(13, sma_1), 2);
  };
  auto sma_3 = [sma_2](const Quote& qt) {
    return sma(qt.ts<double>(13, sma_2), 2);
  };
  auto tripple_sma = [sma_3, &qt](const int delay) {
    return sma_3(qt.clock_back(delay));
  };
  const double second = tripple_sma(1);
  if (second == 0) return NA_REAL;
  const double first = tripple_sma(0);
  return first / second - 1.0;
};


// ((RANK(CORR(SUM(((HIGH + LOW) / 2), 20), SUM(MEAN(VOLUME,60), 20), 9)) <
// RANK(CORR(LOW, VOLUME, 6))) * -1)
Alpha_mfun alpha123 = [](const Quotes& qts) -> Timeseries {
  auto sum_hl20 = [](const Quote& qt) {
    return sum((qt.ts_high(20) + qt.ts_low(20)) / 2.0);
  };
  auto mean_vol60 = [](const Quote& qt) {
    return mean(qt.ts_volume(60));
  };
  auto sum_mv60_20 = [mean_vol60](const Quote& qt) {
    return sum(qt.ts<double>(20, mean_vol60));
  };
  auto corr9 = [sum_hl20, sum_mv60_20](const Quote& qt) {
    auto v1 = qt.ts<double>(9, sum_hl20);
    auto v2 = qt.ts<double>(9, sum_mv60_20);
    return corr(v1, v2);
  };
  auto left = rank(qts.apply(corr9));

  auto corr6 = [](const Quote& qt) {
    return corr(qt.ts_low(6), qt.ts_volume(6));
  };
  auto right = rank(qts.apply(corr6));
  return (left < right) * -1.0;
};


// (CLOSE -VWAP) / DECAYLINEAR(RANK(TSMAX(CLOSE, 30)),2)
Alpha_mfun alpha124 = [](const Quotes& qts) -> Timeseries {
  auto close_vwap = [](const Quote& qt) {
    return qt.close() - qt.vwap();
  };
  auto left = qts.apply(close_vwap);

  auto tsmax_close_30 = [](const Quote& qt) {
    return tsmax(qt.ts_close(30));
  };
  auto rk_30 = [tsmax_close_30](const Quotes& qts) {
    return rank(qts.apply(tsmax_close_30));
  };

  auto rk = qts.tsapply(2, rk_30);
  Timeseries right = apply(rk, decaylinear);
  return left / right;
};


// (RANK(DECAYLINEAR(CORR((VWAP), MEAN(VOLUME,80),17), 20)) /
// RANK(DECAYLINEAR(DELTA(((CLOSE * 0.5) + (VWAP * 0.5)), 3), 16)))
Alpha_mfun alpha125 = [](const Quotes& qts) -> Timeseries {
  auto mean_vol_80 = [](const Quote& qt) {
    return mean(qt.ts_volume(80));
  };
  auto corr_17 = [mean_vol_80](const Quote& qt) {
    auto vwap = qt.ts_vwap(17);
    auto mvol80 = qt.ts<double>(17, mean_vol_80);
    return corr(vwap, mvol80);
  };
  auto decay_20 = [corr_17](const Quote& qt) {
    return decaylinear(qt.ts<double>(20, corr_17));
  };
  auto left = rank(qts.apply(decay_20));

  auto close_vwap = [](const Quote& qt) {
    return qt.close() * 0.5 + qt.vwap() * 0.5;
  };
  auto delta_3 = [close_vwap](const Quote& qt) {
    return delta(qt.ts<double>(3, close_vwap));
  };
};

Alpha_fun alpha149 = [](const Quote& qt) -> double {
  const auto dr = qt.ts_close(252) / qt.ts_close(252, 1) - 1.0;
  const auto bmk_dr = qt.ts_bmk_close(252) / qt.ts_bmk_close(252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  if (x.size() < 2) return NA_REAL;
  return regbeta(x, y);
};

}
