#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
// ((RANK((VWAP -TSMIN(VWAP, 12)))^
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


// (SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2)-
// DELAY(SMA(SMA(SMA(LOG(CLOSE),13,2),13,2),13,2),1))/
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
    return delta(qt.ts<double>(4, close_vwap));
  };
  auto decay_16 = [delta_3](const Quote& qt) {
    return decaylinear(qt.ts<double>(17, delta_3));
  };
  auto right = rank(qts.apply(decay_16));
  return left / right;
};


// (CLOSE+HIGH+LOW)/3
Alpha_fun alpha126 = [](const Quote& qt) -> double {
  return (qt.close() + qt.high() + qt.low()) / 3.0;
};


// (MEAN((100*(CLOSE-TSMAX(CLOSE,12)) /(TSMAX(CLOSE,12)))^2, 12))^(1/2)
Alpha_fun alpha127 = [](const Quote& qt) -> double {
  auto max_close_12 = tsmax(qt.ts_close(12));
  auto close_diff = (qt.ts_close(12) - max_close_12) * 100.0;
  auto mp = mean(pow(close_diff / max_close_12, 2.0));
  if (!R_FINITE(mp)) return NA_REAL;
  return std::pow(mp, 0.5);
};


// 100-(100/(1+ SUM( ((HIGH+LOW+CLOSE)/3>DELAY((HIGH+LOW+CLOSE)/3,1)?
// (HIGH+LOW+CLOSE)/3*VOLUME:0)  ,14) /SUM(((HIGH+LOW+CLOSE)/3<
// DELAY((HIGH+LOW+CLOSE)/3,1)?(HIGH+LOW+CLOSE)/3*VOLUME:0),14)))
Alpha_fun alpha128 = [](const Quote& qt) -> double {
  auto fun = [](const bool larger) {
    return [larger](const Quote& qt) {
      auto avg_hlc = (qt.high() + qt.low() + qt.close()) / 3.0;
      auto vol = qt.volume();
      auto avg_hlc_1 = (qt.high(1) + qt.low(1) + qt.close(1)) / 3.0;
      if (larger) {
        return (avg_hlc > avg_hlc_1) ? avg_hlc * vol : 0.0;
      } else {
        return (avg_hlc < avg_hlc_1) ? avg_hlc * vol : 0.0;
      }
    };
  };
  auto right = sum(qt.ts<double>(14, fun(false)));
  auto left = sum(qt.ts<double>(14, fun(true)));
  if (right == 0.0) return (left == 0.0) ? NA_REAL : 100.0;
  return 100.0 - 100.0 / (left / right + 1.0);
};


// SUM((CLOSE-DELAY(CLOSE,1)<0?ABS(CLOSE-DELAY(CLOSE,1)):0),12)
Alpha_fun alpha129 = [](const Quote& qt) -> double {
  auto fun = [](const Quote& qt) {
    auto close_diff = qt.close() - qt.close(1);
    return close_diff < 0 ? std::abs(close_diff) : 0.0;
  };
  return sum(qt.ts<double>(12, fun));
};


// (RANK(DECAYLINEAR(CORR(((HIGH + LOW) / 2), MEAN(VOLUME,40), 9), 10)) /
// RANK(DECAYLINEAR(CORR(RANK(VWAP), RANK(VOLUME), 7),3)))
Alpha_mfun alpha130 = [](const Quotes& qts) -> Timeseries {
  auto hl = [](const Quote& qt) {
    return (qt.high() + qt.low()) / 2.0;
  };
  auto mvol = [](const Quote& qt) {
    return mean(qt.ts_volume(40));
  };
  auto cor_hl_mvol = [hl, mvol](const Quote& qt) {
    return corr(qt.ts<double>(9, hl), qt.ts<double>(9, mvol));
  };
  auto dec_cor_hm = [cor_hl_mvol](const Quote& qt) {
    return decaylinear(qt.ts<double>(10, cor_hl_mvol));
  };
  auto left = qts.apply(dec_cor_hm);

  auto rk_vwap = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.vwap(); }));
  };
  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };
  auto cor_rk = [rk_vwap, rk_vol](const Quotes& qts) {
    return apply(qts.tsapply(7, rk_vwap), qts.tsapply(7, rk_vol), corr);
  };
  auto right = rank(apply(qts.tsapply(3, cor_rk), decaylinear));

  return left / right;
};


// (RANK(DELAT(VWAP, 1))^TSRANK(CORR(CLOSE,MEAN(VOLUME,50), 18), 18))
Alpha_mfun alpha131 = [](const Quotes& qts) -> Timeseries {
  auto d_vwap = [](const Quote& qt) {
    return delta(qt.ts_vwap(2));
  };
  auto rk_d_vwap = qts.apply(d_vwap);

  auto mvol50 = [](const Quote& qt) {
    return mean(qt.ts_volume(50));
  };
  auto cor_18 = [mvol50](const Quote& qt) {
    return corr(qt.ts_close(18), qt.ts<double>(18, mvol50));
  };
  auto tsrk18 = [cor_18](const Quote& qt) {
    return tsrank(qt.ts<double>(18, cor_18));
  };

  return pow(rk_d_vwap, qts.apply(tsrk18));
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
