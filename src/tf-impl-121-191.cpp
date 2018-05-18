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

  auto fun_exp = [](const Quote& qt) {
    auto tsrank_cor = [](const Quote& qt) {
      auto cor_x = [](const Quote& qt) {
        return tsrank(qt.ts_vwap(20));
      };
      auto cor_y = [](const Quote& qt) {
        auto mean_vol_60 = [] (const Quote& qt) {
          return mean(qt.ts_volume(60));
        };
        return tsrank(qt.ts<double>(2, mean_vol_60));
      };
      return corr(qt.ts<double>(18, cor_x), qt.ts<double>(18, cor_y));
    };
    return tsrank(qt.ts<double>(3, tsrank_cor));
  };
  auto exp = qts.apply(fun_exp);
  return pow(base, exp) * -1.0;
};

Alpha_fun alpha149 = [](const Quote& quote) -> double {
  const auto dr = quote.ts_close(252) / quote.ts_close(252, 1) - 1.0;
  const auto bmk_dr = quote.ts_bmk_close(252) / quote.ts_bmk_close(252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  if (x.size() < 2) return NA_REAL;
  return regbeta(x, y);
};


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


// CORR(RANK(((CLOSE -TSMIN(LOW, 12)) / (TSMAX(HIGH, 12) -TSMIN(LOW,12)))), RANK(VOLUME), 6)
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
  if (ISNA(qt.close(1))) {
    return NA_REAL;
  }
  return (qt.close() - qt.close(1)) / qt.close(1) * qt.volume();
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
// ((-1 * TSRANK(ABS(DELTA(CLOSE, 7)), 60)) * SIGN(DELTA(CLOSE, 7)) : (-1 *  VOLUME))
Alpha_fun Alpha180 = [](const Quote& qt) -> double {
  auto abs_fun = [](const Quote& qt) {
    return std::abs(qt.close() - qt.close(7));
  };

  if (mean(qt.ts_volume(20)) < qt.volume()) {
    return tsrank(qt.ts<double>(60, abs_fun)) * sign(qt.close() - qt.close(7)) * -1.0;
  } else {
    return qt.volume() * -1.0;
  }
};


//






}
