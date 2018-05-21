#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
// MAX(RANK(DECAYLINEAR(DELTA(VWAP, 1), 12)),
// RANK(DECAYLINEAR(RANK(CORR((LOW), MEAN(VOLUME,80), 8)), 17))) * -1
Alpha_mfun alpha061 = [](const Quotes& qts) -> Timeseries {
  auto decay_linear1 = [](const Quote& qt) {
    return decaylinear(
      qt.ts<double>(12, [](const Quote& qt) { return delta(qt.ts_vwap(2)); })
    );
  };

  auto cor = [](const Quote& qt) {
    return corr(
      qt.ts<double>(8, [](const Quote& qt) { return mean(qt.ts_volume(80)); }),
      qt.ts_low(8)
    );
  };

  auto rk = [cor](const Quotes& qts) {
    return rank(qts.apply(cor));
  };

  auto decay_linear2 = [rk](const Quotes& qts) {
    auto tmp = qts.tsapply(17, rk);
    return apply(tmp, decaylinear);
  };
  Timeseries part1 = qts.apply(decay_linear1);
  Timeseries part2 = decay_linear2(qts);
  return pmax(rank(part1), rank(part2));
};


// -1 * CORR(HIGH, RANK(VOLUME), 5)
Alpha_mfun alpha062 = [](const Quotes& qts) -> Timeseries {
  auto rank_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };
  auto high_ser = [](const Quotes& qts) {
    return qts.apply([](const Quote& qt) { return qt.high(); });
  };
  auto rk1 = qts.tsapply(5, rank_vol);
  auto rk2 = qts.tsapply(5, high_ser);
  return apply(rk1, rk2, corr) * -1.0;
};


// SMA(MAX(CLOSE - DELAY(CLOSE, 1), 0), 6, 1) / SMA(ABS(CLOSE - DELAY(CLOSE, 1)), 6, 1) * 100
Alpha_fun alpha063 = [](const Quote& qt) -> double {
  auto get_hinge_diff = [](const Quote& qt) {
    return std::max(qt.close() - qt.pclose(), 0.0);
  };
  auto get_abs_diff   = [](const Quote& qt) {
    return std::abs(qt.close() - qt.pclose());
  };
  const auto sma_hinge = sma(qt.ts<double>(6, get_hinge_diff), 1);
  const auto sma_abs   = sma(qt.ts<double>(6, get_abs_diff), 1);
  if (sma_abs == 0.0) return NA_REAL;
  return sma_hinge / sma_abs * 100;
};


// MAX(RANK(DECAYLINEAR(CORR(RANK(VWAP), RANK(VOLUME), 4), 4)),
//     RANK(DECAYLINEAR(MAX(CORR(RANK(VWAP), RANK(MEAN(VOLUME, 60)), 4), 13), 14))) * -1
Alpha_mfun alpha064 = [](const Quotes& qts) -> Timeseries {
  auto rk_vwap = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.vwap(); }));
  };

  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };

  auto rk_close = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.close(); }));
  };

  auto rk_mean_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return mean(qt.ts_volume(60)); }));
  };

  auto decay_linear1 = [rk_vwap, rk_vol](const Quotes& qts) {
    auto rk1 = qts.tsapply(4, rk_vwap);
    auto rk2 = qts.tsapply(4, rk_vol);
    return apply(rk1, rk2, corr);
  };

  auto cor = [rk_close, rk_mean_vol](const Quotes& qts) {
    auto rk1 = qts.tsapply(4, rk_close);
    auto rk2 = qts.tsapply(4, rk_mean_vol);
    return apply(rk1, rk2, corr);
  };

  auto decay_linear2 = [cor](const Quotes& qts) {
    auto corr1 = qts.tsapply(13, cor);
    auto ma1 = apply(corr1, tsmax);
    return ma1;
  };

  auto tmp1 = qts.tsapply(4, decay_linear1);
  Timeseries part1 = apply(tmp1, decaylinear);
  auto tmp2 = qts.tsapply(14, decay_linear2);
  Timeseries part2 = apply(tmp2, decaylinear);
  return pmax(rank(part1), rank(part2));
};

// MEAN(CLOSE, 6) / CLOSE
Alpha_fun alpha065 = [](const Quote& qt) -> double {
  return mean(qt.ts_close(6)) / qt.close();
};


// (CLOSE - MEAN(CLOSE, 6)) / MEAN(CLOSE, 6) * 100
Alpha_fun alpha066 = [](const Quote& qt) -> double {
  const auto mean_close_6 = mean(qt.ts_close(6));
  return (qt.close() - mean_close_6) / mean_close_6 * 100;
};


// See also alpha63
Alpha_fun alpha067 = [](const Quote& qt) -> double {
  auto get_hinge_diff = [](const Quote& qt) {
    return std::max(qt.close() - qt.pclose(), 0.0);
  };
  auto get_abs_diff   = [](const Quote& qt) {
    return std::abs(qt.close() - qt.pclose());
  };
  const auto sma_hinge = sma(qt.ts<double>(24, get_hinge_diff), 1);
  const auto sma_abs   = sma(qt.ts<double>(24, get_abs_diff), 1);
  if (sma_abs == 0.0) return NA_REAL;
  return sma_hinge / sma_abs * 100;
};


// (HIGH + LOW) / 2 - DELAY(HIGH, 1) + DELAY(LOW, 1) / 2
Alpha_fun alpha068 = [](const Quote& qt) -> double {
  auto tmp_ser = [](const Quote& qt) {
    const auto part1 = (qt.high() + qt.low()) / 2 - (qt.high(1) + qt.low(1)) / 2;
    const auto part2 = (qt.high() - qt.low()) / qt.volume();
    return part1 * part2;
  };
  return sma(qt.ts<double>(15, tmp_ser), 2);
};


// SUM(DTM, 20) > SUM(DBM, 20) => (SUM(DTM, 20) - SUM(DBM, 20)) / SUM(DTM, 20)
// SUM(DTM, 20) = SUM(DBM, 20) => 0
// SUM(DTM, 20) < SUM(DBM, 20) => (SUM(DTM, 20) - SUM(DBM, 20)) / SUM(DBM, 20)
Alpha_fun alpha069 = [](const Quote& qt) -> double {
  const auto sum_dtm_20 = sum(qt.ts_dtm(20));
  const auto sum_dbm_20 = sum(qt.ts_dbm(20));
  if (sum_dtm_20 > sum_dbm_20) {
    return sum_dtm_20 == 0.0 ?
    NA_REAL :
    (sum_dtm_20 - sum_dbm_20) / sum_dtm_20;
  }
  else if (sum_dtm_20 == sum_dbm_20) {
    return 0.0;
  }
  else {
    return sum_dbm_20 == 0.0 ?
    NA_REAL :
    (sum_dtm_20 - sum_dbm_20) / sum_dbm_20;
  }
};


// STD(AMOUNT, 6)
Alpha_fun alpha070 = [](const Quote& qt) -> double {
  return stdev(qt.ts_amount(6));
};


// (CLOSE - MEAN(CLOSE, 24)) / MEAN(CLOSE, 24) * 100
Alpha_fun alpha071 = [](const Quote& qt) -> double {
  const auto mean_close_24 = mean(qt.ts_close(24));
  if (mean_close_24 == 0.0) return NA_REAL;
  return (qt.close() - mean_close_24) / mean_close_24 * 100;
};


// SMA((TSMAX(HIGH, 6) - CLOSE) /
// (TSMAX(HIGH, 6) - TSMIN(LOW, 6)) * 100, 15, 1)
Alpha_fun alpha072 = [](const Quote& qt) -> double {
  auto foo = [](const Quote& qt) {
    // The highest price in the past six days minus the closing price.
    const auto hmc = tsmax(qt.ts_close(6)) - qt.close();
    const auto ran = tsmax(qt.ts_high(6)) - tsmin(qt.ts_low(6));
    if (ran == 0.0) return NA_REAL;
    return hmc / ran * 100;
  };
  return sma(qt.ts<double>(15, foo), 1);
};


// -(TSRANK(DECAYLINEAR(CORR((CLOSE), VOLUME, 10), 16), 4), 5) - RANK(DECAYLINEAR(CORR(RANK(VWAP), MEAN(VOLUME, 30), 4), 3)))
Alpha_mfun alpha073 = [](const Quotes& qts) -> Timeseries {
  auto cor1 = [](const Quote& qt) {
    return corr(qt.ts_close(10), qt.ts_volume(10));
  };
  auto dec1 = [cor1](const Quote& qt) {
    return decaylinear(qt.ts<double>(16, cor1));
  };
  auto dec2 = [dec1](const Quote& qt) {
    return decaylinear(qt.ts<double>(4, dec1));
  };
  auto part1 = [dec2](const Quote& qt) {
    return tsrank(qt.ts<double>(5, dec2));
  };
  auto cor2 = [](const Quote& qt) {
    return corr(
      qt.ts_vwap(4),
      qt.ts<double>(4, [](const Quote& qt) { return mean(qt.ts_volume(30)); })
    );
  };
  auto part2 = [cor2](const Quote& qt) {
    return decaylinear(qt.ts<double>(3, cor2));
  };
  return qts.apply(part1) - rank(qts.apply(part2));
};


// RANK(CORR(SUM(((LOW * 0.35) + (VWAP * 0.65)), 20),
// SUM(MEAN(VOLUME, 40), 20), 7)) +
// RANK(CORR(RANK(VWAP), RANK(VOLUME), 6))
Alpha_mfun alpha074 = [](const Quotes& qts) -> Timeseries {
  auto sum1 = [](const Quote& qt) {
    return sum(qt.ts_low(20) + qt.ts_vwap(20));
  };
  auto sum2 = [](const Quote& qt) {
    return sum(qt.ts<double>(
        20,
        [](const Quote& qt){ return mean(qt.ts_volume(40)); }
    ));
  };
  auto corr1 = [sum1, sum2](const Quote& qt) {
    return corr(qt.ts<double>(7, sum1), qt.ts<double>(7, sum2));
  };
  auto rk_vwap = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.vwap(); }));
  };
  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };
  auto corr2 = [rk_vwap, rk_vol](const Quotes& qts) {
    auto rk1 = qts.tsapply(4, rk_vwap);
    auto rk2 = qts.tsapply(4, rk_vol);
    return apply(rk1, rk2, corr);
  };
  return rank(qts.apply(corr1)) + rank(corr2(qts));
};


// count1 := COUNT(CLOSE > OPEN && BANCHMARKINDEXCLOSE < BANCHMARKINDEXOPEN)
// count2 := COUNT(BANCHMARKINDEXCLOSE < BANCHMARKINDEXOPEN)
// count1 / count2
Alpha_fun alpha075 = [](const Quote& qt) -> double {
  auto count1_fun = [](const Quote& qt) {
    return qt.close() > qt.open() && qt.bmk_close() < qt.bmk_open();
  };
  auto count2_fun = [](const Quote& qt) {
    return qt.bmk_close() < qt.bmk_open();
  };
  auto count1 = count(qt.ts<bool>(50, count1_fun));
  auto count2 = count(qt.ts<bool>(50, count2_fun));
  if (count2 == 0.0) return NA_REAL;
  return count1 / count2;
};


// STD(ABS(CLOSE / DELAY(CLOSE, 1) - 1) / VOLUME, 20) /
// MEAN(ABS(CLOSE / DELAY(CLOSE, 1) - 1) / VOLUME, 20)
Alpha_fun alpha076 = [](const Quote& qt) -> double {
  auto get_abs_dc = [](const Quote& qt) {
    if (qt.volume() == 0.0) return NA_REAL;
    return std::abs(qt.close() / qt.close(1) - 1) / qt.volume();
  };
  auto mdc20 = mean(qt.ts<double>(20, get_abs_dc));
  if (mdc20 == 0.0) return NA_REAL;
  return stdev(qt.ts<double>(20, get_abs_dc)) / mdc20;
};


// MIN(RANK(DECAYLINEAR(((((HIGH + LOW) / 2) + HIGH) - (VWAP + HIGH)), 20)),
//     RANK(DECAYLINEAR(CORR(((HIGH + LOW) / 2), MEAN(VOLUME,40), 3), 6)))
Alpha_mfun alpha077 = [](const Quotes& qts) -> Timeseries {
  auto decay_linear1 = [](const Quote& qt) {
    return decaylinear(
      (qt.ts_high(20) + qt.ts_low(20)) / 2 - (qt.ts_vwap(20) + qt.ts_high(20))
    );
  };

  auto cor = [](const Quote& qt) {
    auto tmp = qt.ts<double>(
      3,
      [](const Quote& qt){ return mean(qt.ts_volume(40)); }
    );
    return corr((qt.ts_high(3) + qt.ts_low(3)) / 2, tmp);
  };
  auto decay_linear2 = [cor](const Quote& qt) {
    return decaylinear(qt.ts<double>(6, cor));
  };
  return pmin(rank(qts.apply(decay_linear1)), rank(qts.apply(decay_linear2)));
};


// *((HIGH + LOW + CLOSE) / 3 - MEAN((HIGH + LOW + CLOSE) / 3, 12)) /
// (0.015 * MEAN(ABS(CLOSE - MEAN((HIGH + LOW + CLOSE) / 3, 12)), 12))
Alpha_fun alpha078 = [](const Quote& qt) -> double {
  auto aver = (qt.high() + qt.low() + qt.close()) / 3;
  auto get_aver = [](const Quote& qt) {
    return (qt.high() + qt.low() + qt.close()) / 3;
  };
  auto get_tmp_ser = [get_aver](const Quote& qt) {
    return abs(qt.close() - mean(qt.ts<double>(12, get_aver)));
  };
  auto aver_12  = qt.ts<double>(12, get_aver);
  auto aver_ser = qt.ts<double>(12, get_tmp_ser);
  auto mean_abs = mean(abs(aver_ser));
  if (mean_abs == 0.0) return NA_REAL;
  return (aver - mean(aver_12)) / (0.015 * mean_abs);
};


// see also alpha63
Alpha_fun alpha079 = [](const Quote& qt) -> double {
  auto get_hinge_diff = [](const Quote& qt) {
    return std::max(qt.close() - qt.pclose(), 0.0);
  };
  auto get_abs_diff   = [](const Quote& qt) {
    return std::abs(qt.close() - qt.pclose());
  };
  const auto sma_hinge = sma(qt.ts<double>(12, get_hinge_diff), 1);
  const auto sma_abs   = sma(qt.ts<double>(12, get_abs_diff), 1);
  if (sma_abs == 0.0) return NA_REAL;
  return sma_hinge / sma_abs * 100;
};


// (VOLUME - DELAY(VOLUME, 5)) / DELAY(VOLUME, 5) * 100
Alpha_fun alpha080 = [](const Quote& qt) -> double {
  if (qt.volume(5) == 0.0) return NA_REAL;
  return (qt.volume() - qt.volume(5)) / qt.volume(5) * 100;
};


// SMA(VOLUME, 21, 2)
Alpha_fun alpha081 = [](const Quote& qt) -> double {
  return sma(qt.ts_volume(21), 2);
};


// SMA((TSMAX(HIGH, 6) - CLOSE) / (TSMAX(HIGH, 6) - TSMIN(LOW, 6)) * 100, 20, 1)
Alpha_fun alpha082 = [](const Quote& qt) -> double {
  auto get_tmp_ser = [](const Quote& qt) {
    const auto hl = tsmax(qt.ts_high(6)) - tsmin(qt.ts_low(6));
    if (hl == 0.0) return NA_REAL;
    return (tsmax(qt.ts_high(6)) - qt.close()) / hl * 100.0;
  };
  return sma(qt.ts<double>(20, get_tmp_ser), 1);
};


// -1 * RANK(COVIANCE(RANK(HIGH), RANK(VOLUME), 5))
Alpha_mfun alpha083 = [](const Quotes& qts) -> Timeseries {
  auto rk_high = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.high(); }));
  };
  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };
  auto rk1 = qts.tsapply(5, rk_high);
  auto rk2 = qts.tsapply(5, rk_vol);
  Timeseries tmp = apply(rk1, rk2, covariance);
  return rank(tmp) * -1.0;
};


// SUM((CLOSE > DELAY(CLOSE, 1) ? VOLUMN : (CLOSE < DELAY(CLOSE, 1) ? -VOLUMN : 0)), 20)
// CLOSE > PCLOSE => factor =  volume
// CLOSE < PCLOSE => factor = -volume
// CLOSE = PCLOSE => factor = 0
Alpha_fun alpha084 = [](const Quote& qt) -> double {
  auto ser = [](const Quote& qt) {
    if (qt.close() > qt.pclose()) {
      return qt.volume();
    }
    else if (qt.close() < qt.pclose()) {
      return -qt.volume();
    }
    else {
      return 0.0;
    }
  };
  return sum(qt.ts<double>(20, ser));
};


// TSRANK((VOLUME / MEAN(VOLUME,20)), 20) * TSRANK((-1 * DELTA(CLOSE, 7)), 8)
Alpha_fun alpha085 = [](const Quote& qt) -> double {
  auto get_delta = [](const Quote& qt) {
    return qt.close() - qt.close(7);
  };
  auto get_tmp   = [](const Quote& qt) {
    auto mvol20 = mean(qt.ts_volume(20));
    if (mvol20 == 0.0) return NA_REAL;
    return qt.volume() / mvol20;
  };
  const double trank_8 = tsrank(qt.ts<double>(8, get_delta));
  const double tmp_20  = tsrank(qt.ts<double>(20, get_tmp));
  return tmp_20 * trank_8;
};


// (0.25 < (((DELAY(CLOSE, 20) - DELAY(CLOSE, 10)) / 10) -
// ((DELAY(CLOSE, 10) - CLOSE) / 10))) ? (-1 * 1) :
// (((((DELAY(CLOSE, 20) - DELAY(CLOSE, 10)) / 10) -
// ((DELAY(CLOSE, 10) - CLOSE) / 10)) < 0) ? 1 :
// ((-1 * 1) * (CLOSE - DELAY(CLOSE, 1))))
Alpha_fun alpha086 = [](const Quote& qt) -> double {
  auto c_20_10 = qt.close(20) - qt.close(10);
  auto c_10_00 = qt.close(10) - qt.close();
  const auto statement = (c_20_10 / 10) - (c_10_00 / 10);
  return (statement > 0.25) ?
  -1.0 :
  (statement < 0) ? 1.0 : -(qt.close() - qt.pclose());
};


// (RANK(DECAYLINEAR(DELTA(VWAP, 4), 7)) +
// TSRANK(DECAYLINEAR(((((LOW * 0.9) + (LOW * 0.1)) - VWAP) /
// (OPEN - ((HIGH + LOW) / 2))), 11), 7)) * -1
Alpha_mfun alpha087 = [](const Quotes& qts) -> Timeseries {
  auto decay_linear1 = [](const Quote& qt) {
    return decaylinear(
      qt.ts<double>(
        7,
        [](const Quote& qt){ return delta(qt.ts_vwap(4)); }
      )
    );
  };

  auto decay_linear2 = [](const Quote& qt) {
    auto part1 = qt.ts_low(11) * 0.9 + qt.ts_low(11) * 0.1 - qt.ts_vwap(11);
    auto part2 = qt.ts_open(11) - (qt.ts_high(11) + qt.ts_low(11) / 2);
    return decaylinear(part1 / part2);
  };

  auto ts_rank = [decay_linear2](const Quote& qt) {
    return tsrank(qt.ts<double>(7, decay_linear2));
  };

  Timeseries part1 = rank(qts.apply(decay_linear1));
  Timeseries part2 = qts.apply(ts_rank);
  return (part1 + part2) * -1.0;
};


// (CLOSE - DELAY(CLOSE, 20)) / DELAY(CLOSE, 20) * 100
Alpha_fun alpha088 = [](const Quote& qt) -> double {
  return (qt.close() / qt.close(20) - 1) * 100;
};


// SMA(CLOSE, 13, 2) - SMA(CLOSE, 27, 2) - SMA(SMA(CLOSE, 13, 2) - SMA(CLOSE, 27, 2), 10, 2)
Alpha_fun alpha089 = [](const Quote& qt) -> double {
  auto get_tmp_ser = [](const Quote& qt) {
    return sma(qt.ts_close(13), 2) - sma(qt.ts_close(27), 2);
  };
  return (sma(qt.ts_close(13), 2) -
          sma(qt.ts_close(27), 2) -
          sma(qt.ts<double>(10, get_tmp_ser), 2)) * 2;
};


// RANK(CORR(RANK(VWAP), RANK(VOLUME), 5)) * -1
Alpha_mfun alpha090 = [](const Quotes& qts) -> Timeseries {
  auto rk_vwap = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.vwap(); }));
  };

  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };

  auto rk1 = qts.tsapply(5, rk_vwap);
  auto rk2 = qts.tsapply(5, rk_vol);
  Timeseries tmp = apply(rk1, rk2, corr);
  return rank(tmp) * -1.0;
};


// (RANK(CLOSE - MAX(CLOSE, 5))) * RANK(CORR(MEAN(VOLUME, 40), LOW, 5)) * -1
Alpha_mfun alpha091 = [](const Quotes& qts) -> Timeseries {
  auto cmm = [](const Quote& qt) {
    return qt.close() - tsmax(qt.ts_close(5));
  };
  auto mean_ser = [](const Quote& qt) {
    return mean(qt.ts_volume(40));
  };
  auto tmp_ser = [mean_ser](const Quote& qt) {
    return corr(qt.ts<double>(5, mean_ser), qt.ts_low(5));
  };
  return rank(qts.apply(cmm)) * rank(qts.apply(tmp_ser)) * -1.0;
};


// MAX(RANK(DECAYLINEAR(DELTA(((CLOSE * 0.35)	+ (VWAP	* 0.65)), 2), 3)),
// TSRANK(DECAYLINEAR(ABS(CORR((MEAN(VOLUME,180)), CLOSE, 13)), 5), 15)) * -1)
Alpha_mfun alpha092 = [](const Quotes& qts) -> Timeseries{
  auto decay_linear1 = [](const Quote& qt) {
    return decaylinear(
      qt.ts<double>(3, [](const Quote& qt) {
        return delta(qt.ts_close(2) * 0.35 + qt.ts_vwap(2) * 0.62);
      })
    );
  };

  auto decay_linear2 = [](const Quote& qt) {
    auto mean_ser = qt.ts<double>(13, [](const Quote& qt) {
      return mean(qt.ts_volume(180));
    });
    return decaylinear(qt.ts<double>(5, [mean_ser](const Quote& qt) {
      return abs(corr(mean_ser, qt.ts_close(13)));
    }));
  };

  auto ts_rank = [decay_linear2](const Quote& qt) {
    return tsrank(qt.ts<double>(15, decay_linear2));
  };
  Timeseries part1 = rank(qts.apply(decay_linear1));
  Timeseries part2 = qts.apply(ts_rank);
  return pmax(part1, part2);
};


// SUM((OPEN >= DELAY(OPEN, 1) ? 0 : MAX(OPEN - LOW, OPEN - DELAY(OPEN, 1))), 20)
Alpha_fun alpha093 = [](const Quote& qt) -> double {
  auto ser = [](const Quote& qt) {
    if (qt.open() >= qt.open(1)) {
      return 0.0;
    }
    else {
      return std::max(qt.open() - qt.low(), qt.open() - qt.open(1));
    }
  };
  return sum(qt.ts<double>(20, ser));
};


// SUM((CLOSE > DELAY(CLOSE, 1) ? VOLUME :
// (CLOSE < DELAY(CLOSE, 1) ? -VOLUME : 0)), 30)
Alpha_fun alpha094 = [](const Quote& qt) -> double {
  auto ser = [](const Quote& qt) {
    auto close = qt.close();
    auto pclose = qt.pclose();
    return (close > pclose) ?
    qt.volume() :
      (close < pclose) ? -qt.volume() : 0.0;
  };
  return sum(qt.ts<double>(30, ser));
};


// STD(AMOUNT, 20)
Alpha_fun alpha095 = [](const Quote& qt) -> double {
  return stdev(qt.ts_amount(20));
};


// SMA(SMA((CLOSE - TSMIN(LOW, 9)) / (TSMAX(HIGH, 9) - TSMIN(LOW, 9)) * 100, 3, 1), 3, 1)
Alpha_fun alpha096 = [](const Quote& qt) -> double {
  auto tmp_ser = [](const Quote& qt) {
    auto minl9 = tsmin(qt.ts_low(9));
    auto maxh9 = tsmin(qt.ts_high(9));
    if (maxh9 == minl9) return NA_REAL;
    return (qt.close() - minl9) / (maxh9 - minl9) * 100;
  };
  auto fun = [tmp_ser](const Quote& qt) {
    return sma(qt.ts<double>(3, tmp_ser), 1);
  };
  return sma(qt.ts<double>(3, fun), 1);
};


// STD(VOLUME, 10)
Alpha_fun alpha097 = [](const Quote& qt) -> double {
  return stdev(qt.ts_volume(10));
};


// (((DELTA((SUM(CLOSE, 100) / 100), 100) /
// DELAY(CLOSE, 100)) < 0.05) || ((DELTA((SUM(CLOSE, 100) / 100), 100) /
// DELAY(CLOSE, 100)) == 0.05)) ?
// (-1 * (CLOSE - TSMIN(CLOSE, 100))) : (-1 * DELTA(CLOSE, 3))
Alpha_fun alpha098 = [](const Quote& qt) -> double {
  auto fun = [](const Quote& qt) { return mean(qt.ts_close(100)); };
  auto delta100 = delta(qt.ts<double>(100, fun));
  auto close100 = qt.close(100);
  bool statement = (delta100 / close100) <= 0.05;
  auto min_close_100 = tsmin(qt.ts_close(100));
  return statement ? -(qt.close() - min_close_100) : -delta(qt.ts_close(3));
};


// -1 * RANK(COVIANCE(RANK(CLOSE), RANK(VOLUME), 5))
Alpha_mfun alpha099 = [](const Quotes& qts) -> Timeseries{
  auto rk_close = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.close(); }));
  };
  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };
  auto rk1 = qts.tsapply(5, rk_close);
  auto rk2 = qts.tsapply(5, rk_vol);
  Timeseries tmp = apply(rk1, rk2, covariance);
  return rank(tmp) * -1.0;
};


// STD(VOLUME, 20)
Alpha_fun alpha100 = [](const Quote& qt) -> double {
  return stdev(qt.ts_volume(20));
};


// ((RANK(CORR(CLOSE, SUM(MEAN(VOLUME,30), 37), 15)) <
//   RANK(CORR(RANK(((HIGH * 0.1) + (VWAP * 0.9))), RANK(VOLUME), 11))) * -1)
Alpha_mfun alpha101= [](const Quotes& qts) -> Timeseries{
  auto mean_ser = [](const Quote& qt) {
    return mean(qt.ts_volume(30));
  };
  auto sum_mean_vol = [mean_ser](const Quote& qt) {
    return sum(qt.ts<double>(37, mean_ser));
  };
  auto corr1 = [sum_mean_vol](const Quote& qt) {
    return corr(qt.ts_close(15), qt.ts<double>(15, sum_mean_vol));
  };

  auto rk_hav = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {
      return qt.high() * 0.1 + qt.vwap() * 0.9;
    }));
  };
  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) {
      return qt.volume();
    }));
  };
  auto corr2 = [rk_hav, rk_vol](const Quotes& qts) {
    auto rk1 = qts.tsapply(11, rk_hav);
    auto rk2 = qts.tsapply(11, rk_vol);
    return apply(rk1, rk2, corr);
  };
  Timeseries rk1 = rank(qts.apply(corr1));
  Timeseries rk2 = rank(corr2(qts));
  return (rk1 < rk2) * -1.0;
};


// SMA(MAX(VOLUME-DELAY(VOLUME, 1), 0), 6, 1) / SMA(ABS(VOLUME-DELAY(VOLUME, 1)), 6, 1) * 100
Alpha_fun alpha102 = [](const Quote& qt) -> double {
  auto fun_max = [](const Quote& qt) {
    return std::max(qt.volume() - qt.volume(1), 0.0);
  };
  auto fun_abs = [](const Quote& qt) {
    return std::abs(qt.volume() - qt.volume(1));
  };
  auto sma_max = sma(qt.ts<double>(6, fun_max), 1);
  auto sma_abs = sma(qt.ts<double>(6, fun_abs), 1);
  return sma_max / sma_abs * 100;
};


// ((20 - LOWDAY(LOW, 20)) / 20) * 100
Alpha_fun alpha103 = [](const Quote& qt) -> double {
  return (20 - lowday(qt.ts_low(20))) / 20 * 100;
};


// -1 * (DELTA(CORR(HIGH, VOLUME, 5), 5) * RANK(STD(CLOSE, 20)))
Alpha_mfun alpha104= [](const Quotes& qts) -> Timeseries{
  auto std = [](const Quote& qt) {
    return stdev(qt.ts_close(20));
  };
  auto cor_h_v = [](const Quote& qt) {
    return corr(qt.ts_high(5), qt.ts_volume(5));
  };
  auto del = [cor_h_v](const Quote& qt) {
    return delta(qt.ts<double>(5, cor_h_v));
  };
  Timeseries rk = rank(qts.apply(std));
  Timeseries dl = qts.apply(del);
  return dl * rk * -1.0;
};


// -1 * CORR(RANK(OPEN), RANK(VOLUME), 10)
Alpha_mfun alpha105= [](const Quotes& qts) -> Timeseries{
  auto rk_open = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.open(); }));
  };
  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };
  auto rk1 = qts.tsapply(10, rk_open);
  auto rk2 = qts.tsapply(10, rk_vol);
  return apply(rk1, rk2, corr) * -1.0;
};


// CLOSE - DELAY(CLOSE, 20)
Alpha_fun alpha106 = [](const Quote& qt) -> double {
  return qt.close() - qt.close(20);
};


// (-1 * RANK((OPEN - DELAY(HIGH, 1)))) *
// RANK((OPEN - DELAY(CLOSE, 1))) * RANK((OPEN - DELAY(LOW, 1)))
Alpha_mfun alpha107= [](const Quotes& qts) -> Timeseries{
  auto part1 = [](const Quote& qt) {
    return qt.open() - qt.high(1);
  };
  auto part2 = [](const Quote& qt) {
    return qt.open() - qt.pclose();
  };
  auto part3 = [](const Quote& qt) {
    return qt.open() - qt.low(1);
  };
  Timeseries rk1 = rank(qts.apply(part1));
  Timeseries rk2 = rank(qts.apply(part2));
  Timeseries rk3 = rank(qts.apply(part3));
  return rk1 * rk2 * rk3 * -1.0;
};


// (RANK((HIGH - MIN(HIGH, 2))) ^ RANK(CORR((VWAP), (MEAN(VOLUME,120)), 6))) * -1
Alpha_mfun alpha108= [](const Quotes& qts) -> Timeseries{
  auto part1 = [](const Quote& qt) {
    return qt.high() - tsmin(qt.ts_high(2));
  };
  auto mvol120 = [](const Quote& qt) {
    return mean(qt.ts_volume(120));
  };
  auto part2 = [mvol120](const Quote& qt) {
    return corr(qt.ts_vwap(6), qt.ts<double>(6, mvol120));
  };
  Timeseries rk1 = rank(qts.apply(part1));
  Timeseries rk2 = rank(qts.apply(part2));
  return pow(rk1, rk2) * -1.0;
};


// SMA(HIGH - LOW, 10, 2) / SMA(SMA(HIGH - LOW), 10, 2)
Alpha_fun alpha109 = [](const Quote& qt) -> double {
  auto sma_ser_0 = sma(qt.ts_high(10) - qt.ts_low(10), 2);
  auto sma_ser = [](const Quote& qt) {
    return sma(qt.ts_high(10) - qt.ts_low(10), 2);
  };
  return sma_ser_0 / sma(qt.ts<double>(10, sma_ser), 2);
};


// SUM(MAX(0, HIGH - DELAY(CLOSE, 1)), 20) / SUM(MAX(0, DELAY(CLOSE, 1) - LOW), 20) * 100
Alpha_fun alpha110 = [](const Quote& qt) -> double {
  auto fun1 = [](const Quote& qt) {
    return std::max(0.0, qt.high() - qt.pclose());
  };
  auto fun2 = [](const Quote& qt) {
    return std::max(0.0, qt.pclose() - qt.low());
  };
  auto deno = sum(qt.ts<double>(20, fun2));
  if (deno == 0.0) return NA_REAL;
  auto num = sum(qt.ts<double>(20, fun1));
  return num / deno * 100.0;
};


// SMA(VOL * ((CLOSE - LOW) - (HIGH - CLOSE)) / (HIGH - LOW), 11, 2) -
// SMA(VOL * ((CLOSE - LOW) - (HIGH - CLOSE)) / (HIGH - LOW), 4, 2)
Alpha_fun alpha111 = [](const Quote& qt) -> double {
  auto fun = [](const Quote& qt) {
    auto v = qt.volume();
    auto h = qt.high();
    auto l = qt.low();
    auto c = qt.close();
    auto hl = h - l;
    if (hl == 0.0) return NA_REAL;
    return v * ((c - l) - (h - c)) / hl;
  };
  return sma(qt.ts<double>(11, fun), 2) - sma(qt.ts<double>(4, fun), 2);
};


// (SUM((CLOSE - DELAY(CLOSE, 1) > 0 ? CLOSE - DELAY(CLOSE, 1) : 0), 12) -
//  SUM((CLOSE - DELAY(CLOSE, 1) < 0 ? DELAY(CLOSE, 1) - CLOSE : 0), 12)) /
// (SUM((CLOSE - DELAY(CLOSE, 1) > 0 ? CLOSE - DELAY(CLOSE, 1) : 0), 12) +
//  SUM((CLOSE - DELAY(CLOSE, 1) < 0 ? DELAY(CLOSE, 1) - CLOSE : 0), 12)) * 100
Alpha_fun alpha112 = [](const Quote& qt) -> double {
  auto fun1 = [](const Quote& qt) {
    return qt.close() - qt.pclose() > 0 ? qt.close() - qt.pclose() : 0;
  };
  auto fun2 = [](const Quote& qt) {
    return qt.close() - qt.pclose() < 0 ? qt.pclose() - qt.close() : 0;
  };
  const auto sum_1_12 = sum(qt.ts<double>(12, fun1));
  const auto sum_2_12 = sum(qt.ts<double>(12, fun2));
  auto deno = sum_1_12 + sum_2_12;
  if (deno == 0.0) return NA_REAL;
  return (sum_1_12 - sum_2_12) / deno * 100;
};


// -1 * ((RANK((SUM(DELAY(CLOSE, 5), 20) / 20)) * CORR(CLOSE, VOLUME, 2)) * RANK(CORR(SUM(CLOSE, 5), SUM(CLOSE, 20), 2)))
Alpha_mfun alpha113= [](const Quotes& qts) -> Timeseries{
  auto part1 = [](const Quote& qt) {
    return sum(qt.ts<double>(20, [](const Quote& qt) { return qt.close(5); })) / 20;
  };

  auto part2 = [](const Quote& qt) {
    return corr(qt.ts_close(2), qt.ts_volume(2));
  };

  auto part3 = [](const Quote& qt) {
    auto sum1 = [](const Quote& qt) { return sum(qt.ts_close(5 )); };
    auto sum2 = [](const Quote& qt) { return sum(qt.ts_close(20)); };
    return corr(qt.ts<double>(2, sum1), qt.ts<double>(2, sum2));
  };
  Timeseries ts1 = rank(qts.apply(part1));
  Timeseries ts2 = qts.apply(part2);
  Timeseries ts3 = rank(qts.apply(part3));
  return ts1 * ts2 * ts3 * -1.0;
};


// RANK(DELAY(((HIGH - LOW) / (SUM(CLOSE, 5) / 5)), 2)) * RANK(RANK(VOLUME)) / (((HIGH - LOW) / (SUM(CLOSE, 5) / 5)) / (VWAP - CLOSE))
Alpha_mfun alpha114= [](const Quotes& qts) -> Timeseries{
  auto part1 = [](const Quote& qt) {
    auto sc_5_2 = sum(qt.ts_close(5, 2));
    if (sc_5_2 == 0.0) return NA_REAL;
    return (qt.high(2) - qt.low(2)) / (sc_5_2 / 5);
  };

  auto part2 = [](const Quote& qt) {
    auto sc_5 = sum(qt.ts_close(5));
    auto vc = qt.vwap() - qt.close();
    if (sc_5 == 0.0 || vc == 0.0) return NA_REAL;
    return (qt.high() - qt.low()) / (sc_5 / 5) / vc;
  };

  Timeseries ts1 = rank(qts.apply(part1));
  Timeseries ts2 = rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  Timeseries ts3 = qts.apply(part2);
  return ts1 * ts2 * ts3;
};


// RANK(CORR(((HIGH * 0.9) + (CLOSE * 0.1)), MEAN(VOLUME,30), 10)) ^
// RANK(CORR(TSRANK(((HIGH + LOW) / 2), 4), TSRANK(VOLUME, 10), 7))
Alpha_mfun alpha115 = [](const Quotes& qts) -> Timeseries{
  auto corr1 = [](const Quote& qt) {
    auto part1 = [](const Quote& qt) { return qt.high() * 0.9 + qt.close() * 0.1; };
    auto part2 = [](const Quote& qt) { return mean(qt.ts_volume(30)); };
    return corr(qt.ts<double>(10, part1), qt.ts<double>(10, part2));
  };

  auto tsrank1 = [](const Quote& qt) {
    auto tmp = [](const Quote& qt) { return (qt.high() + qt.low()) / 2; };
    return tsrank(qt.ts<double>(4, tmp));
  };

  auto tsrank2 = [](const Quote& qt) {
    return tsrank(qt.ts_volume(10));
  };

  auto corr2 = [tsrank1, tsrank2](const Quote& qt) {
    return corr(qt.ts<double>(7, tsrank1), qt.ts<double>(7, tsrank2));
  };
  return pow(rank(qts.apply(corr1)), rank(qts.apply(corr2)));
};


// REGBETA(CLOSE, SEQUENCE, 20)
Alpha_fun alpha116 = [](const Quote& qt) -> double {
  return regbeta(qt.ts_close(20), sequence(20));
};


// TSRANK(VOLUME. 32) * (1 - TSRANK(CLOSE + HIGH - LOW, 16) * (1 - TSRANK(RET, 32))
Alpha_fun alpha117 = [](const Quote& qt) -> double {
  const auto part1 = tsrank(qt.ts_volume(32));
  const auto part2 = 1 - tsrank(qt.ts_close(16) + qt.ts_high(16) - qt.ts_low(16));
  const auto part3 = 1 - tsrank(qt.ts_ret(32));
  return part1 * part2 * part3;
};


// SUM(HIGH - OPEN, 20) / SUM(OPEN - LOW, 20) * 100
Alpha_fun alpha118 = [](const Quote& qt) -> double {
  auto h20 = qt.ts_high(20);
  auto o20 = qt.ts_open(20);
  auto l20 = qt.ts_low(20);
  auto deno = sum(o20 - l20);
  if (deno == 0.0) return NA_REAL;
  return sum(h20 - o20) / deno * 100;
};


// (RANK(DECAYLINEAR(CORR(VWAP, SUM(MEAN(VOLUME,5), 26), 5), 7)) -
//  RANK(DECAYLINEAR(TSRANK(MIN(CORR(RANK(OPEN), RANK(MEAN(VOLUME,15)), 21), 9), 7), 8)))
Alpha_mfun alpha119 = [](const Quotes& qts) -> Timeseries{
  auto sum_mean_vol5 = [](const Quote& qt) {
    auto mvol5 = [](const Quote& qt) { return mean(qt.ts_volume(5)); };
    return sum(qt.ts<double>(26, mvol5));
  };
  auto cor = [sum_mean_vol5](const Quote& qt) {
    return corr(qt.ts_vwap(5), qt.ts<double>(5, sum_mean_vol5));
  };
  auto decay_linear1 = [cor](const Quote& qt) {
    return decaylinear(qt.ts<double>(7, cor));
  };
  auto rk_open = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.open(); }));
  };
  auto rk_mean_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return mean(qt.ts_volume(15)); }));
  };
  auto decay_linear2 = [rk_open, rk_mean_vol](const Quotes& qts) {
    auto rk1 = qts.tsapply(21, rk_open);
    auto rk2 = qts.tsapply(21, rk_mean_vol);
    auto cor = [rk1, rk2](const Quotes& quotes) { return apply(rk1, rk2, corr); };
    auto corr1 = qts.tsapply(9, cor);
    auto mi1 = [corr1](const Quotes& quotes) { return apply(corr1, tsmin); };
    auto min1 = qts.tsapply(7, mi1);
    auto ts_rank = [min1](const Quotes& quotes) { return apply(min1, tsrank); };
    return apply(qts.tsapply(8, ts_rank), decaylinear);
  };
  return rank(qts.apply(decay_linear1)) - rank(decay_linear2(qts));
};


// RANK((VWAP - CLOSE)) / RANK((VWAP + CLOSE))
Alpha_mfun alpha120 = [](const Quotes& qts) -> Timeseries{
  auto vmc = [](const Quote& qt) {
    return qt.vwap() - qt.close();
  };
  auto vac = [](const Quote& qt) {
    return qt.vwap() + qt.close();
  };
  return rank(qts.apply(vmc)) / rank(qts.apply(vac));
};

}
