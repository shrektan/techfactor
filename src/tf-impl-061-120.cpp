#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{
//MAX(RANK(DECAYLINEAR(DELTA(VWAP, 1), 12)), RANK(DECAYLINEAR(RANK(CORR((LOW), MEAN(VOLUME,80), 8)), 17))) * -1
Alpha_mfun alpha061 = [](const Quotes& qts) -> Timeseries {
  auto decay_linear1 = [](const Quote& qt) {
    return decaylinear(qt.ts<double>(12, [](const Quote& qt) { return delta(qt.ts_vwap(2)); }));
  };

  auto cor = [](const Quote& qt) {
    return corr(qt.ts<double>(8, [](const Quote& qt) { return mean(qt.ts_volume(80)); }), qt.ts_low(8));
  };

  auto rk = [&](const Quotes& qts) {
    return rank(qts.apply(cor));
  };

  auto decay_linear2 = [&](const Quotes& qts) {
    auto tmp = qts.tsapply(17, rk);
    return apply(tmp, decaylinear);
  };
  Timeseries part1 = qts.apply(decay_linear1);
  Timeseries part2 = decay_linear2(qts);
  return pmax(rank(part1), rank(part2));
};


// -1 * CORR(HIGH, RANK(VOLUME), 5)
Alpha_mfun alpha062 = [](const Quotes& quotes) -> Timeseries {
  const int num_day = 5;
  auto volume = [](const Quote& quote, const int delay) {
    return quote.volume(delay);
  };

  auto high = [](const Quote& quote, const int delay) {
    return quote.high(delay);
  };

  auto rank_vol = [&](const int delay) {
    return rank(quotes.apply([&](const Quote& quote) { return volume(quote, delay); }));
  };

  auto high_ser = [&](const int delay) {
    return quotes.apply([&](const Quote& quote) { return high(quote, delay); });
  };

  auto rk1 = ts<Timeseries>(num_day, rank_vol);
  auto rk2 = ts<Timeseries>(num_day, high_ser);

  return apply(rk1, rk2, corr) * -1.0;
};


// SMA(MAX(CLOSE - DELAY(CLOSE, 1), 0), 6, 1) / SMA(ABS(CLOSE - DELAY(CLOSE, 1)), 6, 1) * 100
Alpha_fun alpha063 = [](const Quote& quote) -> double {
  auto get_hinge_diff = [&quote](const int delay) {
    return std::max(quote.close(delay) - quote.pclose(delay), 0.0);
  };

  auto get_abs_diff   = [&quote](const int delay) {
    return std::abs(quote.close(delay) - quote.pclose(delay));
  };
  const auto sma_hinge = sma(ts<double>(6, get_hinge_diff), 1);
  const auto sma_abs   = sma(ts<double>(6, get_abs_diff  ), 1);
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

  auto decay_linear2 = [&](const Quotes& qts) {
    auto rk1 = qts.tsapply(4, rk_close);
    auto rk2 = qts.tsapply(4, rk_mean_vol);
    auto cor = [&](const Quotes& quotes) { return apply(rk1, rk2, corr); };
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
Alpha_fun alpha065 = [](const Quote& quote) -> double {
  return mean(quote.ts_close(6)) / quote.close();
};


// (CLOSE - MEAN(CLOSE, 6)) / MEAN(CLOSE, 6) * 100
Alpha_fun alpha066 = [](const Quote& quote) -> double {
  const auto mean_close_6 = mean(quote.ts_close(6));
  return (quote.close() - mean_close_6) / mean_close_6 * 100;
};


// See also alpha63
Alpha_fun alpha067 = [](const Quote& quote) -> double {
  auto get_hinge_diff = [&quote](const int delay) {
    return std::max(quote.close(delay) - quote.pclose(delay), 0.0);
  };

  auto get_abs_diff   = [&quote](const int delay) {
    return std::abs(quote.close(delay) - quote.pclose(delay));
  };
  const auto sma_hinge = sma(ts<double>(24, get_hinge_diff), 1);
  const auto sma_abs   = sma(ts<double>(24, get_abs_diff  ), 1);
  return sma_hinge / sma_abs * 100;
};


// (HIGH + LOW) / 2 - DELAY(HIGH, 1) + DELAY(LOW, 1) / 2
Alpha_fun alpha068 = [](const Quote& quote) -> double {
  auto tmp_ser = [&quote](const int delay) {
    const auto part1 = (quote.high(delay) + quote.low(delay)) / 2 - (quote.high(delay + 1) + quote.low(delay + 1)) / 2;
    const auto part2 = (quote.high(delay) - quote.low(delay)) / quote.volume(delay);
    return part1 * part2;
  };
  return sma(ts<double>(15, tmp_ser), 2);
};


// SUM(DTM, 20) > SUM(DBM, 20) => (SUM(DTM, 20) - SUM(DBM, 20)) / SUM(DTM, 20)
// SUM(DTM, 20) = SUM(DBM, 20) => 0
// SUM(DTM, 20) < SUM(DBM, 20) => (SUM(DTM, 20) - SUM(DBM, 20)) / SUM(DBM, 20)
Alpha_fun alpha069 = [](const Quote& quote) -> double {
  double factor;
  const auto sum_dtm_20 = sum(quote.ts_dtm(20));
  const auto sum_dbm_20 = sum(quote.ts_dbm(20));
  if(sum_dtm_20 > sum_dbm_20)
  {
    factor = (sum_dtm_20 - sum_dbm_20) / sum_dtm_20;
  }
  else if(sum_dtm_20 == sum_dbm_20)
  {
    factor = 0;
  }
  else
  {
    factor = (sum_dtm_20 - sum_dbm_20) / sum_dbm_20;
  }
  return factor;
};


// STD(AMOUNT, 6)
Alpha_fun alpha070 = [](const Quote& quote) -> double {
  if(ISNA(sum(quote.ts_amount(6))))
  {
    return NA_REAL;
  }
  return stdev(quote.ts_amount(6));
};


// (CLOSE - MEAN(CLOSE, 24)) / MEAN(CLOSE, 24) * 100
Alpha_fun alpha071 = [](const Quote& quote) -> double {
  const auto mean_close_24 = mean(quote.ts_close(24));
  return (quote.close() - mean_close_24) / mean_close_24 * 100;
};


// SMA((TSMAX(HIGH, 6) - CLOSE) / (TSMAX(HIGH, 6) - TSMIN(LOW, 6)) * 100, 15, 1)
Alpha_fun alpha072 = [](const Quote& quote) -> double {
  auto foo = [&quote](const int delay) {
    // The highest price in the past six days minus the closing price.
    const auto hmc = tsmax(quote.ts_close(6, delay)) - quote.close(delay);
    const auto ran = tsmax(quote.ts_high(6, delay)) - tsmin(quote.ts_low(6, delay));
    return hmc / ran * 100;
  };
  return sma(ts<double>(15, foo), 1);
};


// -(TSRANK(DECAYLINEAR(CORR((CLOSE), VOLUME, 10), 16), 4), 5) - RANK(DECAYLINEAR(CORR(RANK(VWAP), MEAN(VOLUME, 30), 4), 3)))
Alpha_mfun alpha073 = [](const Quotes& qts) -> Timeseries {
  auto cor1 = [](const Quote& qt) {
    return corr(qt.ts_close(10), qt.ts_volume(10));
  };

  auto dec1 = [&](const Quote& qt) {
    return decaylinear(qt.ts<double>(16, cor1));
  };

  auto dec2 = [&](const Quote& qt) {
    return decaylinear(qt.ts<double>(4, dec1));
  };

  auto part1 = [&](const Quote& qt) {
    return tsrank(qt.ts<double>(5, dec2));
  };

  auto cor2 = [](const Quote& qt) {
    return corr(qt.ts_vwap(4), qt.ts<double>(4, [](const Quote& qt) { return mean(qt.ts_volume(30)); }));
  };

  auto part2 = [&](const Quote& qt) {
    return decaylinear(qt.ts<double>(3, cor2));
  };

  return qts.apply(part1) - rank(qts.apply(part2));
};


// RANK(CORR(SUM(((LOW * 0.35) + (VWAP * 0.65)), 20), SUM(MEAN(VOLUME, 40), 20), 7)) +
// RANK(CORR(RANK(VWAP), RANK(VOLUME), 6))
Alpha_mfun alpha074 = [](const Quotes& qts) -> Timeseries {
  auto sum1 = [](const Quote& qt) {
    return sum(qt.ts_low(20) + qt.ts_vwap(20));
  };

  auto sum2 = [](const Quote& qt) {
    return sum(qt.ts<double>(20, [](const Quote& qt){ return mean(qt.ts_volume(40)); }));
  };

  auto corr1 = [&](const Quote& qt) {
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
Alpha_fun alpha075 = [](const Quote& quote) -> double {
  auto count1 = [&quote](const int delay) {
    return quote.close(delay) > quote.open(delay) && quote.bmk_close(delay) < quote.bmk_open(delay);
  };

  auto count2 = [&quote](const int delay) {
    return quote.bmk_close(delay) < quote.bmk_open(delay);
  };

  if(ISNA(count(ts<bool>(50, count2))) || count(ts<bool>(50, count2)) == 0.0)
  {
    return NA_REAL;
  }
  return count(ts<bool>(50, count1)) / count(ts<bool>(50, count2));
};


// STD(ABS(CLOSE / DELAY(CLOSE, 1) - 1) / VOLUME, 20) / MEAN(ABS(CLOSE / DELAY(CLOSE, 1) - 1) / VOLUME, 20)
Alpha_fun alpha076 = [](const Quote& quote) -> double {
  auto get_abs_dc = [&quote](const int delay) {
    return std::abs(quote.close(delay) / quote.close(delay + 1) - 1) / quote.volume(delay);
  };

  if(ISNA(mean(ts<double>(20, get_abs_dc))) || mean(ts<double>(20, get_abs_dc)) == 0.0)
  {
    return NA_REAL;
  }
  return stdev(ts<double>(20, get_abs_dc)) / mean(ts<double>(20, get_abs_dc));
};


// MIN(RANK(DECAYLINEAR(((((HIGH + LOW) / 2) + HIGH) - (VWAP + HIGH)), 20)),
//     RANK(DECAYLINEAR(CORR(((HIGH + LOW) / 2), MEAN(VOLUME,40), 3), 6)))
Alpha_mfun alpha077 = [](const Quotes& qts) -> Timeseries {
  auto decay_linear1 = [](const Quote& qt) {
    return decaylinear((qt.ts_high(20) + qt.ts_low(20)) / 2 - (qt.ts_vwap(20) + qt.ts_high(20)));
  };

  auto cor = [](const Quote& qt) {
    auto tmp = qt.ts<double>(3, [](const Quote& qt){ return mean(qt.ts_volume(40)); });
    return corr((qt.ts_high(3) + qt.ts_low(3)) / 2, tmp);
  };

  auto decay_linear2 = [&](const Quote& qt) {
    return decaylinear(qt.ts<double>(6, cor));
  };
  return pmin(rank(qts.apply(decay_linear1)), rank(qts.apply(decay_linear2)));
};


// *((HIGH + LOW + CLOSE) / 3 - MEAN((HIGH + LOW + CLOSE) / 3, 12)) /
// (0.015 * MEAN(ABS(CLOSE - MEAN((HIGH + LOW + CLOSE) / 3, 12)), 12))
Alpha_fun alpha078 = [](const Quote& quote) -> double {
  auto get_aver = [&quote](const int delay) {
    return (quote.high(delay) + quote.low(delay) + quote.close(delay)) / 3;
  };

  auto get_tmp_ser = [&quote, get_aver](const int delay) {
    return abs(quote.close(delay) - mean(ts<double>(12 + delay, get_aver)));
  };
  std::vector<double> aver_12  = ts<double>(12, get_aver);
  std::vector<double> aver_ser = ts<double>(12, get_tmp_ser);
  return (get_aver(0) - mean(aver_12)) / (0.015 * mean(abs(aver_ser)));
};


// see also alpha63
Alpha_fun alpha079 = [](const Quote& quote) -> double {
  auto get_hinge_diff = [&quote](const int delay) {
    return std::max(quote.close(delay) - quote.pclose(delay), 0.0);
  };

  auto get_abs_diff   = [&quote](const int delay) {
    return std::abs(quote.close(delay) - quote.pclose(delay));
  };
  const auto sma_hinge = sma(ts<double>(12, get_hinge_diff), 1);
  const auto sma_abs   = sma(ts<double>(12, get_abs_diff  ), 1);
  return sma_hinge / sma_abs * 100;
};


// (VOLUME - DELAY(VOLUME, 5)) / DELAY(VOLUME, 5) * 100
Alpha_fun alpha080 = [](const Quote& quote) -> double {
  return (quote.volume() - quote.volume(5)) / quote.volume(5) * 100;
};


// SMA(VOLUME, 21, 2)
Alpha_fun alpha081 = [](const Quote& quote) -> double {
  return sma(quote.ts_volume(21), 2);
};


// SMA((TSMAX(HIGH, 6) - CLOSE) / (TSMAX(HIGH, 6) - TSMIN(LOW, 6)) * 100, 20, 1)
Alpha_fun alpha082 = [](const Quote& quote) -> double {
  auto get_tmp_ser = [&quote](const int delay) {
    const auto tmp = (tsmax(quote.ts_high(6, delay)) - quote.close(delay)) /
      (tsmax(quote.ts_high(6, delay)) - tsmin(quote.ts_low(6, delay))) * 100;
    return tmp;
  };
  return sma(ts<double>(20, get_tmp_ser), 1);
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
Alpha_fun alpha084 = [](const Quote& quote) -> double {
  auto get_tmp_ser = [&quote](const int delay) {
    double factor;
    if(quote.close(delay) > quote.pclose(delay))
    {
      factor = quote.volume((delay));
    }
    else if(quote.close(delay) < quote.pclose(delay))
    {
      factor = -quote.volume((delay));
    }
    else
    {
      factor = 0.0;
    }
    return factor;
  };
  return sum(ts<double>(20, get_tmp_ser));
};


// TSRANK((VOLUME / MEAN(VOLUME,20)), 20) * TSRANK((-1 * DELTA(CLOSE, 7)), 8)
Alpha_fun alpha085 = [](const Quote& quote) -> double {
  auto get_delta = [&quote](const int delay) {
    return quote.close(delay) - quote.close(delay + 7);
  };

  auto get_tmp   = [&quote](const int delay) {
    return quote.volume(delay) / mean(quote.ts_volume(delay + 20));
  };
  const double trank_8 = tsrank(ts<double>(8,  get_delta));
  const double tmp_20  = tsrank(ts<double>(20, get_tmp  ));
  return tmp_20 * trank_8;
};


// (0.25 < (((DELAY(CLOSE, 20) - DELAY(CLOSE, 10)) / 10) - ((DELAY(CLOSE, 10) - CLOSE) / 10))) ? (-1 * 1) :
// (((((DELAY(CLOSE, 20) - DELAY(CLOSE, 10)) / 10) - ((DELAY(CLOSE, 10) - CLOSE) / 10)) < 0) ? 1 :
// ((-1 * 1) * (CLOSE - DELAY(CLOSE, 1))))
Alpha_fun alpha086 = [](const Quote& quote) -> double {
  const auto statement = ((quote.close(20) - quote.close(10)) / 10) -
    ((quote.close(10) - quote.close()) / 10);
  double factor;
  if(statement > 0.25)
  {
    factor = -1.0;
  }
  else if(statement < 0)
  {
    factor = 1.0;
  }
  else
  {
    factor = -(quote.close() - quote.pclose());
  }
  return factor;
};


// (RANK(DECAYLINEAR(DELTA(VWAP, 4), 7)) + TSRANK(DECAYLINEAR(((((LOW * 0.9) + (LOW * 0.1)) - VWAP) /
// (OPEN - ((HIGH + LOW) / 2))), 11), 7)) * -1
Alpha_mfun alpha087 = [](const Quotes& qts) -> Timeseries {
  auto decay_linear1 = [](const Quote& qt) {
    auto tmp = qt.ts<double>(7, [](const Quote& qt){ return delta(qt.ts_vwap(4)); });
    return decaylinear(tmp);
  };

  auto decay_linear2 = [](const Quote& qt) {
    auto part1 = qt.ts_low(11) * 0.9 + qt.ts_low(11) * 0.1 - qt.ts_vwap(11);
    auto part2 = qt.ts_open(11) - (qt.ts_high(11) + qt.ts_low(11) / 2);
    return decaylinear(part1 / part2);
  };

  auto ts_rank = [&](const Quote& qt) {
    return tsrank(qt.ts<double>(7, decay_linear2));
  };

  Timeseries part1 = rank(qts.apply(decay_linear1));
  Timeseries part2 = qts.apply(ts_rank);
  return (part1 + part2) * -1.0;
};


// (CLOSE - DELAY(CLOSE, 20)) / DELAY(CLOSE, 20) * 100
Alpha_fun alpha088 = [](const Quote& quote) -> double {
  return (quote.close() / quote.close(20) - 1) * 100;
};


// SMA(CLOSE, 13, 2) - SMA(CLOSE, 27, 2) - SMA(SMA(CLOSE, 13, 2) - SMA(CLOSE, 27, 2), 10, 2)
Alpha_fun alpha089 = [](const Quote& quote) -> double {
  auto get_tmp_ser = [&quote](const int delay) {
    return sma(quote.ts_close(13, delay), 2) - sma(quote.ts_close(27, delay), 2);
  };
  return (sma(quote.ts_close(13), 2) - sma(quote.ts_close(27), 2) - sma(ts<double>(10, get_tmp_ser), 2)) * 2;
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
Alpha_mfun alpha091 = [](const Quotes& quotes) -> Timeseries {
  auto cmm = [](const Quote& quote) {
    return quote.close() - tsmax(quote.ts_close(5));
  };

  auto tmp_ser = [](const Quote& quote) {
    auto mean_ser = [&quote](const int delay) {
      return mean(quote.ts_volume(delay + 40));
    };
    return corr(ts<double>(5, mean_ser), quote.ts_low(5));
  };
  return rank(quotes.apply(cmm)) * rank(quotes.apply(tmp_ser)) * -1.0;
};


// MAX(RANK(DECAYLINEAR(DELTA(((CLOSE * 0.35)	+ (VWAP	* 0.65)), 2), 3)),
// TSRANK(DECAYLINEAR(ABS(CORR((MEAN(VOLUME,180)), CLOSE, 13)), 5), 15)) * -1)
Alpha_mfun alpha092 = [](const Quotes& qts) -> Timeseries{
  auto decay_linear1 = [](const Quote& qt) {
    return decaylinear(qt.ts<double>(3, [](const Quote& qt) { return delta(qt.ts_close(2) * 0.35 + qt.ts_vwap(2) * 0.62); }));
  };

  auto decay_linear2 = [](const Quote& qt) {
    auto mean_ser = qt.ts<double>(13, [](const Quote& qt) { return mean(qt.ts_volume(180)); });
    return decaylinear(qt.ts<double>(5, [&](const Quote& qt) { return abs(corr(mean_ser, qt.ts_close(13))); }));
  };

  auto ts_rank = [&](const Quote& qt) {
    return tsrank(qt.ts<double>(15, decay_linear2));
  };
  Timeseries part1 = rank(qts.apply(decay_linear1));
  Timeseries part2 = qts.apply(ts_rank);
  return pmax(part1, part2);
};


// SUM((OPEN >= DELAY(OPEN, 1) ? 0 : MAX(OPEN - LOW, OPEN - DELAY(OPEN, 1))), 20)
Alpha_fun alpha093 = [](const Quote& quote) -> double {
  auto tmp_ser = [&quote](const int delay) {
    double factor;
    if(quote.open(delay) >= quote.open(delay + 1))
    {
      factor = 0.0;
    }
    else
    {
      factor = std::max(quote.open(delay) - quote.low(delay), quote.open(delay) - quote.open(delay + 1));
    }
    return factor;
  };
  return sum(ts<double>(20, tmp_ser));
};


// SUM((CLOSE > DELAY(CLOSE, 1) ? VOLUME : (CLOSE < DELAY(CLOSE, 1) ? -VOLUME : 0)), 30)
Alpha_fun alpha094 = [](const Quote& quote) -> double {
  auto tmp_ser = [&quote](const int delay) {
    double factor;
    if(quote.close(delay) > quote.pclose(delay))
    {
      factor = quote.volume(delay);
    }
    else if(quote.close(delay) < quote.pclose(delay))
    {
      factor = -quote.volume(delay);
    }
    else
    {
      factor = 0.0;
    }
    return factor;
  };
  return sum(ts<double>(30, tmp_ser));
};


// STD(AMOUNT, 20)
Alpha_fun alpha095 = [](const Quote& quote) -> double {
  if(ISNA(sum(quote.ts_amount(20))))
  {
    return NA_REAL;
  }
  return stdev(quote.ts_amount(20));
};


// SMA(SMA((CLOSE - TSMIN(LOW, 9)) / (TSMAX(HIGH, 9) - TSMIN(LOW, 9)) * 100, 3, 1), 3, 1)
Alpha_fun alpha096 = [](const Quote& quote) -> double {
  auto fun = [&quote](const int delay1) {
    auto tmp_ser = [&quote, delay1](const int delay2) {
      return (quote.close(delay1 + delay2) - tsmin(quote.ts_low(9, delay1 + delay2))) /
        (tsmax(quote.ts_high(9, delay1 + delay2)) - tsmin(quote.ts_low(9, delay1 + delay2))) * 100;
    };
    return sma(ts<double>(3, tmp_ser), 1);
  };
  return sma(ts<double>(3, fun), 1);
};


// STD(VOLUME, 10)
Alpha_fun alpha097 = [](const Quote& quote) -> double {
  if(ISNA(sum(quote.ts_amount(10))))
  {
    return NA_REAL;
  }
  return stdev(quote.ts_volume(10));
};


// (((DELTA((SUM(CLOSE, 100) / 100), 100) / DELAY(CLOSE, 100)) < 0.05) || ((DELTA((SUM(CLOSE, 100) / 100), 100) /
// DELAY(CLOSE, 100)) == 0.05)) ? (-1 * (CLOSE - TSMIN(CLOSE, 100))) : (-1 * DELTA(CLOSE, 3))
Alpha_fun alpha098 = [](const Quote& quote) -> double {
  bool statement = [](const Quote& qt) {
    auto tmp = [](const Quote& qt) { return mean(qt.ts_close(100)); };
    return delta(qt.ts<double>(100, tmp)) / qt.close(100) <= 0.05;
  };

  return statement ? -(quote.close() - tsmin(quote.ts_close(100))) : -delta(quote.ts_close(3));
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
Alpha_fun alpha100 = [](const Quote& quote) -> double {
  if(ISNA(sum(quote.ts_volume(20))))
  {
    return NA_REAL;
  }
  return stdev(quote.ts_volume(20));
};


// ((RANK(CORR(CLOSE, SUM(MEAN(VOLUME,30), 37), 15)) <
//   RANK(CORR(RANK(((HIGH * 0.1) + (VWAP * 0.9))), RANK(VOLUME), 11))) * -1)
Alpha_mfun alpha101= [](const Quotes& qts) -> Timeseries{
  auto mean_ser = [](const Quote& qt) {
    return mean(qt.ts_volume(30));
  };

  auto corr1 = [&](const Quote& qt) {
    auto sum_mean_vol = [&](const Quote& qt) { return sum(qt.ts<double>(37, mean_ser)); };
    return corr(qt.ts_close(15), qt.ts<double>(15, sum_mean_vol));
  };

  auto rk_hav = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.high() * 0.1 + qt.vwap() * 0.9; }));
  };

  auto rk_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.volume(); }));
  };

  auto corr2 = [rk_hav, rk_vol](const Quotes& qts) {
    auto rk1 = qts.tsapply(11, rk_hav);
    auto rk2 = qts.tsapply(11, rk_vol);
    return apply(rk1, rk2, corr);
  };
  Timeseries rk1 = rank(qts.apply(corr1));
  Timeseries rk2 = rank(corr2(qts));
  return (rk1<rk2) * -1.0;
};


// SMA(MAX(VOLUME-DELAY(VOLUME, 1), 0), 6, 1) / SMA(ABS(VOLUME-DELAY(VOLUME, 1)), 6, 1) * 100
Alpha_fun alpha102 = [](const Quote& quote) -> double {
  auto max_ser = [](const Quote& qt) {
    return std::max(qt.volume() - qt.volume(1), 0.0);
  };

  auto abs_ser = [](const Quote& qt) {
    return std::abs(qt.volume() - qt.volume(1));
  };
  return sma(quote.ts<double>(6, max_ser), 1) / sma(quote.ts<double>(6, abs_ser), 1) * 100;
};


// ((20 - LOWDAY(LOW, 20)) / 20) * 100
Alpha_fun alpha103 = [](const Quote& quote) -> double {
  return (20 - lowday(quote.ts_low(20))) / 20 * 100;
};


// -1 * (DELTA(CORR(HIGH, VOLUME, 5), 5) * RANK(STD(CLOSE, 20)))
Alpha_mfun alpha104= [](const Quotes& qts) -> Timeseries{
  auto std = [](const Quote& qt) {
    return stdev(qt.ts_close(20));
  };

  auto del = [](const Quote& qt) {
    return delta(qt.ts<double>(5, [](const Quote& qt) { return corr(qt.ts_high(5), qt.ts_volume(5)); }));
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
Alpha_fun alpha106 = [](const Quote& quote) -> double {
  return quote.close() - quote.close(20);
};


// (-1 * RANK((OPEN - DELAY(HIGH, 1)))) * RANK((OPEN - DELAY(CLOSE, 1))) * RANK((OPEN - DELAY(LOW, 1)))
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

  auto part2 = [](const Quote& qt) {
    return corr(qt.ts_vwap(6), qt.ts<double>(6, [](const Quote& qt){ return mean(qt.ts_volume(120)); }));
  };
  Timeseries rk1 = rank(qts.apply(part1));
  Timeseries rk2 = rank(qts.apply(part2));
  return pow(rk1, rk2) * -1.0;
};


// SMA(HIGH - LOW, 10, 2) / SMA(SMA(HIGH - LOW), 10, 2)
Alpha_fun alpha109 = [](const Quote& quote) -> double {
  auto sma_ser = [&quote](const int delay) {
    return sma(quote.ts_high(10, delay) - quote.ts_low(10, delay), 2);
  };
  return sma_ser(0) / sma(ts<double>(10, sma_ser), 2);
};


// SUM(MAX(0, HIGH - DELAY(CLOSE, 1)), 20) / SUM(MAX(0, DELAY(CLOSE, 1) - LOW), 20) * 100
Alpha_fun alpha110 = [](const Quote& quote) -> double {
  auto max_ser1 = [&quote](const int delay) {
    return std::max(0.0, quote.high(delay) - quote.pclose(delay));
  };

  auto max_ser2 = [&quote](const int delay) {
    return std::max(0.0, quote.pclose(delay) - quote.low(delay));
  };

  if(ISNA(sum(ts<double>(20, max_ser2))) || sum(ts<double>(20, max_ser2)) == 0.0)
  {
    return NA_REAL;
  }
  return sum(ts<double>(20, max_ser1)) / sum(ts<double>(20, max_ser2)) * 100;
};


// SMA(VOL * ((CLOSE - LOW) - (HIGH - CLOSE)) / (HIGH - LOW), 11, 2) -
// SMA(VOL * ((CLOSE - LOW) - (HIGH - CLOSE)) / (HIGH - LOW), 4, 2)
Alpha_fun alpha111 = [](const Quote& quote) -> double {
  auto tmp_ser = [&quote](const int delay) {
    return quote.volume(delay) * ((quote.close(delay) - quote.low(delay)) - (quote.high(delay) - quote.close(delay))) /
      (quote.high(delay) - quote.low(delay));
  };
  return sma(ts<double>(11, tmp_ser), 2) - sma(ts<double>(4, tmp_ser), 2);
};


// (SUM((CLOSE - DELAY(CLOSE, 1) > 0 ? CLOSE - DELAY(CLOSE, 1) : 0), 12) -
//  SUM((CLOSE - DELAY(CLOSE, 1) < 0 ? DELAY(CLOSE, 1) - CLOSE : 0), 12)) /
// (SUM((CLOSE - DELAY(CLOSE, 1) > 0 ? CLOSE - DELAY(CLOSE, 1) : 0), 12) +
//  SUM((CLOSE - DELAY(CLOSE, 1) < 0 ? DELAY(CLOSE, 1) - CLOSE : 0), 12)) * 100
Alpha_fun alpha112 = [](const Quote& quote) -> double {
  auto tmp_ser1 = [&quote](const int delay) {
    return quote.close(delay) - quote.pclose(delay) > 0 ? quote.close(delay) - quote.pclose(delay) : 0;
  };

  auto tmp_ser2 = [&quote](const int delay) {
    return quote.close(delay) - quote.pclose(delay) < 0 ? quote.pclose(delay) - quote.close(delay) : 0;
  };
  const auto sum_tmp1_12 = sum(ts<double>(12, tmp_ser1));
  const auto sum_tmp2_12 = sum(ts<double>(12, tmp_ser2));

  if (ISNA(sum_tmp1_12 + sum_tmp2_12) || sum_tmp1_12 + sum_tmp2_12 == 0.0) {
    return NA_REAL;
  }
  return (sum_tmp1_12 - sum_tmp2_12) / (sum_tmp1_12 + sum_tmp2_12) * 100;
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
    return (qt.high(2) - qt.low(2)) / (sum(qt.ts_close(5, 2)) / 5);
  };

  auto part2 = [](const Quote& qt) {
    return (qt.high() - qt.low()) / (sum(qt.ts_close(5)) / 5) / (qt.vwap() - qt.close());
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

  auto corr2 = [&](const Quote& qt) {
    return corr(qt.ts<double>(7, tsrank1), qt.ts<double>(7, tsrank2));
  };
  return pow(rank(qts.apply(corr1)), rank(qts.apply(corr2)));
};


// REGBETA(CLOSE, SEQUENCE, 20)
Alpha_fun alpha116 = [](const Quote& quote) -> double {
  return regbeta(quote.ts_close(20), sequence(20));
};


// TSRANK(VOLUME. 32) * (1 - TSRANK(CLOSE + HIGH - LOW, 16) * (1 - TSRANK(RET, 32))
Alpha_fun alpha117 = [](const Quote& quote) -> double {
  const auto part1 = tsrank(quote.ts_volume(32));
  const auto part2 = 1 - tsrank(quote.ts_close(16) + quote.ts_high(16) - quote.ts_low(16));
  const auto part3 = 1 - tsrank(quote.ts_ret(32));
  return part1 * part2 * part3;
};


// SUM(HIGH - OPEN, 20) / SUM(OPEN - LOW, 20) * 100
Alpha_fun alpha118 = [](const Quote& quote) -> double {
  return sum(quote.ts_high(20) - quote.ts_open(20)) / sum(quote.ts_open(20) - quote.ts_low(20)) * 100;
};


// (RANK(DECAYLINEAR(CORR(VWAP, SUM(MEAN(VOLUME,5), 26), 5), 7)) -
//  RANK(DECAYLINEAR(TSRANK(MIN(CORR(RANK(OPEN), RANK(MEAN(VOLUME,15)), 21), 9), 7), 8)))
Alpha_mfun alpha119 = [](const Quotes& qts) -> Timeseries{
  auto sum_mean_vol5 = [](const Quote& qt) {
    auto tmp = qt.ts<double>(26, [](const Quote& qt) { return mean(qt.ts_volume(5)); });
    return sum(tmp);
  };

  auto cor = [&](const Quote& qt) {
    return corr(qt.ts_vwap(5), qt.ts<double>(5, sum_mean_vol5));
  };

  auto decay_linear1 = [&](const Quote& qt) {
    return decaylinear(qt.ts<double>(7, cor));
  };

  auto rk_open = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return qt.open(); }));
  };

  auto rk_mean_vol = [](const Quotes& qts) {
    return rank(qts.apply([](const Quote& qt) { return mean(qt.ts_volume(15)); }));
  };

  auto decay_linear2 = [&](const Quotes& qts) {
    auto rk1 = qts.tsapply(21, rk_open);
    auto rk2 = qts.tsapply(21, rk_mean_vol);
    auto cor = [&](const Quotes& quotes) { return apply(rk1, rk2, corr); };
    auto corr1 = qts.tsapply(9, cor);
    auto mi1 = [&](const Quotes& quotes) { return apply(corr1, tsmin); };
    auto min1 = qts.tsapply(7, mi1);
    auto ts_rank = [&](const Quotes& quotes) { return apply(min1, tsrank); };
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