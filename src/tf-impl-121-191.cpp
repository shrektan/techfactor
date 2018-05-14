#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

Alpha_fun alpha149 = [](const Quotes& quotes) -> double {
  const auto dr = quotes.ts_close(252) / quotes.ts_close(252, 1) - 1.0;
  const auto bmk_dr = quotes.ts_bmk_close(252) / quotes.ts_bmk_close(252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  return regbeta(x, y);
};

}