#include <Rcpp.h>
#include "algo.h"

namespace alpha_impl
{

Alpha_fun alpha149 = [](const Quote& quote) -> double {
  const auto dr = quote.ts_close(252) / quote.ts_close(252, 1) - 1.0;
  const auto bmk_dr = quote.ts_bmk_close(252) / quote.ts_bmk_close(252, 1) - 1.0;
  const auto x = filter(dr, bmk_dr < 0.0);
  const auto y = filter(bmk_dr, bmk_dr < 0.0);
  if (x.size() < 2) return NA_REAL;
  return regbeta(x, y);
};

}
