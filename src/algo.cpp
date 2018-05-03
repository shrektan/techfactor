#include <Rcpp.h>
#include "algo.h"


// [[Rcpp::export("tf_delta")]]
Timeseries delta(const Timeseries& x)
{
  const int n_x = x.size();
  if (n_x == 2) Rcpp::stop("x must have at least two elements.");
  Timeseries res(n_x - 1);
  for (int i {1}; i < n_x; ++i) {
    res[i - 1] = x[i] - x[i - 1];
  }
  return res;
}


// [[Rcpp::export("tf_rank")]]
Timeseries rank(const Timeseries& x)
{
  Timeseries sorted = x;
  std::sort(sorted.begin(), sorted.end());
  const int n_x = x.size();
  Timeseries res(n_x);
  for (int i {0}; i < n_x; ++i) {
    auto iter = std::lower_bound(sorted.cbegin(), sorted.cend(), x[i]);
    res[i] = std::distance(x.cbegin(), iter);
  }
  return res;
}


// [[Rcpp::export("tf_sum")]]
double sum(const Timeseries& x)
{
  return std::accumulate(x.cbegin(), x.cend(), 0.0);
}


// [[Rcpp::export("tf_mean")]]
double mean(const Timeseries& x)
{
  if (x.size() == 0) return NA_REAL;
  return sum(x) / x.size();
}


// [[Rcpp::export("tf_stdev")]]
double stdev(const Timeseries& x)
{
  if (x.size() <= 1) return NA_REAL;
  const double avg = mean(x);
  return std::accumulate(x.cbegin(), x.cend(), 0.0, [avg](const double v) {
    return std::pow(v - avg, 2.0);
  }) / (x.size() - 1.0);
}


// [[Rcpp::export("tf_tsmin")]]
double tsmin(const Timeseries& x)
{
  return *std::min(x.cbegin(), x.cend());
}


// [[Rcpp::export("tf_tsmax")]]
double tsmax(const Timeseries& x)
{
  return *std::max(x.begin(), x.end());
}


// [[Rcpp::export("tf_tsrank")]]
int tsrank(const Timeseries& x)
{
  Timeseries sorted = x;
  std::sort(sorted.begin(), sorted.end());
  const int n_x = x.size();
  auto iter = std::lower_bound(sorted.cbegin(), sorted.cend(), x[x.size() - 1]);
  return std::distance(x.cbegin(), iter);
}


// [[Rcpp::export("tf_covariance")]]
double covariance(const Timeseries& x, const Timeseries& y)
{
  if (x.size() != y.size()) Rcpp::stop("The size of x and y must be the same.");
  if (x.size() <= 1) return NA_REAL;
  const double avg_x = mean(x);
  const double avg_y = mean(y);
  return std::inner_product(
    x.cbegin(), x.cend(), y.cbegin(), 0.0,
    std::plus<double>(), [avg_x, avg_y](const double v_x, const double v_y) {
      return (v_x - avg_x) * (v_y - avg_y);
    }
  ) / (x.size() - 1.0);
}


// [[Rcpp::export("tf_corr")]]
double corr(const Timeseries& x, const Timeseries& y)
{
  const double cov = covariance(x, y);
  const double deno = stdev(x) * stdev(y);
  if (deno <= 0.0) return NA_REAL;
  return cov / deno;
}


// [[Rcpp::export("tf_sign")]]
int sign(const double x)
{
  return (x > 0.0) ? 1 : (x < 0.0) ? -1 : 0;
}


// [[Rcpp::export("tf_sma")]]
double sma(const Timeseries& x, const int n, const int m);


// [[Rcpp::export("tf_wma")]]
double wma(const Timeseries& x);


// [[Rcpp::export("tf_decaylinear")]]
Timeseries decaylinear(const Timeseries& x, const int days);


// [[Rcpp::export("tf_sequence")]]
Timeseries sequence(const int n)
{
  if (n <= 0) Rcpp::stop("n must be a positive integer but now is %d.", n);
  Timeseries res(n);
  for (int i = 0; i < n; ++i) res[i] = i + 1.0;
  return res;
}


// [[Rcpp::export("tf_sumac")]]
Timeseries sumac(const Timeseries& x)
{
  const int n_x = x.size();
  Timeseries res(n_x);
  for (int i = 0; i < n_x; ++i)
  {
    if (i == 0) {
      res[i] = x[i];
    } else {
      res[i] = res[i - 1] + x[i];
    }
  }
  return res;
}


// [[Rcpp::export("tf_log")]]
Timeseries log(const Timeseries& x)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [](double v) { return std::log(v); });
  return res;
}


// [[Rcpp::export("tf_abs")]]
Timeseries abs(const Timeseries& x)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [](double v) { return std::abs(v); });
  return res;
}


// [[Rcpp::export("tf_prod")]]
double prod(const Timeseries& x)
{
  return std::accumulate(x.cbegin(), x.cend(), 1.0, [](double v1, double v2) {
    return v1 * v2;
  });
}


// [[Rcpp::export("tf_count")]]
int count(const std::vector<bool>& x) {
  return std::accumulate(x.cbegin(), x.cend(), 0);
}


// [[Rcpp::export("tf_regbeta")]]
double regbeta(const Timeseries& x, const Timeseries& y);


// [[Rcpp::export("tf_regresi")]]
double regresi(const Timeseries& x, const Timeseries& y);


// [[Rcpp::export("tf_filter")]]
Timeseries filter(const Timeseries& x, const std::vector<bool>& cond)
{
  if (x.size() != cond.size()) Rcpp::stop("The length of x and cond must equal.");
  Timeseries res;
  const int n_x = x.size();
  for (int i {0}; i < n_x; ++i) {
    if (cond[i]) res.push_back(x[i]);
  }
  return res;
}


// [[Rcpp::export("tf_highday")]]
int highday(const Timeseries& x)
{
  auto it = std::max_element(x.cbegin(), x.cend());
  return std::distance(it, x.cend());
}


// [[Rcpp::export("tf_lowday")]]
int lowday(const Timeseries& x)
{
  auto it = std::min_element(x.cbegin(), x.cend());
  return std::distance(it, x.cend());
}



// [[Rcpp::export("tf_assert_valid_from_to")]]
void assert_valid(const Rcpp::newDateVector from_to)
{
  if (from_to.size() != 2) Rcpp::stop(
    "from_to should be a length 2 date vector."
  );
  if (from_to[0] > from_to[1]) Rcpp::stop(
    "%s %s",
    "The second element of from_to should be larger than",
    "or equal to the first."
  );
}


// [[Rcpp::export("tf_assert_same_size")]]
void assert_same_size(const Timeseries& x, const Timeseries& y)
{
  if (x.size() != y.size()) Rcpp::stop(
    "x and y must have the same size."
  );
}