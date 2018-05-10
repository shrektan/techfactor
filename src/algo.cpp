#include <Rcpp.h>
#include "algo.h"


inline void assert_length(const Timeseries& x, const int n)
{
  if (int(x.size()) < n) Rcpp::stop("x must have at least %d elements.", n);
}


// [[Rcpp::export("tf_na_vector")]]
Timeseries na_vector(const int length)
{
  Timeseries res(length);
  std::fill(res.begin(), res.end(), NA_REAL);
  return res;
}


// [[Rcpp::export("tf_delta")]]
Timeseries delta(const Timeseries& x)
{
  assert_length(x, 2);
  const int n_x = x.size();
  Timeseries res(n_x - 1);
  for (int i {1}; i < n_x; ++i) {
    res[i - 1] = x[i] - x[i - 1];
  }
  return res;
}


// [[Rcpp::export("tf_rank")]]
Timeseries rank(const Timeseries& x)
{
  if (any_na(x)) return na_vector(x.size());
  Timeseries sorted = x;
  std::sort(sorted.begin(), sorted.end());
  Timeseries res;
  std::transform(
    x.cbegin(), x.cend(), std::back_inserter(res),
    [&sorted](const double v) {
      auto iter = std::lower_bound(sorted.cbegin(), sorted.cend(), v);
      return std::distance(sorted.cbegin(), iter) + 1.0;
    }
  );
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
  assert_length(x, 1);
  return sum(x) / x.size();
}


// [[Rcpp::export("tf_stdev")]]
double stdev(const Timeseries& x)
{
  return std::sqrt(covariance(x, x));
}


// [[Rcpp::export("tf_tsmin")]]
double tsmin(const Timeseries& x)
{
  assert_length(x, 1);
  if (any_na(x)) return NA_REAL;
  return *std::min_element(x.cbegin(), x.cend());
}


// [[Rcpp::export("tf_tsmax")]]
double tsmax(const Timeseries& x)
{
  assert_length(x, 1);
  if (any_na(x)) return NA_REAL;
  return *std::max_element(x.cbegin(), x.cend());
}


// [[Rcpp::export("tf_tsrank")]]
double tsrank(const Timeseries& x)
{
  assert_length(x, 1);
  if (any_na(x)) return NA_REAL;
  Timeseries sorted = x;
  std::sort(sorted.begin(), sorted.end());
  auto iter = std::lower_bound(sorted.cbegin(), sorted.cend(), x[x.size() - 1]);
  return std::distance(sorted.cbegin(), iter) + 1.0;
}


// [[Rcpp::export("tf_covariance")]]
double covariance(const Timeseries& x, const Timeseries& y)
{
  assert_length(x, 2);
  if (any_na(x) || any_na(y)) return NA_REAL;
  if (x.size() != y.size()) Rcpp::stop("The size of x and y must be the same.");
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
double sign(const double x)
{
  if (Rcpp::NumericVector::is_na(x)) return NA_REAL;
  return (x > 0.0) ? 1.0 : (x < 0.0) ? -1.0 : 0.0;
}


// [[Rcpp::export("tf_sma")]]
double sma(const Timeseries& x, const int m)
{
  assert_length(x, 1); assert_length(x, m);
  if (m <= 0) Rcpp::stop("m must be positive.");
  if (any_na(x)) return NA_REAL;
  const int n = x.size();
  if (n == 1) return x[0];
  return std::accumulate(
    x.cbegin() + 1, x.cend(),
    x[0], [m, n](const double last_value, const double new_entry) {
      return last_value * (n - m) / n + new_entry * m / n;
    }
  );
}


// [[Rcpp::export("tf_wma")]]
double wma(const Timeseries& x)
{
  assert_length(x, 1);
  if (any_na(x)) return NA_REAL;
  const int n = x.size();
  Timeseries weights(n);
  int i {n};
  std::generate(
    weights.begin(), weights.end(),
    [&i]() mutable { return std::pow(0.9, --i); }
  );
  return std::inner_product(x.cbegin(), x.cend(), weights.cbegin(), 0.0);
}


// [[Rcpp::export("tf_decaylinear")]]
Timeseries decaylinear(const Timeseries& x)
{
  assert_length(x, 1);
  if (any_na(x)) return na_vector(x.size());
  const int n = x.size();
  Timeseries weights(sequence(n));
  const double sum_weights = std::accumulate(weights.cbegin(), weights.cend(), 0.0);
  return weights / sum_weights * x;
}


// [[Rcpp::export("tf_sequence")]]
Timeseries sequence(const int n)
{
  if (n <= 0) Rcpp::stop("n must be a positive integer but now is %d.", n);
  Timeseries res(n);
  std::iota(res.begin(), res.end(), 1);
  return res;
}


// [[Rcpp::export("tf_sumac")]]
Timeseries sumac(const Timeseries& x)
{
  Timeseries res(x.size());
  std::partial_sum(x.cbegin(), x.cend(), res.begin());
  return res;
}


// [[Rcpp::export("tf_log")]]
Timeseries log(const Timeseries& x)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [](double v) {
    if (Rcpp::NumericVector::is_na(v) || v <= 0.0) return NA_REAL;
    return std::log(v);
  });
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
  assert_length(x, 1);
  return std::accumulate(x.cbegin(), x.cend(), 1.0, std::multiplies<double>());
}


// [[Rcpp::export("tf_count")]]
double count(const std::vector<bool>& x) {
  return std::count(x.cbegin(), x.cend(), true);
}


// [[Rcpp::export("tf_regbeta")]]
double regbeta(const Timeseries& y, const Timeseries& x)
{
  assert_length(x, 2);
  const double cov_xy = covariance(x, y);
  const double std_x = stdev(x);
  if (std_x == 0.0) return NA_REAL;
  return cov_xy / std_x / std_x;
}


// [[Rcpp::export("tf_regresi")]]
Timeseries regresi(const Timeseries& y, const Timeseries& x)
{
  assert_length(x, 2);
  const double beta = regbeta(y, x);
  const double intercept = mean(y) - mean(x) * beta;
  Timeseries res(y.size());
  std::transform(
    y.cbegin(), y.cend(), x.cbegin(),
    res.begin(), [beta, intercept](const double v_y, const double v_x) {
      return v_y - v_x * beta - intercept;
    }
  );
  return res;
}


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
double highday(const Timeseries& x)
{
  assert_length(x, 1);
  if (any_na(x)) return NA_REAL;
  auto it = std::max_element(x.cbegin(), x.cend());
  return std::distance(it, x.cend()) - 1.0;
}


// [[Rcpp::export("tf_lowday")]]
double lowday(const Timeseries& x)
{
  assert_length(x, 1);
  if (any_na(x)) return NA_REAL;
  auto it = std::min_element(x.cbegin(), x.cend());
  return std::distance(it, x.cend()) - 1.0;
}



// [[Rcpp::export("tf_assert_valid_from_to")]]
void assert_valid(const Rcpp::newDateVector from_to)
{
  if (from_to.size() != 2) Rcpp::stop(
    "from_to must be a length 2 date vector."
  );
  if (from_to[0] > from_to[1]) Rcpp::stop(
    "%s %s",
    "The 2nd element of from_to must be larger than",
    "or equal to the 1st."
  );
}


// [[Rcpp::export("tf_assert_same_size")]]
void assert_same_size(const Timeseries& x, const Timeseries& y)
{
  if (x.size() != y.size()) Rcpp::stop(
    "x and y must have the same size."
  );
}


// [[Rcpp::export("tf_any_na")]]
bool any_na(const Timeseries& x)
{
  return std::any_of(x.cbegin(), x.cend(), Rcpp::NumericVector::is_na);
}


// [[Rcpp::export("tf_assert_no_na")]]
void assert_no_na(const Timeseries& x)
{
  if (any_na(x)) Rcpp::stop("x mustn't contain NA.");
}