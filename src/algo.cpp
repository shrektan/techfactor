#include <Rcpp.h>
#include "algo.h"


Timeseries delta(const Timeseries& x)
{
  const int n_x = x.size();
  Timeseries res(n_x);
  if (n_x == 1) {
    res[0] = NA_REAL;
    return res;
  }
  for (int i {1}; i < n_x; ++i) {
    res[i] = x[i] - x[i - 1];
  }
  return res;
}


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
double sum(const Timeseries& x)
{
  return std::accumulate(x.cbegin(), x.cend(), 0.0);
}


double mean(const Timeseries& x)
{
  if (x.size() == 0) return NA_REAL;
  return sum(x) / x.size();
}

double stdev(const Timeseries& x)
{
  if (x.size() <= 1) return NA_REAL;
  const double avg = mean(x);
  return std::accumulate(x.cbegin(), x.cend(), 0.0, [avg](const double v) {
    return std::pow(v - avg, 2.0);
  }) / (x.size() - 1.0);
}

double tsmin(const Timeseries& x)
{
  return *std::min(x.cbegin(), x.cend());
}

double tsmax(const Timeseries& x)
{
  return *std::max(x.begin(), x.end());
}
int tsrank(const Timeseries& x)
{
  Timeseries sorted = x;
  std::sort(sorted.begin(), sorted.end());
  const int n_x = x.size();
  auto iter = std::lower_bound(sorted.cbegin(), sorted.cend(), x[x.size() - 1]);
  return std::distance(x.cbegin(), iter);
}

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
double corr(const Timeseries& x, const Timeseries& y)
{
  const double cov = covariance(x, y);
  const double deno = stdev(x) * stdev(y);
  if (deno <= 0.0) return NA_REAL;
  return cov / deno;
}

int sign(const double x)
{
  return (x > 0.0) ? 1 : (x < 0.0) ? -1 : 0;
}
double sma(const Timeseries& x, const int n, const int m);
double wma(const Timeseries& x);
Timeseries sequence(const int n)
{
  if (n <= 0) Rcpp::stop("n must be a positive integer but now is %d.", n);
  Timeseries res(n);
  for (int i = 0; i < n; ++i) res[i] = i + 1.0;
  return res;
}
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
Timeseries log(const Timeseries& x)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [](double v) { return std::log(v); });
  return res;
}
Timeseries abs(const Timeseries& x)
{
  Timeseries res(x.size());
  std::transform(x.cbegin(), x.cend(), res.begin(), [](double v) { return std::abs(v); });
  return res;
}

double prod(const Timeseries& x)
{
  return std::accumulate(x.cbegin(), x.cend(), 1.0, [](double v1, double v2) {
    return v1 * v2;
  });
}
int count(const std::vector<bool>& x) {
  return std::accumulate(x.cbegin(), x.cend(), 0);
}
double regbeta(const Timeseries& x, const Timeseries& y);
double regresi(const Timeseries& x, const Timeseries& y);
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
int highday(const Timeseries& x)
{
  auto it = std::max_element(x.cbegin(), x.cend());
  return std::distance(it, x.cend());
}
int lowday(const Timeseries& x)
{
  auto it = std::min_element(x.cbegin(), x.cend());
  return std::distance(it, x.cend());
}




Timeseries close(const Quotes& quotes, const int n, const int delay = 0);
Timeseries open(const Quotes& quotes, const int n, const int delay = 0);
Timeseries high(const Quotes& quotes, const int n, const int delay = 0);
Timeseries low(const Quotes& quotes, const int n, const int delay = 0);
Timeseries prev_close(const Quotes& quotes, const int n, const int delay = 0);
Timeseries volume(const Quotes& quotes, const int n, const int delay = 0);
Timeseries amount(const Quotes& quotes, const int n, const int delay = 0);
Timeseries bmk_close(const Quotes& quotes, const int n, const int delay = 0);
Timeseries bmk_open(const Quotes& quotes, const int n, const int delay = 0);
Timeseries operator+(const Timeseries& x, const Timeseries& y);
Timeseries operator-(const Timeseries& x, const Timeseries& y);
Timeseries operator*(const Timeseries& x, const Timeseries& y);
Timeseries operator/(const Timeseries& x, const Timeseries& y);
Timeseries operator+(const Timeseries& x, const double y);
Timeseries operator-(const Timeseries& x, const double y);
Timeseries operator*(const Timeseries& x, const double y);
Timeseries operator/(const Timeseries& x, const double y);
std::vector<bool> operator>(const Timeseries& x, const double y);
std::vector<bool> operator<(const Timeseries& x, const double y);

