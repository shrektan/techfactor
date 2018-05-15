/*
 * Design:
 *
 * - the date sequences here must be contious trading date sequence, we also have a converter
 *   so that locate a special date is fast and easy
 * - the option to use single pass algo to boost the performance
 *   __UPDATE__: difficult to do it generally and may not neccessary since many calculations
 *   are very small except for the regression stuff.
 * - any NA in the input will lead to NA in the output
 * - support concurrency for different codes. It means we can calculate different codes at the
 *   same time. However, you can't calculate the same codes for different date parrellely.
 */


#include <Rcpp.h>
#include "algo.h"

extern std::map<std::string, Alpha_fun&> tf_caculators;
extern std::map<std::string, Alpha_mfun&> tf_mcaculators;


// [[Rcpp::export]]
Rcpp::NumericMatrix create_xts(Rcpp::NumericMatrix x_mat, Rcpp::newDateVector x_dates)
{
  Rcpp::NumericVector dates = Rcpp::clone(x_dates) * 86400;
  dates.attr("tzone") = "UTC";
  dates.attr("tclass") = "Date";

  Rcpp::NumericMatrix mat = Rcpp::clone(x_mat);
  if (!Rf_isNull(mat.attr("dimnames"))) {
    Rcpp::List dimnames = mat.attr("dimnames");
    if (dimnames.size() != 2) Rcpp::stop(
      "weird to find dimnames size is %d (expect 2)."
    );
    dimnames[0] = R_NilValue;
  }
  mat.attr("index") = dates;
  mat.attr("class") = Rcpp::CharacterVector::create("xts", "zoo");
  mat.attr(".indexCLASS") = "Date";
  mat.attr("tclass") = "Date";
  mat.attr(".indexTZ") = "UTC";
  mat.attr("tzone") = "UTC";
  return mat;
}


//' The registered factor names
//'
//' Return all the factor names that are registered in the C++ routine.
//'
//' @export
// [[Rcpp::export]]
Rcpp::CharacterVector tf_reg_factors()
{
  using namespace Rcpp;
  std::vector<std::string> normal;
  for (auto& elem : tf_caculators) normal.push_back(elem.first);
  std::vector<std::string> panel;
  for (auto& elem : tf_mcaculators) panel.push_back(elem.first);
  std::vector<std::string> all;
  std::set_union(
    normal.cbegin(), normal.cend(),
    panel.cbegin(), panel.cend(),
    std::back_inserter(all)
  );
  Rcpp::CharacterVector res = Rcpp::wrap(all);
  res.attr("normal") = Rcpp::wrap(normal);
  res.attr("panel") = Rcpp::wrap(panel);
  return res;
}


//' The quote external pointer
//'
//' @param qt_tbl A `data.frame` contains the daily price info
//'   of a security. It must contain a column `DATE` and 10 numeric
//'   columns `PCLOSE`, `OPEN`, `HIGH`, `LOW`, `CLOSE`, `VWAP`,
//'   `VOLUME`, `AMOUNT`, `BMK_CLOSE`, `BMK_OPEN`. Moreover, the
//'   column `DATE` must be in ascending order.
//'
//' @return An R external pointer than can hold a `Quote` object.
//' @export
// [[Rcpp::export]]
SEXP tf_quote_xptr(Rcpp::DataFrame qt_tbl)
{
  Quote* ptr = new Quote {qt_tbl};
  auto res = Rcpp::XPtr<Quote>(ptr, true);
  res.attr("class") = Rcpp::StringVector::create("tf_quote_xptr");
  return res;
}


//' @rdname tf_quote_xptr
//' @export
// [[Rcpp::export]]
SEXP tf_quotes_xptr(Rcpp::List qt_tbls)
{
  Quotes* ptr = new Quotes {qt_tbls};
  auto res = Rcpp::XPtr<Quotes>(ptr, true);
  res.attr("class") = Rcpp::StringVector::create("tf_quotes_xptr");
  return res;
}


template<typename T>
void asset_valid(Rcpp::XPtr<T> x, const std::string& classname)
{
  const std::string msg = "The class of xptr must be " + classname;
  if (Rf_isNull(x.attr("class"))) {
    Rcpp::stop(msg);
  } else {
    Rcpp::StringVector classes = x.attr("class");
    const bool existed =
      std::find(classes.cbegin(), classes.cend(), Rcpp::String(classname)) ==
      classes.cend();
    if (!existed) {
      Rcpp::stop(msg);
    }
  }
}



//' Calculate the TF for single security
//'
//' @param qt_ptr An R external pointer generated by [tf_quote_xptr()].
//' @param names A character vector in which all the elements are registered.
//' @param from_to A length two `Date` vector whose first element must be
//'   smaller than or equal to the first.
//'
//' @return An `xts` object in which the columns are the factors' value.
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix tf_qt_cal(SEXP qt_ptr, Rcpp::StringVector names, Rcpp::newDateVector from_to)
{
  using namespace Rcpp;
  assert_valid(from_to);
  XPtr<Quote> qt_xptr {qt_ptr};
  asset_valid(qt_xptr, "tf_quote_xptr");
  auto& qt = *qt_xptr;

  const auto dates = qt.tdates(from_to);
  NumericMatrix res(dates.size(), names.size());

  auto res_iter = res.begin();
  for (auto c_name : names) {
    const std::string name {c_name};
    if (tf_caculators.count(name)) {
      auto calculator = tf_caculators.at(name);
      std::transform(
        dates.cbegin(), dates.cend(), res_iter,
        [&qt, &calculator] (const RDate date) {
          qt.set(date);
          return calculator(qt);
        }
      );
      std::advance(res_iter, dates.size());
    }
    else {
      if (tf_mcaculators.count(name)) {
        stop("factor %s can only be used in tf_mcal()", name);
      } else {
        stop("factor %s must be defined before using.", name);
      }
    }
  }

  newDateVector r_dates(dates.size());
  std::copy(dates.cbegin(), dates.cend(), r_dates.begin());
  res.attr("dimnames") = Rcpp::List::create(R_NilValue, names);
  return create_xts(res, r_dates);
}


//' Calculate the TF for a security pool
//'
//' @param qts_ptr An R external pointer generated by [tf_quotes_xptr()].
//' @param name a registered factor name
//' @inheritParams tf_qt_cal
//' @return An `xts` object in which the columns are the factor value for
//'   each security in the pool.
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix tf_qts_cal(SEXP qts_ptr, std::string name, Rcpp::newDateVector from_to)
{
  using namespace Rcpp;
  assert_valid(from_to);
  XPtr<Quotes> qts_xptr {qts_ptr};
  asset_valid(qts_xptr, "tf_quotes_xptr");
  auto& qts = *qts_xptr;
  const auto dates = qts.tdates(from_to);
  NumericMatrix res(qts.size(), dates.size());
  auto res_iter = res.begin();
  if (tf_mcaculators.count(name)) {
    auto mcalculator = tf_mcaculators.at(name);
    for (const auto date : dates) {
      qts.set(date);
      const auto fv = mcalculator(qts);
      std::copy(fv.cbegin(), fv.cend(), res_iter);
      std::advance(res_iter, fv.size());
    }
  }
  else if (tf_caculators.count(name)) {
    auto calculator = tf_caculators.at(name);
    auto fun = [calculator] (const Quote& qt) {
      return calculator(qt);
    };
    for (const auto date : dates) {
      qts.set(date);
      const auto fv = qts.apply(fun);
      std::copy(fv.cbegin(), fv.cend(), res_iter);
      std::advance(res_iter, fv.size());
    }
  }
  else {
    stop("factor %s must be defined before using.", name);
  }

  return res;
  res = Rcpp::transpose(res);
  newDateVector r_dates(dates.size());
  std::copy(dates.cbegin(), dates.cend(), r_dates.begin());
  res.attr("dimnames") = Rcpp::List::create(R_NilValue, Rcpp::wrap(qts.names()));
  return create_xts(res, r_dates);
}