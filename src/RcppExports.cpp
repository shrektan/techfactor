// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "GCAMCTF_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// na_vector
Timeseries na_vector(const int length);
RcppExport SEXP _GCAMCTF_na_vector(SEXP lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type length(lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(na_vector(length));
    return rcpp_result_gen;
END_RCPP
}
// delta
double delta(const Timeseries& x);
RcppExport SEXP _GCAMCTF_delta(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(delta(x));
    return rcpp_result_gen;
END_RCPP
}
// rank
Timeseries rank(const Timeseries& x);
RcppExport SEXP _GCAMCTF_rank(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rank(x));
    return rcpp_result_gen;
END_RCPP
}
// sum
double sum(const Timeseries& x);
RcppExport SEXP _GCAMCTF_sum(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sum(x));
    return rcpp_result_gen;
END_RCPP
}
// mean
double mean(const Timeseries& x);
RcppExport SEXP _GCAMCTF_mean(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(mean(x));
    return rcpp_result_gen;
END_RCPP
}
// stdev
double stdev(const Timeseries& x);
RcppExport SEXP _GCAMCTF_stdev(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(stdev(x));
    return rcpp_result_gen;
END_RCPP
}
// tsmin
double tsmin(const Timeseries& x);
RcppExport SEXP _GCAMCTF_tsmin(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(tsmin(x));
    return rcpp_result_gen;
END_RCPP
}
// tsmax
double tsmax(const Timeseries& x);
RcppExport SEXP _GCAMCTF_tsmax(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(tsmax(x));
    return rcpp_result_gen;
END_RCPP
}
// tsrank
double tsrank(const Timeseries& x);
RcppExport SEXP _GCAMCTF_tsrank(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(tsrank(x));
    return rcpp_result_gen;
END_RCPP
}
// covariance
double covariance(const Timeseries& x, const Timeseries& y);
RcppExport SEXP _GCAMCTF_covariance(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Timeseries& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(covariance(x, y));
    return rcpp_result_gen;
END_RCPP
}
// corr
double corr(const Timeseries& x, const Timeseries& y);
RcppExport SEXP _GCAMCTF_corr(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Timeseries& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(corr(x, y));
    return rcpp_result_gen;
END_RCPP
}
// sign
double sign(const double x);
RcppExport SEXP _GCAMCTF_sign(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sign(x));
    return rcpp_result_gen;
END_RCPP
}
// sma
double sma(const Timeseries& x, const int m);
RcppExport SEXP _GCAMCTF_sma(SEXP xSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(sma(x, m));
    return rcpp_result_gen;
END_RCPP
}
// wma
double wma(const Timeseries& x);
RcppExport SEXP _GCAMCTF_wma(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(wma(x));
    return rcpp_result_gen;
END_RCPP
}
// decaylinear
double decaylinear(const Timeseries& x);
RcppExport SEXP _GCAMCTF_decaylinear(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(decaylinear(x));
    return rcpp_result_gen;
END_RCPP
}
// sequence
Timeseries sequence(const int n);
RcppExport SEXP _GCAMCTF_sequence(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(sequence(n));
    return rcpp_result_gen;
END_RCPP
}
// sumac
Timeseries sumac(const Timeseries& x);
RcppExport SEXP _GCAMCTF_sumac(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sumac(x));
    return rcpp_result_gen;
END_RCPP
}
// log
Timeseries log(const Timeseries& x);
RcppExport SEXP _GCAMCTF_log(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(log(x));
    return rcpp_result_gen;
END_RCPP
}
// abs
Timeseries abs(const Timeseries& x);
RcppExport SEXP _GCAMCTF_abs(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(abs(x));
    return rcpp_result_gen;
END_RCPP
}
// prod
double prod(const Timeseries& x);
RcppExport SEXP _GCAMCTF_prod(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(prod(x));
    return rcpp_result_gen;
END_RCPP
}
// count
double count(const std::vector<bool>& x);
RcppExport SEXP _GCAMCTF_count(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<bool>& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(count(x));
    return rcpp_result_gen;
END_RCPP
}
// regbeta
double regbeta(const Timeseries& y, const Timeseries& x);
RcppExport SEXP _GCAMCTF_regbeta(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(regbeta(y, x));
    return rcpp_result_gen;
END_RCPP
}
// regresi
double regresi(const Timeseries& y, const Timeseries& x);
RcppExport SEXP _GCAMCTF_regresi(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(regresi(y, x));
    return rcpp_result_gen;
END_RCPP
}
// filter
Timeseries filter(const Timeseries& x, const std::vector<bool>& cond);
RcppExport SEXP _GCAMCTF_filter(SEXP xSEXP, SEXP condSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::vector<bool>& >::type cond(condSEXP);
    rcpp_result_gen = Rcpp::wrap(filter(x, cond));
    return rcpp_result_gen;
END_RCPP
}
// highday
double highday(const Timeseries& x);
RcppExport SEXP _GCAMCTF_highday(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(highday(x));
    return rcpp_result_gen;
END_RCPP
}
// lowday
double lowday(const Timeseries& x);
RcppExport SEXP _GCAMCTF_lowday(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(lowday(x));
    return rcpp_result_gen;
END_RCPP
}
// assert_valid
void assert_valid(const Rcpp::newDateVector from_to);
RcppExport SEXP _GCAMCTF_assert_valid(SEXP from_toSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::newDateVector >::type from_to(from_toSEXP);
    assert_valid(from_to);
    return R_NilValue;
END_RCPP
}
// any_na
bool any_na(const Timeseries& x);
RcppExport SEXP _GCAMCTF_any_na(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(any_na(x));
    return rcpp_result_gen;
END_RCPP
}
// assert_no_na
void assert_no_na(const Timeseries& x);
RcppExport SEXP _GCAMCTF_assert_no_na(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    assert_no_na(x);
    return R_NilValue;
END_RCPP
}
// assert_valid_dates
void assert_valid_dates(const std::vector<RDate>& x, const std::string& field);
RcppExport SEXP _GCAMCTF_assert_valid_dates(SEXP xSEXP, SEXP fieldSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<RDate>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type field(fieldSEXP);
    assert_valid_dates(x, field);
    return R_NilValue;
END_RCPP
}
// assert_valid_price
void assert_valid_price(const Timeseries& x, const std::string& field);
RcppExport SEXP _GCAMCTF_assert_valid_price(SEXP xSEXP, SEXP fieldSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type field(fieldSEXP);
    assert_valid_price(x, field);
    return R_NilValue;
END_RCPP
}
// assert_valid_volume
void assert_valid_volume(const Timeseries& x, const std::string& field);
RcppExport SEXP _GCAMCTF_assert_valid_volume(SEXP xSEXP, SEXP fieldSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type field(fieldSEXP);
    assert_valid_volume(x, field);
    return R_NilValue;
END_RCPP
}
// assert_sorted
void assert_sorted(const std::vector<RDate>& x);
RcppExport SEXP _GCAMCTF_assert_sorted(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<RDate>& >::type x(xSEXP);
    assert_sorted(x);
    return R_NilValue;
END_RCPP
}
// tf_qt_tdates
Rcpp::newDateVector tf_qt_tdates(SEXP quote_ptr, const Rcpp::newDateVector from_to);
RcppExport SEXP _GCAMCTF_tf_qt_tdates(SEXP quote_ptrSEXP, SEXP from_toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type quote_ptr(quote_ptrSEXP);
    Rcpp::traits::input_parameter< const Rcpp::newDateVector >::type from_to(from_toSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_qt_tdates(quote_ptr, from_to));
    return rcpp_result_gen;
END_RCPP
}
// tf_qt_today
Rcpp::Date tf_qt_today(SEXP quote_ptr, const Rcpp::Date today);
RcppExport SEXP _GCAMCTF_tf_qt_today(SEXP quote_ptrSEXP, SEXP todaySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type quote_ptr(quote_ptrSEXP);
    Rcpp::traits::input_parameter< const Rcpp::Date >::type today(todaySEXP);
    rcpp_result_gen = Rcpp::wrap(tf_qt_today(quote_ptr, today));
    return rcpp_result_gen;
END_RCPP
}
// tf_qt_get
double tf_qt_get(SEXP quote_ptr, const Rcpp::Date today, const std::string tag, const int delay);
RcppExport SEXP _GCAMCTF_tf_qt_get(SEXP quote_ptrSEXP, SEXP todaySEXP, SEXP tagSEXP, SEXP delaySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type quote_ptr(quote_ptrSEXP);
    Rcpp::traits::input_parameter< const Rcpp::Date >::type today(todaySEXP);
    Rcpp::traits::input_parameter< const std::string >::type tag(tagSEXP);
    Rcpp::traits::input_parameter< const int >::type delay(delaySEXP);
    rcpp_result_gen = Rcpp::wrap(tf_qt_get(quote_ptr, today, tag, delay));
    return rcpp_result_gen;
END_RCPP
}
// tf_qt_ts_get
Timeseries tf_qt_ts_get(SEXP quote_ptr, const Rcpp::Date today, const std::string tag, const int n, const int delay);
RcppExport SEXP _GCAMCTF_tf_qt_ts_get(SEXP quote_ptrSEXP, SEXP todaySEXP, SEXP tagSEXP, SEXP nSEXP, SEXP delaySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type quote_ptr(quote_ptrSEXP);
    Rcpp::traits::input_parameter< const Rcpp::Date >::type today(todaySEXP);
    Rcpp::traits::input_parameter< const std::string >::type tag(tagSEXP);
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int >::type delay(delaySEXP);
    rcpp_result_gen = Rcpp::wrap(tf_qt_ts_get(quote_ptr, today, tag, n, delay));
    return rcpp_result_gen;
END_RCPP
}
// tf_ts_op
Timeseries tf_ts_op(const Timeseries& x, const Timeseries& y, const std::string op);
RcppExport SEXP _GCAMCTF_tf_ts_op(SEXP xSEXP, SEXP ySEXP, SEXP opSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Timeseries& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const std::string >::type op(opSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_ts_op(x, y, op));
    return rcpp_result_gen;
END_RCPP
}
// tf_ts_scalar_op
Timeseries tf_ts_scalar_op(const Timeseries& x, const double y, const std::string op);
RcppExport SEXP _GCAMCTF_tf_ts_scalar_op(SEXP xSEXP, SEXP ySEXP, SEXP opSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const double >::type y(ySEXP);
    Rcpp::traits::input_parameter< const std::string >::type op(opSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_ts_scalar_op(x, y, op));
    return rcpp_result_gen;
END_RCPP
}
// tf_ts
Timeseries tf_ts(SEXP quote_ptr, const Rcpp::Date today, const int n);
RcppExport SEXP _GCAMCTF_tf_ts(SEXP quote_ptrSEXP, SEXP todaySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type quote_ptr(quote_ptrSEXP);
    Rcpp::traits::input_parameter< const Rcpp::Date >::type today(todaySEXP);
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_ts(quote_ptr, today, n));
    return rcpp_result_gen;
END_RCPP
}
// tf_assert_valid_panel
void tf_assert_valid_panel(const Rcpp::List x);
RcppExport SEXP _GCAMCTF_tf_assert_valid_panel(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List >::type x(xSEXP);
    tf_assert_valid_panel(x);
    return R_NilValue;
END_RCPP
}
// tf_panel_sum
Rcpp::NumericVector tf_panel_sum(const Rcpp::List x);
RcppExport SEXP _GCAMCTF_tf_panel_sum(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_panel_sum(x));
    return rcpp_result_gen;
END_RCPP
}
// tf_assert_same_size
void tf_assert_same_size(const Timeseries& x, const Timeseries& y);
RcppExport SEXP _GCAMCTF_tf_assert_same_size(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Timeseries& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Timeseries& >::type y(ySEXP);
    tf_assert_same_size(x, y);
    return R_NilValue;
END_RCPP
}
// create_xts
Rcpp::NumericMatrix create_xts(Rcpp::NumericMatrix x_mat, Rcpp::newDateVector x_dates);
RcppExport SEXP _GCAMCTF_create_xts(SEXP x_matSEXP, SEXP x_datesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x_mat(x_matSEXP);
    Rcpp::traits::input_parameter< Rcpp::newDateVector >::type x_dates(x_datesSEXP);
    rcpp_result_gen = Rcpp::wrap(create_xts(x_mat, x_dates));
    return rcpp_result_gen;
END_RCPP
}
// tf_reg_factors
Rcpp::CharacterVector tf_reg_factors();
RcppExport SEXP _GCAMCTF_tf_reg_factors() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(tf_reg_factors());
    return rcpp_result_gen;
END_RCPP
}
// tf_quote_xptr
SEXP tf_quote_xptr(Rcpp::DataFrame qt_tbl);
RcppExport SEXP _GCAMCTF_tf_quote_xptr(SEXP qt_tblSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type qt_tbl(qt_tblSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_quote_xptr(qt_tbl));
    return rcpp_result_gen;
END_RCPP
}
// enc2utf8
Rcpp::StringVector enc2utf8(Rcpp::StringVector x, bool only_mark);
RcppExport SEXP _GCAMCTF_enc2utf8(SEXP xSEXP, SEXP only_markSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type only_mark(only_markSEXP);
    rcpp_result_gen = Rcpp::wrap(enc2utf8(x, only_mark));
    return rcpp_result_gen;
END_RCPP
}
// tf_quotes_xptr
SEXP tf_quotes_xptr(Rcpp::List qt_tbls);
RcppExport SEXP _GCAMCTF_tf_quotes_xptr(SEXP qt_tblsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type qt_tbls(qt_tblsSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_quotes_xptr(qt_tbls));
    return rcpp_result_gen;
END_RCPP
}
// assert_valid_class
void assert_valid_class(SEXP x, const std::string& classname);
RcppExport SEXP _GCAMCTF_assert_valid_class(SEXP xSEXP, SEXP classnameSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type classname(classnameSEXP);
    assert_valid_class(x, classname);
    return R_NilValue;
END_RCPP
}
// tf_qt_cal
Rcpp::NumericMatrix tf_qt_cal(SEXP qt_ptr, Rcpp::StringVector names, Rcpp::newDateVector from_to);
RcppExport SEXP _GCAMCTF_tf_qt_cal(SEXP qt_ptrSEXP, SEXP namesSEXP, SEXP from_toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type qt_ptr(qt_ptrSEXP);
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type names(namesSEXP);
    Rcpp::traits::input_parameter< Rcpp::newDateVector >::type from_to(from_toSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_qt_cal(qt_ptr, names, from_to));
    return rcpp_result_gen;
END_RCPP
}
// hardware_cores
int hardware_cores();
RcppExport SEXP _GCAMCTF_hardware_cores() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(hardware_cores());
    return rcpp_result_gen;
END_RCPP
}
// tf_qts_cal
Rcpp::NumericMatrix tf_qts_cal(SEXP qts_ptr, std::string name, Rcpp::newDateVector from_to, const int threads_no);
RcppExport SEXP _GCAMCTF_tf_qts_cal(SEXP qts_ptrSEXP, SEXP nameSEXP, SEXP from_toSEXP, SEXP threads_noSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type qts_ptr(qts_ptrSEXP);
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< Rcpp::newDateVector >::type from_to(from_toSEXP);
    Rcpp::traits::input_parameter< const int >::type threads_no(threads_noSEXP);
    rcpp_result_gen = Rcpp::wrap(tf_qts_cal(qts_ptr, name, from_to, threads_no));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_GCAMCTF_na_vector", (DL_FUNC) &_GCAMCTF_na_vector, 1},
    {"_GCAMCTF_delta", (DL_FUNC) &_GCAMCTF_delta, 1},
    {"_GCAMCTF_rank", (DL_FUNC) &_GCAMCTF_rank, 1},
    {"_GCAMCTF_sum", (DL_FUNC) &_GCAMCTF_sum, 1},
    {"_GCAMCTF_mean", (DL_FUNC) &_GCAMCTF_mean, 1},
    {"_GCAMCTF_stdev", (DL_FUNC) &_GCAMCTF_stdev, 1},
    {"_GCAMCTF_tsmin", (DL_FUNC) &_GCAMCTF_tsmin, 1},
    {"_GCAMCTF_tsmax", (DL_FUNC) &_GCAMCTF_tsmax, 1},
    {"_GCAMCTF_tsrank", (DL_FUNC) &_GCAMCTF_tsrank, 1},
    {"_GCAMCTF_covariance", (DL_FUNC) &_GCAMCTF_covariance, 2},
    {"_GCAMCTF_corr", (DL_FUNC) &_GCAMCTF_corr, 2},
    {"_GCAMCTF_sign", (DL_FUNC) &_GCAMCTF_sign, 1},
    {"_GCAMCTF_sma", (DL_FUNC) &_GCAMCTF_sma, 2},
    {"_GCAMCTF_wma", (DL_FUNC) &_GCAMCTF_wma, 1},
    {"_GCAMCTF_decaylinear", (DL_FUNC) &_GCAMCTF_decaylinear, 1},
    {"_GCAMCTF_sequence", (DL_FUNC) &_GCAMCTF_sequence, 1},
    {"_GCAMCTF_sumac", (DL_FUNC) &_GCAMCTF_sumac, 1},
    {"_GCAMCTF_log", (DL_FUNC) &_GCAMCTF_log, 1},
    {"_GCAMCTF_abs", (DL_FUNC) &_GCAMCTF_abs, 1},
    {"_GCAMCTF_prod", (DL_FUNC) &_GCAMCTF_prod, 1},
    {"_GCAMCTF_count", (DL_FUNC) &_GCAMCTF_count, 1},
    {"_GCAMCTF_regbeta", (DL_FUNC) &_GCAMCTF_regbeta, 2},
    {"_GCAMCTF_regresi", (DL_FUNC) &_GCAMCTF_regresi, 2},
    {"_GCAMCTF_filter", (DL_FUNC) &_GCAMCTF_filter, 2},
    {"_GCAMCTF_highday", (DL_FUNC) &_GCAMCTF_highday, 1},
    {"_GCAMCTF_lowday", (DL_FUNC) &_GCAMCTF_lowday, 1},
    {"_GCAMCTF_assert_valid", (DL_FUNC) &_GCAMCTF_assert_valid, 1},
    {"_GCAMCTF_any_na", (DL_FUNC) &_GCAMCTF_any_na, 1},
    {"_GCAMCTF_assert_no_na", (DL_FUNC) &_GCAMCTF_assert_no_na, 1},
    {"_GCAMCTF_assert_valid_dates", (DL_FUNC) &_GCAMCTF_assert_valid_dates, 2},
    {"_GCAMCTF_assert_valid_price", (DL_FUNC) &_GCAMCTF_assert_valid_price, 2},
    {"_GCAMCTF_assert_valid_volume", (DL_FUNC) &_GCAMCTF_assert_valid_volume, 2},
    {"_GCAMCTF_assert_sorted", (DL_FUNC) &_GCAMCTF_assert_sorted, 1},
    {"_GCAMCTF_tf_qt_tdates", (DL_FUNC) &_GCAMCTF_tf_qt_tdates, 2},
    {"_GCAMCTF_tf_qt_today", (DL_FUNC) &_GCAMCTF_tf_qt_today, 2},
    {"_GCAMCTF_tf_qt_get", (DL_FUNC) &_GCAMCTF_tf_qt_get, 4},
    {"_GCAMCTF_tf_qt_ts_get", (DL_FUNC) &_GCAMCTF_tf_qt_ts_get, 5},
    {"_GCAMCTF_tf_ts_op", (DL_FUNC) &_GCAMCTF_tf_ts_op, 3},
    {"_GCAMCTF_tf_ts_scalar_op", (DL_FUNC) &_GCAMCTF_tf_ts_scalar_op, 3},
    {"_GCAMCTF_tf_ts", (DL_FUNC) &_GCAMCTF_tf_ts, 3},
    {"_GCAMCTF_tf_assert_valid_panel", (DL_FUNC) &_GCAMCTF_tf_assert_valid_panel, 1},
    {"_GCAMCTF_tf_panel_sum", (DL_FUNC) &_GCAMCTF_tf_panel_sum, 1},
    {"_GCAMCTF_tf_assert_same_size", (DL_FUNC) &_GCAMCTF_tf_assert_same_size, 2},
    {"_GCAMCTF_create_xts", (DL_FUNC) &_GCAMCTF_create_xts, 2},
    {"_GCAMCTF_tf_reg_factors", (DL_FUNC) &_GCAMCTF_tf_reg_factors, 0},
    {"_GCAMCTF_tf_quote_xptr", (DL_FUNC) &_GCAMCTF_tf_quote_xptr, 1},
    {"_GCAMCTF_enc2utf8", (DL_FUNC) &_GCAMCTF_enc2utf8, 2},
    {"_GCAMCTF_tf_quotes_xptr", (DL_FUNC) &_GCAMCTF_tf_quotes_xptr, 1},
    {"_GCAMCTF_assert_valid_class", (DL_FUNC) &_GCAMCTF_assert_valid_class, 2},
    {"_GCAMCTF_tf_qt_cal", (DL_FUNC) &_GCAMCTF_tf_qt_cal, 3},
    {"_GCAMCTF_hardware_cores", (DL_FUNC) &_GCAMCTF_hardware_cores, 0},
    {"_GCAMCTF_tf_qts_cal", (DL_FUNC) &_GCAMCTF_tf_qts_cal, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_GCAMCTF(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
