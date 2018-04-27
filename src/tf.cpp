// Design:
// * the date sequences here must be contious trading date sequence, we also have a converter
//   so that locate a special date is fast and easy
// * the option to use single pass algo to boost the performance
// * any NA in the input will lead to NA in the output
// *

#include <map>
#include <Rcpp.h>


enum class Quote_tag {
  prev_close, open, high, low, close, vwap, volume, amount
};

using Code = int;
using Date = int;
using Index = std::map<Date, int>;
using Quote_elem = std::map<Quote_tag, std::vector<double>>;
using Quote_ts = std::vector<double>;

class TD_converter {
public:
  TD_converter() = default;
  TD_converter(Rcpp::newDateVector x);
  const int td_code(const Date date);
private:
  std::map<Date, int> dates_;
};



class Quote_query
{
public:
  Quote_query() = default;
  Quote_query(const int code, const int date) :
    code_ {code}, date_ {date} { }
private:
  int code_ {-1};
  int date_ {-1};
};



class Quote_raw
{
public:
  Quote_raw() = default;
  Quote_ts& ref;
  int start_ {0};
  int end_ {0};
};


// implied that the date sequence here must be a continous trading date sequence
class Quote_pool
{
public:
  Quote_pool() = default;
  const std::vector<double>& raw(const Code code, const Quote_tag tag);

private:
  std::map<Code, std::pair<Index, Quote_elem>> pool_;
};


double ret(const Quote_raw& raw);
double ret(const Quote_raw& raw, const double pv);

