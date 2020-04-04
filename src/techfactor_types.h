#ifndef __TECHFACTOR_TYPES__
#define __TECHFACTOR_TYPES__

#include <vector>
#include <functional>

using Code = int;
using RDate = int;
using Timeseries = std::vector<double>;
using Panel = std::vector<std::vector<double>>;
class Quote;
using Alpha_fun = std::function<double(const Quote&)>;
class Quotes;
using Alpha_mfun = std::function<Timeseries(const Quotes&)>;

#endif // __TECHFACTOR_TYPES__
