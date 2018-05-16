#ifndef __GCAMCTF_TYPES__
#define __GCAMCTF_TYPES__

#include <vector>
#include <functional>

using Code = int;
using RDate = int;
using Timeseries = std::vector<double>;
class Quote;
using Alpha_fun = std::function<double(const Quote&)>;
class Quotes;
using Alpha_mfun = std::function<Timeseries(const Quotes&)>;

#endif // __GCAMCTF_TYPES__
