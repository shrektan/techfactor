# techfactor 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Set up CI and code coverage.
* Implemented basic algo and quote classes.
* Ready to to implement the factor definitions.
* Added the documentation for exported functions.
* Renamed `tf_factor()` to `tf_cal()` to be not confusing with `tf_registered_factors()`.
* Refactored the cpp file structure.
* `delta()` gains an additional param n.
* Renamed `Quotes` to `Quote` because we are going to introduce a new type => a vector of `Quote` which will be named as `Quotes`.
* Added `Quotes` and an R interface to process panel based indicators.
* Eliminated the need for numerous `delay` in alpha definition functions by introducing the proxy class.
* Added the `Quotes.tsapply()`, `Panel`, `assert_valid(Panel)` and `Panel apply()` functions.
* `delta()` returns a double value that equals to `x[k] - x[0]`.
* Reimplemented `rank()` to allow `NA` (rather than return a `NA` vector even when there's only one `NA`) because the function is mainly used for cross-sectional data, where `NA` is common. In other words, the implementation now equals to `data.table::frank(x, ties.method = "min", na.last = "keep")`.
* `Quote_raw()` constructor now checks the validity of price, volume and dates so that we can be sure that there's no zero or negative price, negative volume or NA date in the dataset.
* `tf_qts_cal()` now supports non-ASCII names => all the non-UTF8 names will be converted to UTF-8 in the cpp level and return with UTF-8 marked strings.
* Implemented the concurrency calculating.
* Implemented all the 191 alpha factors.
* Added `near()` to do the zero comparision.

