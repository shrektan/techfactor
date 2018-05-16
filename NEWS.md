# GCAMCTF 0.0.8

* Added a `NEWS.md` file to track changes to the package.
* Set up CI and code coverage.
* Implemented basic algo and quote classes.
* Ready to to implement the factor definitions.
* Added the documentation for exported functions.
* Rename `tf_factor()` to `tf_cal()` to be not confusing with `tf_registered_factors()`.
* Refactor the cpp file structure.
* `delta()` gains an additional param n.
* Rename `Quotes` to `Quote` because we are going to introduce a new type => a vector of `Quote` which will be named as `Quotes`.
* Add `Quotes` and R interface to process panel based indicators.