
<!-- README.md is generated from README.Rmd. Please edit that file -->
GCAMCTF
=======

[![wercker status](https://app.wercker.com/status/0a35da2d046e40b24a2854439fc88e0f/s/master "wercker status")](https://app.wercker.com/project/byKey/0a35da2d046e40b24a2854439fc88e0f) [![codecov](https://codecov.io/gh/GCAMC/GCAMCTF/branch/master/graph/badge.svg?token=ck4uoArnhK)](https://codecov.io/gh/GCAMC/GCAMCTF)

The goal of GCAMCTF is to provide tools that calculate technical factors efficiently.

Example
-------

``` r
library(GCAMCTF)
data(tf_quote)
qt <- tf_quotes_xptr(tf_quote)
factors <- tf_reg_factors()
str(factors)
#>  chr [1:6] "alpha001" "alpha003" "alpha005" "alpha014" "alpha053" ...
(factor <- factors[2])
#> [1] "alpha003"
(from_to <- range(tail(tf_quote$DATE)))
#> [1] "2018-04-26" "2018-05-07"
tf_factor(qt, factor, from_to)
#>         DATE VALUE
#> 1 2018-04-26     1
#> 2 2018-04-27     1
#> 3 2018-05-02     1
#> 4 2018-05-03     1
#> 5 2018-05-04     1
#> 6 2018-05-07    -1
```
