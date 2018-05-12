
<!-- README.md is generated from README.Rmd. Please edit that file -->
GCAMCTF
=======

[![wercker status](https://app.wercker.com/status/0a35da2d046e40b24a2854439fc88e0f/s/master "wercker status")](https://app.wercker.com/project/byKey/0a35da2d046e40b24a2854439fc88e0f) [![codecov](https://codecov.io/gh/GCAMC/GCAMCTF/branch/master/graph/badge.svg?token=ck4uoArnhK)](https://codecov.io/gh/GCAMC/GCAMCTF)

The goal of GCAMCTF is to provide tools that calculate technical factors efficiently.

Example
-------

``` r
(Sys.time())
#> [1] "2018-05-12 12:09:13 CST"
library(GCAMCTF)
data(tf_quote)
qt <- tf_quotes_xptr(tf_quote)
factors <- tf_reg_factors()
str(factors)
#>  chr [1:6] "alpha001" "alpha003" "alpha005" "alpha014" "alpha053" ...
(factor <- factors[1])
#> [1] "alpha001"
(from_to <- range(tail(tf_quote$DATE)))
#> [1] "2018-04-26" "2018-05-07"
tf_cal(qt, factor, from_to)
#>         DATE      VALUE
#> 1 2018-04-26 -0.7714286
#> 2 2018-04-27 -0.8285714
#> 3 2018-05-02 -0.8857143
#> 4 2018-05-03 -0.9428571
#> 5 2018-05-04 -0.9428571
#> 6 2018-05-07 -1.0000000
```

session info
------------

``` r
sessioninfo::session_info()
#> ─ Session info ──────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 3.4.3 (2017-11-30)
#>  os       macOS High Sierra 10.13.5   
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  tz       Asia/Shanghai               
#>  date     2018-05-12                  
#> 
#> ─ Packages ──────────────────────────────────────────────────────────────
#>  package      * version date       source        
#>  anytime        0.3.0   2017-06-05 CRAN (R 3.4.0)
#>  backports      1.1.2   2017-12-13 CRAN (R 3.4.3)
#>  clisymbols     1.2.0   2017-05-21 CRAN (R 3.4.0)
#>  data.table     1.11.2  2018-05-08 CRAN (R 3.4.3)
#>  digest         0.6.15  2018-01-28 CRAN (R 3.4.3)
#>  evaluate       0.10.1  2017-06-24 CRAN (R 3.4.1)
#>  GCAMCTF      * 0.0.4   2018-05-12 local         
#>  htmltools      0.3.6   2017-04-28 CRAN (R 3.4.0)
#>  knitr          1.20    2018-02-20 CRAN (R 3.4.3)
#>  magrittr       1.5     2014-11-22 CRAN (R 3.4.0)
#>  RApiDatetime   0.0.3   2017-04-02 CRAN (R 3.4.0)
#>  Rcpp           0.12.16 2018-03-13 CRAN (R 3.4.4)
#>  rmarkdown      1.9     2018-03-01 CRAN (R 3.4.3)
#>  rprojroot      1.3-2   2018-01-03 CRAN (R 3.4.2)
#>  sessioninfo    1.0.0   2017-06-21 cran (@1.0.0) 
#>  stringi        1.2.2   2018-05-02 CRAN (R 3.4.3)
#>  stringr        1.3.0   2018-02-19 CRAN (R 3.4.3)
#>  withr          2.1.2   2018-03-15 CRAN (R 3.4.3)
#>  yaml           2.1.18  2018-03-08 CRAN (R 3.4.4)
```
