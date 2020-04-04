
<!-- README.md is generated from README.Rmd. Please edit that file -->

# techfactor

[![R build
status](https://github.com/shrektan/techfactor/workflows/R-CMD-check/badge.svg)](https://github.com/shrektan/techfactor/actions)
[![codecov](https://codecov.io/gh/shrektan/techfactor/branch/master/graph/badge.svg)](https://codecov.io/gh/shrektan/techfactor)

The goal of techfactor is to provide tools that calculate technical
factors efficiently.

## Example

``` r
(Sys.time())
#> [1] "2020-04-04 17:42:49 CST"
library(techfactor)
data(tf_quote)
qt <- tf_quote_xptr(tf_quote)
factors <- tf_reg_factors()
str(factors)
#>  chr [1:191] "alpha001" "alpha002" "alpha003" "alpha004" "alpha005" ...
#>  - attr(*, "normal")= chr [1:128] "alpha002" "alpha003" "alpha004" "alpha005" ...
#>  - attr(*, "panel")= chr [1:63] "alpha001" "alpha006" "alpha007" "alpha008" ...
(factor <- attr(factors, "normal")[1])
#> [1] "alpha002"
(from_to <- range(tail(tf_quote$DATE)))
#> [1] "2018-04-26" "2018-05-07"
tf_qt_cal(qt, factor, from_to)
#>             alpha002
#> 2018-04-26  0.228474
#> 2018-04-27 -1.238390
#> 2018-05-02  1.376597
#> 2018-05-03 -1.302913
#> 2018-05-04  1.133333
#> 2018-05-07 -1.404219
```

## session info

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 3.6.2 (2019-12-12)
#>  os       macOS Catalina 10.15.3      
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language en                          
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Asia/Shanghai               
#>  date     2020-04-04                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version    date       lib source                            
#>  anytime       0.3.7      2020-01-20 [1] CRAN (R 3.6.0)                    
#>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 3.6.0)                    
#>  cli           2.0.2      2020-02-28 [1] CRAN (R 3.6.0)                    
#>  crayon        1.3.4      2017-09-16 [1] CRAN (R 3.6.0)                    
#>  data.table    1.12.9     2020-03-29 [1] local                             
#>  digest        0.6.25     2020-02-23 [1] CRAN (R 3.6.0)                    
#>  evaluate      0.14       2019-05-28 [1] CRAN (R 3.6.0)                    
#>  fansi         0.4.1      2020-01-08 [1] CRAN (R 3.6.0)                    
#>  glue          1.3.2      2020-03-12 [1] CRAN (R 3.6.0)                    
#>  htmltools     0.4.0.9002 2020-01-27 [1] Github (rstudio/htmltools@e07546c)
#>  knitr         1.28.2     2020-03-11 [1] local                             
#>  lattice       0.20-38    2018-11-04 [1] CRAN (R 3.6.2)                    
#>  magrittr      1.5        2014-11-22 [1] CRAN (R 3.6.0)                    
#>  Rcpp          1.0.4.5    2020-04-03 [1] local                             
#>  rlang         0.4.5      2020-03-01 [1] CRAN (R 3.6.0)                    
#>  rmarkdown     2.1        2020-01-20 [1] CRAN (R 3.6.0)                    
#>  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 3.6.0)                    
#>  stringi       1.4.6      2020-02-17 [1] CRAN (R 3.6.0)                    
#>  stringr       1.4.0      2019-02-10 [1] CRAN (R 3.6.0)                    
#>  techfactor  * 0.1.1      2020-04-04 [1] local                             
#>  withr         2.1.2      2018-03-15 [1] CRAN (R 3.6.0)                    
#>  xfun          0.12       2020-01-13 [1] CRAN (R 3.6.0)                    
#>  xts           0.12-0     2020-01-19 [1] CRAN (R 3.6.0)                    
#>  yaml          2.2.1      2020-02-01 [1] CRAN (R 3.6.2)                    
#>  zoo           1.8-7      2020-01-10 [1] CRAN (R 3.6.0)                    
#> 
#> [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
```
