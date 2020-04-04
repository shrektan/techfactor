
<!-- README.md is generated from README.Rmd. Please edit that file -->

# techfactor

[![R build
status](https://github.com/shrektan/techfactor/workflows/R-CMD-check/badge.svg)](https://github.com/shrektan/techfactor/actions)
[![codecov](https://codecov.io/gh/shrektan/techfactor/branch/master/graph/badge.svg)](https://codecov.io/gh/shrektan/techfactor)

The goal of techfactor is to provide tools that calculate technical
factors efficiently.

## Example

``` r
library(techfactor)
data(tf_quote)
head(tf_quote)
#>          DATE PCLOSE  OPEN  HIGH   LOW CLOSE     VWAP   VOLUME     AMOUNT
#> 1: 2018-01-02  31.06 31.45 32.99 31.45 32.56 32.46114 68343350 2218502767
#> 2: 2018-01-03  32.56 32.50 33.78 32.23 32.33 32.93164 64687020 2130249691
#> 3: 2018-01-04  32.33 32.76 33.53 32.10 33.12 32.89830 52908580 1740602533
#> 4: 2018-01-05  33.12 32.98 35.88 32.80 34.76 34.59591 84310196 2916787872
#> 5: 2018-01-08  34.76 35.11 36.96 35.11 35.99 36.04448 83078359 2994515872
#> 6: 2018-01-09  35.99 35.63 36.11 34.95 35.84 35.55054 47845909 1700947894
#>    BMK_CLOSE BMK_OPEN
#> 1:  3405.275 3405.275
#> 2:  3429.864 3429.864
#> 3:  3442.373 3442.373
#> 4:  3446.696 3446.696
#> 5:  3459.510 3459.510
#> 6:  3470.250 3470.250
(from_to <- range(tail(tf_quote$DATE)))
#> [1] "2018-04-26" "2018-05-07"

factors <- tf_reg_factors()
str(factors)
#>  chr [1:191] "alpha001" "alpha002" "alpha003" "alpha004" "alpha005" ...
#>  - attr(*, "normal")= chr [1:128] "alpha002" "alpha003" "alpha004" "alpha005" ...
#>  - attr(*, "panel")= chr [1:63] "alpha001" "alpha006" "alpha007" "alpha008" ...
(normal_factor <- attr(factors, "normal")[1])
#> [1] "alpha002"
(panel_factor <- attr(factors, "panel")[1])
#> [1] "alpha001"

qt <- tf_quote_xptr(tf_quote)
tf_qt_cal(qt, normal_factor, from_to)
#>             alpha002
#> 2018-04-26  0.228474
#> 2018-04-27 -1.238390
#> 2018-05-02  1.376597
#> 2018-05-03 -1.302913
#> 2018-05-04  1.133333
#> 2018-05-07 -1.404219

qts <- tf_quotes_xptr(tf_quotes)
tf_qts_cal(qts, normal_factor, from_to)
#>              SZ300333    SH601158  SZ002788   SH603101   SH600020   SH601668
#> 2018-04-26  1.8965517  0.08571429  1.564356  0.4707602  0.4444444  0.3563636
#> 2018-04-27 -0.4007534 -0.92500000 -1.666667 -1.0959596 -0.7777778 -0.3200000
#>              SH600615 SZ002721  SZ300517   SH601567   SH603477   SZ002297
#> 2018-04-26  0.8758170       NA 0.5395764 -0.6153846  0.4538462  1.0396341
#> 2018-04-27 -0.3572985       NA 0.3940621 -0.2797203 -0.6760684 -0.6146341
#>              SH600537  SH603906   SH603183   SZ002884  SZ300531  SZ002641
#> 2018-04-26  1.0476190  1.604159  1.0332307  0.4903226  1.494949  0.800000
#> 2018-04-27 -0.7142857 -1.049057 -0.1057424 -2.0000000 -1.122807 -1.666667
#>              SZ002851   SH600719
#> 2018-04-26  0.4403752 -0.7593583
#> 2018-04-27 -0.5645370  0.7930283
tf_qts_cal(qts, panel_factor, from_to)
#>               SZ300333   SH601158   SZ002788  SH603101   SH600020   SH601668
#> 2018-04-26  0.04664214 -0.6441926 -0.5135616 0.3013375 -0.9032789 -0.6186114
#> 2018-04-27 -0.28739263 -0.5270030 -0.4062324 0.6185499 -0.8719299 -0.5925445
#>             SH600615 SZ002721   SZ300517   SH601567    SH603477    SZ002297
#> 2018-04-26 0.1917762       NA -0.6290566 -0.8998849 -0.06515103 -0.07402860
#> 2018-04-27 0.5320094       NA -0.7884800 -0.9069789 -0.05118596 -0.06976503
#>                SH600537   SH603906   SH603183   SZ002884   SZ300531   SZ002641
#> 2018-04-26 -0.296330051 -0.8887960 -0.2486362 -0.5629602 -0.6881489 -0.4926093
#> 2018-04-27 -0.009231862 -0.3809784 -0.2365224  0.4326827 -0.4215067  0.1503632
#>              SZ002851   SH600719
#> 2018-04-26         NA 0.10130851
#> 2018-04-27 -0.6780386 0.05142658
```

## session info

``` r
xfun::session_info(packages = 'techfactor')
#> R version 3.6.2 (2019-12-12)
#> Platform: x86_64-apple-darwin15.6.0 (64-bit)
#> Running under: macOS Catalina 10.15.3
#> 
#> Locale: en_US.UTF-8 / en_US.UTF-8 / en_US.UTF-8 / C / en_US.UTF-8 / en_US.UTF-8
#> 
#> Package version:
#>   anytime_0.3.7     BH_1.72.0.3       data.table_1.12.9 graphics_3.6.2   
#>   grDevices_3.6.2   grid_3.6.2        lattice_0.20.38   magrittr_1.5     
#>   methods_3.6.2     Rcpp_1.0.4.5      stats_3.6.2       techfactor_0.1.1 
#>   utils_3.6.2       xts_0.12.0        zoo_1.8.7
```
