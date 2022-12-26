
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zeppr (Beta version)

<!-- badges: start -->
<!-- badges: end -->

This R package provides a collection of R functions to process plant
protection data. It is a beta version provided AS IS without guaranty of
any sort. Use it for non commercial purpose at your own risks. For
commercial use, please contact me.

## Installation

You can install the development version of zeppr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JFK24/zeppr")
```

The library devtools is required to install the package. Uncomment the
first line of code above to install it if necessaray.

## Examples

### Normalized cumulative sum

From a vector of values, returns a vector with the cumulative sum from
1st to current element divided by the sum of all values. The resulting
values will range from 0 to 1.

``` r
library(zeppr)
normalized_cumsum(c(1,2,3,4,5))
#> [1] 0.06666667 0.20000000 0.40000000 0.66666667 1.00000000
```

### Growing degree-days

TO DO

### ISIP weather data processing

TO DO
