
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zeppr (Beta version!)

<!-- badges: start -->
<!-- badges: end -->

This R package provides a collection of R functions to process plant
protection data. It is actually made for a particular institute in
Germany. It is a beta version provided AS IS without any guaranty or
support. It is available for non-commercial use at your own risks.
Contact me for commercial use.

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

From a vector of values, first calculates the the cumulative sum for
each element from the 1st element. Then, divides the values by the sum
of all values. The resulting values will range from 0 to 1 (as
percentages).

``` r
library(zeppr)
normalized_cumsum(c(1,2,3,4,5))
#> [1] 0.06666667 0.20000000 0.40000000 0.66666667 1.00000000
```

### Growing degree-days

Calculates the Growing degree-days for time series data. It can be
hourly or daily data. The data is provided as vectors of time (date or
date and time) and temperatures (min, max or average). The **input
vectors are assumed to be complete**: no missing time points and no
missing temperature values.

``` r
# growing_degree_days()
# mutate_cumsum_gdd()
```

### ISIP weather data processing

TO DO

``` r
# read_isip_hourly_weather_data()
# read_isip_hourly_to_daily_weather_data()
# mutate_isip_daily_weather_with_cumsum_gdd()
# mutate_isip_hourly_weather_with_cumsum_gdd()
```
