
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R pacakge zeppr (Beta!)

<!-- badges: start -->
<!-- badges: end -->

The zeppr package for R provides a collection of functions to process
plant protection data. It is actually made for a particular institute
but it may be useful for more persons. It is a beta version provided AS
IS without any guaranty or support. It is available for non-commercial
use at your own risks. Contact me for commercial use.

## Installation

This package was developed using R 4.2.2. The library `devtools` is also
required. You can install the development version of `zeppr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") # uncomment to install devtools if necessary 
devtools::install_github("JFK24/zeppr")
```

## Examples

### Normalized cumulative sum

``` r
library(zeppr)
# cumulative sum without normalization
normalized_cumsum(c(1,2,3,4,5), normalize = FALSE)
#> [1]  1  3  6 10 15
# with normalization to the total sum (as percentage)
normalized_cumsum(c(1,2,3,4,5))
#> [1] 0.06666667 0.20000000 0.40000000 0.66666667 1.00000000
```

### Growing degree-days for vectors

Calculates the Growing degree-days for pairs of min and max
temperatures. It can be hourly or daily data.

``` r
# A simple example: the growing degree-days gdd = ((9+20)/2)-10 = 4.5
growing_degree_days(t.min=9, t.max=20, t.ceiling=30, t.base=10, use.floor=FALSE)
#> [1] 4.5
# If t.base is used as floor value: gdd = ((10+20)/2)-10 = 5
growing_degree_days(t.min=9, t.max=20, t.ceiling=30, t.base=10, use.floor=TRUE)
#> [1] 5
# If a floor and a ceiling temperature are used:  gdd = ((10+15)/2)-10 = 2.5
growing_degree_days(t.min=9, t.max=20, t.ceiling=15, t.base=10, use.floor=TRUE)
#> [1] 2.5
# Processes pairs of min max temperatures defined in 2 vectors of same length
growing_degree_days(c(7, 8, 10), c(12, 14, 15), t.ceiling=30, t.base=10, use.floor=FALSE)
#> [1] 0.0 1.0 2.5
```

### Growing degree-days for data frames

Function `mutate_cumsum_gdd()` works on a data frame with date and
temperature columns to return a copy of the data frame with an
additional column for the cumulative sum of growing degree-days. The
**input columns are assumed to be complete**: no missing time points and
no missing temperature values.

#### For daily data

``` r
# given a simple data.frame with date, min and max temperature
daily.table <- data.frame(
  Date=as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")),
  Tmin=c(4, 6, 11),
  Tmax=c(12, 14, 20))
print(daily.table)
#>         Date Tmin Tmax
#> 1 2022-01-01    4   12
#> 2 2022-01-02    6   14
#> 3 2022-01-03   11   20
# we add a column containing growing degree-days as follows:
mutate_cumsum_gdd(daily.table, date=Date, t.min=Tmin, t.max=Tmax, 
                  t.ceiling=30, t.base=5, use.floor=FALSE, hourly.data=FALSE)
#>         Date Tmin Tmax cumsum_gdd
#> 1 2022-01-01    4   12        3.0
#> 2 2022-01-02    6   14        8.0
#> 3 2022-01-03   11   20       18.5
# With pipes from magrittr and custom column name:
library(magrittr)
daily.table %>% mutate_cumsum_gdd(Date, Tmin, Tmax, values_to="my_gdd")
#>         Date Tmin Tmax my_gdd
#> 1 2022-01-01    4   12    3.0
#> 2 2022-01-02    6   14    8.0
#> 3 2022-01-03   11   20   18.5
```

#### For hourly data

``` r
# given a simple data.frame with date.time and temperature (no min and max)
hourly.table <- data.frame(
  date.time=as.POSIXct(c("2022-01-01 13:00:00", "2022-01-02 14:00:00", "2022-01-03 15:00:00")),
  temperature=c(12, 14, 20)
)
print(hourly.table)
#>             date.time temperature
#> 1 2022-01-01 13:00:00          12
#> 2 2022-01-02 14:00:00          14
#> 3 2022-01-03 15:00:00          20
# we add a column containing growing degree-days as follows:
mutate_cumsum_gdd(hourly.table, date=date.time, t.min=temperature, t.max=temperature, 
                  t.ceiling=30, t.base=5, use.floor=FALSE, hourly.data=TRUE)
#>             date.time temperature cumsum_gdd
#> 1 2022-01-01 13:00:00          12  0.2916667
#> 2 2022-01-02 14:00:00          14  0.6666667
#> 3 2022-01-03 15:00:00          20  1.2916667
# With pipes from magrittr and custom column name:
library(magrittr)
hourly.table %>% mutate_cumsum_gdd(date.time, temperature, temperature, hourly.data=TRUE, values_to="my_gdd")
#>             date.time temperature    my_gdd
#> 1 2022-01-01 13:00:00          12 0.2916667
#> 2 2022-01-02 14:00:00          14 0.6666667
#> 3 2022-01-03 15:00:00          20 1.2916667
```

### ISIP weather data processing

Functions to process weather data from the ISIP service.

``` r
# Read an ISIP Excel file of weather data:
# read_isip_hourly_weather_data()

# Add the cumulative sum of growing degree-days to an ISIP table:
# mutate_isip_weather_with_cumsum_gdd()
```
