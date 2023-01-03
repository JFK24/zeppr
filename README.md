
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

### Normalized cumulative sum for vectors

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

Calculates the growing degree-days for pairs of min and max day
temperatures. Each pair represents implicitly a time point: either 1 day
or 1 hour (no time data is required here for the calculation). A growing
degree-days value is calculated for each pair independently from the
other pairs. The function is developed for daily data but can be used
for hourly data with a few tricks explained below and automatized in
function `mutate_cumsum_gdd()` described further below.

#### For daily data

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

#### Tricks for hourly data

- Hourly data tables have 1 row per hour and thus 24 rows per day
- Each hour has only 1 temperature (no min and max day temperatures)
- Let us define growing degree-hours as the same than growing
  degree-days but applied for a single hour where the min and max
  temperatures are both equal to the temperature for this hour
- The degree-days for 1 day is equal to the sum of its 24 degree-hours
  divided by 24.

The following tricks are automatized by the `mutate_cumsum_gdd()`
function described further below.

``` r
# Let say we have 24 temperatures for a particular day
temperatures = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
# We can get corresponding degree-hours above 5 degrees
gdh <- growing_degree_days(t.min=temperatures, t.max=temperatures, t.base=5)
print(gdh)
#>  [1] 0 0 0 0 0 1 2 3 4 5 6 7 6 5 4 3 2 1 0 0 0 0 0 0
# the growing degree-days for this day is calculated as follows:
sum(gdh)/24
#> [1] 2.041667
```

### Cumulative sum of growing degree-days for data frames

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
# we add a column containing cumulative sum of growing degree-days as follows:
mutate_cumsum_gdd(daily.table, date=Date, t.min=Tmin, t.max=Tmax, 
                  t.ceiling=30, t.base=5, use.floor=FALSE, hourly.data=FALSE)
#>         Date Tmin Tmax cumsum_gdd
#> 1 2022-01-01    4   12        3.0
#> 2 2022-01-02    6   14        8.0
#> 3 2022-01-03   11   20       18.5
# With %>% pipes from magrittr and custom column name:
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
# we add a column containing cumulative sum of growing degree-days as follows: 
# by setting hourly.data to TRUE and reusing hourly temperatures 
# for both t.min and t.max parameters as follows:
mutate_cumsum_gdd(hourly.table, date=date.time, 
                  t.min=temperature, t.max=temperature, 
                  t.ceiling=30, t.base=5, use.floor=FALSE, hourly.data=TRUE)
#>             date.time temperature cumsum_gdd
#> 1 2022-01-01 13:00:00          12  0.2916667
#> 2 2022-01-02 14:00:00          14  0.6666667
#> 3 2022-01-03 15:00:00          20  1.2916667
# With %>% pipes from magrittr and custom column name:
library(magrittr)
hourly.table %>% mutate_cumsum_gdd(date.time, temperature, temperature, hourly.data=TRUE, values_to="my_gdd")
#>             date.time temperature    my_gdd
#> 1 2022-01-01 13:00:00          12 0.2916667
#> 2 2022-01-02 14:00:00          14 0.6666667
#> 3 2022-01-03 15:00:00          20 1.2916667
```

### Reads ISIP hourly weather data

An example ISIP hourly data file is provided with this package at the
following path:

``` r
file.name <- "20221215_isip_hourly_weather_data_export.xlsx"
path <- system.file("extdata", file.name, package = "zeppr")
```

#### Reads the ISIP hourly data file

``` r
# Get hourly data:
hourly.table <- read_isip_hourly_weather_data(path)
head(hourly.table)
#> # A tibble: 6 × 10
#>   location date                 Tmin temperature  Tmax humidity precip…¹ radia…²
#>   <chr>    <dttm>              <dbl>       <dbl> <dbl>    <dbl>    <dbl>   <dbl>
#> 1 BWWR100  2022-01-01 00:00:00    NA        9.83    NA     92.4        0 0.00258
#> 2 BWWR100  2022-01-01 01:00:00    NA        9.48    NA     94.5        0 0.00258
#> 3 BWWR100  2022-01-01 02:00:00    NA        9.21    NA     96.2        0 0.00234
#> 4 BWWR100  2022-01-01 03:00:00    NA        9.12    NA     96.6        0 0.00257
#> 5 BWWR100  2022-01-01 04:00:00    NA        9.01    NA     96.4        0 0.00270
#> 6 BWWR100  2022-01-01 05:00:00    NA        8.80    NA     95.9        0 0.00270
#> # … with 2 more variables: wind_speed <dbl>, n.hours <int>, and abbreviated
#> #   variable names ¹​precipitation, ²​radiation
```

#### Reads the ISIP hourly data file but transform it as daily data

``` r
# Get daily data:
daily.table <- read_isip_hourly_weather_data(path, returns.daily.data=TRUE)
head(daily.table)
#> # A tibble: 6 × 10
#>   locat…¹ date        Tmin tempe…²  Tmax humid…³ preci…⁴ radia…⁵ wind_…⁶ n.hours
#>   <chr>   <date>     <dbl>   <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <int>
#> 1 BWWR100 2022-01-01  4.86    9.37  9.37    91.0  0        51.8    0.774      24
#> 2 BWWR100 2022-01-02  3.44    7.84  7.84    85.8  0        29.5    1.82       24
#> 3 BWWR100 2022-01-03  9.39   11.2  11.2     76.1  0.0542   16.4    3.48       24
#> 4 BWWR100 2022-01-04  4.51    8.51  8.51    94.8  1.52      6.33   2.86       24
#> 5 BWWR100 2022-01-05  2.29    3.45  3.45    81.8  0        36.4    2.84       24
#> 6 BWWR100 2022-01-06 -1.09    2.34  2.34    90.6  0        35.3    1.60       24
#> # … with abbreviated variable names ¹​location, ²​temperature, ³​humidity,
#> #   ⁴​precipitation, ⁵​radiation, ⁶​wind_speed
```

### Process ISIP weather data

#### Add the cumulative sum of growing degree-days to ISIP tables

Processing hourly data:

``` r
# reads file and keep only 10 rows (10 hours) and 5 first columns
hourly.table <- read_isip_hourly_weather_data(path)[1:10,1:5]
# adds cumulative growing degree-days per row divided by 24
mutate_isip_weather_with_cumsum_gdd(hourly.table)
#> # A tibble: 10 × 6
#>    location date                 Tmin temperature  Tmax cumsum_gdd
#>    <chr>    <dttm>              <dbl>       <dbl> <dbl>      <dbl>
#>  1 BWWR100  2022-01-01 00:00:00    NA        9.83    NA      0.201
#>  2 BWWR100  2022-01-01 01:00:00    NA        9.48    NA      0.388
#>  3 BWWR100  2022-01-01 02:00:00    NA        9.21    NA      0.564
#>  4 BWWR100  2022-01-01 03:00:00    NA        9.12    NA      0.735
#>  5 BWWR100  2022-01-01 04:00:00    NA        9.01    NA      0.902
#>  6 BWWR100  2022-01-01 05:00:00    NA        8.80    NA      1.06 
#>  7 BWWR100  2022-01-01 06:00:00    NA        8.58    NA      1.21 
#>  8 BWWR100  2022-01-01 07:00:00    NA        8.45    NA      1.35 
#>  9 BWWR100  2022-01-01 08:00:00    NA        8.25    NA      1.49 
#> 10 BWWR100  2022-01-01 09:00:00    NA        8.60    NA      1.64
# adds max cumulative growing degree-days per day
mutate_isip_weather_with_cumsum_gdd(hourly.table, max_per_day=TRUE)
#> # A tibble: 10 × 6
#>    location date                 Tmin temperature  Tmax cumsum_gdd
#>    <chr>    <dttm>              <dbl>       <dbl> <dbl>      <dbl>
#>  1 BWWR100  2022-01-01 00:00:00    NA        9.83    NA       1.64
#>  2 BWWR100  2022-01-01 01:00:00    NA        9.48    NA       1.64
#>  3 BWWR100  2022-01-01 02:00:00    NA        9.21    NA       1.64
#>  4 BWWR100  2022-01-01 03:00:00    NA        9.12    NA       1.64
#>  5 BWWR100  2022-01-01 04:00:00    NA        9.01    NA       1.64
#>  6 BWWR100  2022-01-01 05:00:00    NA        8.80    NA       1.64
#>  7 BWWR100  2022-01-01 06:00:00    NA        8.58    NA       1.64
#>  8 BWWR100  2022-01-01 07:00:00    NA        8.45    NA       1.64
#>  9 BWWR100  2022-01-01 08:00:00    NA        8.25    NA       1.64
#> 10 BWWR100  2022-01-01 09:00:00    NA        8.60    NA       1.64
```

Processing daily data:

``` r
# reads file and keep only 10 rows (10 days) and 5 first columns
daily.table <- read_isip_hourly_weather_data(path, 
                                             returns.daily.data=TRUE)[1:10,1:5]
# adds cumulative growing degree-days per row
mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE)
#> # A tibble: 10 × 6
#>    location date         Tmin temperature   Tmax cumsum_gdd
#>    <chr>    <date>      <dbl>       <dbl>  <dbl>      <dbl>
#>  1 BWWR100  2022-01-01  4.86        9.37   9.37        2.12
#>  2 BWWR100  2022-01-02  3.44        7.84   7.84        2.76
#>  3 BWWR100  2022-01-03  9.39       11.2   11.2         8.04
#>  4 BWWR100  2022-01-04  4.51        8.51   8.51        9.54
#>  5 BWWR100  2022-01-05  2.29        3.45   3.45        9.54
#>  6 BWWR100  2022-01-06 -1.09        2.34   2.34        9.54
#>  7 BWWR100  2022-01-07 -1.62        0.801  0.801       9.54
#>  8 BWWR100  2022-01-08  1.73        3.00   3.00        9.54
#>  9 BWWR100  2022-01-09  0.686       3.82   3.82        9.54
#> 10 BWWR100  2022-01-10 -0.808       1.75   1.75        9.54
# adds cumulative growing degree-days per row with floor value for 
# min and max temperatures
mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE, use.floor=TRUE)
#> # A tibble: 10 × 6
#>    location date         Tmin temperature   Tmax cumsum_gdd
#>    <chr>    <date>      <dbl>       <dbl>  <dbl>      <dbl>
#>  1 BWWR100  2022-01-01  4.86        9.37   9.37        2.19
#>  2 BWWR100  2022-01-02  3.44        7.84   7.84        3.61
#>  3 BWWR100  2022-01-03  9.39       11.2   11.2         8.89
#>  4 BWWR100  2022-01-04  4.51        8.51   8.51       10.6 
#>  5 BWWR100  2022-01-05  2.29        3.45   3.45       10.6 
#>  6 BWWR100  2022-01-06 -1.09        2.34   2.34       10.6 
#>  7 BWWR100  2022-01-07 -1.62        0.801  0.801      10.6 
#>  8 BWWR100  2022-01-08  1.73        3.00   3.00       10.6 
#>  9 BWWR100  2022-01-09  0.686       3.82   3.82       10.6 
#> 10 BWWR100  2022-01-10 -0.808       1.75   1.75       10.6
```
