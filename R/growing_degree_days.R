# ==============================================================================
#' Growing Degree-Days
#'
#' Calculates the growing degree-days independently for each pair of min
#' and max day temperature values.
#' Min and max Temperature values are provided as 2 vectors of same size.
#' A pair is defined as 2 values at same position in the vectors.
#' Although no time data is required here for the calculation,
#' each pair represents implicitly a time point: either 1 day or 1 hour.
#' Basic Formula: ((max temperature + min temperature)/2) - base temperature.
#' If the a temperature (min or max temperature) is greater than the ceiling
#' value, it is reset to the ceiling value.
#' When using a floor value, if the a temperature (min or max temperature) is
#' smaller than the base temperature, it is reset to the base temperature.
#'
#' @param t.min (numeric) vector of minimum day temperatures
#' @param t.max (numeric) vector of maximum day temperatures (same length as `t.min`)
#' @param t.ceiling (dbl) ceiling temperature value for `t.max` and `t.min`
#' @param t.base (dbl) base growth temperature
#' @param use.floor (boolean) `t.base` is used as a floor value
#' for `t.max` and `t.min` if `TRUE`, nothing more otherwise
#' @return  (numeric) vector of growing degree-days (same length as `t.min`)
#' @examples
#' # As the following defines, the growing degree-days gdd = ((9+12)/2)-10 = 0.5
#' growing_degree_days(9, 12, t.ceiling=30, t.base=10, use.floor=FALSE)
#' # As the following defines, gdd = ((10+12)/2)-10 = 1
#' growing_degree_days(9, 12, t.ceiling=30, t.base=10, use.floor=TRUE)
#' # As the following defines, gdd = ((10+15)/2)-10 = 2.5
#' growing_degree_days(4, 20, t.ceiling=15, t.base=10, use.floor=TRUE)
#' # Processes pairs of min max temperatures defined in 2 vectors of same length
#' growing_degree_days(c(7, 8, 10), c(12, 14, 15),
#'   t.ceiling=30, t.base=10, use.floor=FALSE)
#' @export
# ==============================================================================
growing_degree_days <- function(t.min, t.max, t.ceiling=30, t.base=5, use.floor=FALSE) {
  my.t.max=ifelse(t.max>t.ceiling, t.ceiling, t.max)
  my.t.min=ifelse(t.min>t.ceiling, t.ceiling, t.min)
  if(use.floor){
    my.t.max=ifelse(my.t.max<t.base, t.base, my.t.max)
    my.t.min=ifelse(my.t.min<t.base, t.base, my.t.min)
  }
  gdd=((my.t.max+my.t.min)/2)-t.base
  gdd=ifelse(gdd<0, 0, gdd)
  return(gdd)
}


# ==============================================================================
#' Adds Growing Degree-Days Column to a Data Frame
#'
#' The input data frame must include columns for date, min and max
#' temperatures. Calculations are done by groups in grouped data frames.
#' Thus, the data.frame should be grouped by relevant variables such as
#' years and geographical locations otherwise it is assumed that there is only
#' 1 location.
#' In each group of the data frame or in the full data frame if no groups are
#' defined, every time point from 1st January (highly recommended) to last
#' date of interest must be represented by exactly one row (no missing time
#' point). `NA`s are not supported for the min and max temperatures.
#' Then, the function calculates the growing degree-days independently for
#' each row as defined in [growing_degree_days()].
#' Finally it calculates the cumulative sum and returns a copy of the data
#' frame with a a new column for the results.
#'
#' @param df (data.frame) table with columns for date, min and max temperatures
#' (or date including time and hourly temperature for hourly data).
#' @param date non-quoted column name containing dates (`Date` class) or
#' dates and times (`POSIXct` class)
#' @param t.min non-quoted column name containing minimum day temperatures if
#' processing daily data, or containing the hourly temperature (like `t.max`)
#' if hourly data (numeric column)
#' @param t.max non-quoted column name containing maximum day temperatures if
#' processing daily data, or containing the hourly temperature (like `t.min`)
#' if hourly data  (numeric column)
#' @param t.ceiling (dbl) ceiling temperature value for `t.max` and `t.min`
#' @param t.base (dbl) base growth temperature
#' @param use.floor (boolean) `t.base` is also a floor value for `t.max` and
#' `t.min` if `TRUE`, nothing more otherwise
#' @param hourly.data (boolean) considers the data as hourly data and divides the
#' output by 24 if `TRUE`, considers as daily data otherwise.
#' @param values.to (chr) name of the new column to store the results
#' @return (data.frame) table copying `df` but adding a numerical column for
#' the cumulative sum of growing degree-days
#' @examples
#' # given a simple data.frame with date, min and max temperature
#' data <- data.frame(
#'   Date=as.Date(c("2022-01-01", "2022-01-02", "2022-01-02")),
#'   Tmin=c(4, 6, 11),
#'   Tmax=c(12, 14, 20))
#' # we add a column containing growing degree-days as follows:
#' mutate_cumsum_gdd(data, Date, Tmin, Tmax)
#' # With pipes from magrittr:
#' library(magrittr)
#' data %>% mutate_cumsum_gdd(Date, Tmin, Tmax)
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ==============================================================================
mutate_cumsum_gdd <- function(df, date, t.min, t.max, t.ceiling=30, t.base=5, use.floor=FALSE, hourly.data=FALSE, values.to="cumsum_gdd"){
  df <- df %>%
    dplyr::mutate(gdd.19815454532=growing_degree_days({{t.min}}, {{t.max}}, t.ceiling, t.base, use.floor)) %>%
    dplyr::mutate({{values.to}}:=slider::slide_index_sum(x=.data$gdd.19815454532, i={{date}}, before=Inf, complete=TRUE)) %>%
    # dplyr::select(-.data$gdd.19815454532)
    dplyr::select(-"gdd.19815454532")
  if(hourly.data){
    df <- df %>%
      dplyr::mutate({{values.to}}:=.data[[values.to]]/24)
  }
  return(df)
}
