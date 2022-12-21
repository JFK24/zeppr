# ==============================================================================
#' Growing Degree-Days
#'
#' Calculates the growing degree-days independently for each pair of min and
#' max day temperature values.
#' Basic Formula: ((max temperature + min temperature)/2) - base temperature
#'
#' @param t.min vector of minimum day temperatures (numeric vector)
#' @param t.max vector of maximum day temperatures (numeric vector; same length as t.min)
#' @param t.ceiling ceiling temperature value for t.max and t.min (numeric value)
#' @param t.base base growth temperature (numeric value)
#' @param use.floor if TRUE, t.base is a floor value for t.max and t.min (boolean)
#' @return vector of growing degree-days (numeric vector; same length as t.min)
#' @examples
#' # As the following defines, the growing degree-days gdd = ((9+12)/2)-10 = 0.5
#' growing_degree_days(9, 12, t.ceiling=30, t.base=10, use.floor=FALSE)
#' # As the following defines, gdd = ((10+12)/2)-10 = 1
#' growing_degree_days(9, 12, t.ceiling=30, t.base=10, use.floor=TRUE)
#' # As the following defines, gdd = ((10+15)/2)-10 = 2.5
#' growing_degree_days(4, 20, t.ceiling=15, t.base=10, use.floor=TRUE)
#' # Computes on pairs of min and max temperatures defined in 2 vectors of same length
#' growing_degree_days(c(7, 8, 10), c(12, 14, 15), t.ceiling=30, t.base=10, use.floor=FALSE)
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
#' The input data frame must includes columns for date, min and max
#' temperatures. Calculations are done by groups in grouped data frames.
#' Thus, the data.frame should be grouped by relevant variables such as
#' years and geographical locations.
#' In each group of the data frame, every time point from 1st January to last
#' date of interest must be represented by exactly one row (no missing time
#' point). NAs are not supported for the min and max temperatures.
#' Then, the function calculates the growing degree-days independently for
#' each row as defined in [growing_degree_days()].
#' Finally it calculates the cumulative sum in a new column.
#'
#' @param df a data.frame object
#' @param date non-quoted column name containing dates (date column type)
#' @param t.min non-quoted column name containing minimum day temperatures (numeric column)
#' @param t.max non-quoted column name containing maximum day temperatures (numeric column)
#' @param t.ceiling ceiling temperature value for t.max and t.min (numeric value)
#' @param t.base base growth temperature (numeric value)
#' @param use.floor if TRUE, t.base is a floor value for t.max and t.min (boolean)
#' @param hourly.data if TRUE consider dates as hourly data (divides the output by 24),
#' as daily data otherwise
#' @param values_to name of the new column to store the output
#' @return a data.frame copying df but adding a numerical column for the cumulative
#' sum of growing degree-days
#' @examples
#' # given a simple data.frame with date, min and max temperature
#' data <- data.frame(Date=as.Date(c("2022-01-01", "2022-01-02", "2022-01-02")), Tmin=c(4, 6, 11), Tmax=c(12, 14, 20))
#' # we add a column containing growing degree-days as follows:
#' mutate_cumsum_gdd(data, Date, Tmin, Tmax)
#' # With pipes from magrittr:
#' data %>% mutate_cumsum_gdd(Date, Tmin, Tmax)
#' @importFrom magrittr %>%
#' @export
# ==============================================================================
mutate_cumsum_gdd <- function(df, date, t.min, t.max, t.ceiling=30, t.base=5, use.floor=FALSE, hourly.data=FALSE, values_to="cumsum_dd"){
  df <- df %>%
    dplyr::mutate(gdd.19815454532=growing_degree_days({{t.min}}, {{t.max}}, t.ceiling, t.base, use.floor)) %>%
    dplyr::mutate({{values_to}}:=slider::slide_index_sum(x=gdd.19815454532, i={{date}}, before=Inf, complete=T)) %>%
    dplyr::select(-gdd.19815454532)
  if(hourly.data){
    df <- df %>%
      dplyr::mutate({{values_to}}:=.data[[values_to]]/24)
  }
  return(df)
}
