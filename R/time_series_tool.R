# ==============================================================================
#' Aggregates numerical values over a sliding and flexible time window.
#'
#' The numerical values are defined in an input vector.
#' The values are iterated in the order given by a vector of corresponding dates.
#' The aggregation uses available functions returning a single value such as sum, mean, max.
#' The time window is defined from a date in the past to the currently iterated position in the vector.
#' Examples of usage would be, e.g., to get the sum of values during
#' the last N days (see `window_type`='before', `before_val` and `before_unit`),
#' the average value since 1st January 2022 (see `window_type`='fixed', `fixed_date`),
#' the sum since last 15th October (see `window_type`='start_values', `start_month`, `start_day`),
#' or the maximun since the 101th day of the year (see `window_type`='yday', `start_yday`)..
#' @param values (numeric) vector of values to aggregate
#' @param times (Date) vector of dates indexing the vector of values
#' @param expr (chr) R expression representing a function call to aggregate
#' the values where '.' replaces the vector of values
#' (e.g. '`sum(., na.rm=TRUE)`', '`mean(., na.rm=TRUE)`', `dplyr::first(.)`')
#' @param window_type (chr) type of window among 'yday', 'start_values', 'before', and 'fixed'
#' @param fixed_date (Date) window start date for 'fixed' window type
#' @param before_val (int) window duration in units defined by `before_unit` for 'before' window type
#' @param before_unit (chr) window duration unit among 'days', 'weeks', 'months' and 'years' for 'before' window type
#' @param start_month (int) start month number for 'start_values' window type
#' @param start_day (int) start day number in the start month for 'start_values' window type
#' @param start_yday (int) start day of year number for 'yday' window type
#' @param complete (bool) returns NA for incomplete windows if TRUE,
#' proceed to computations ignoring missing window parts otherwise
#' @return (numeric) a vector of same size as `values` where values at a given
#' position (positions defined by dates in the `times` vector) are replaced by
#' the aggregation statistic calculated on the time window.
#' The time window is defined from a start to an end date. While sliding over the
#' input `values` in the order defined by `times`, from the first to the last,
#' the window end is the current position and the window start depends on `window_type` as follows.
#' Window type `'fixed'`, the window starts at the date defined in `fixed_date`.
#' Window type `'before'`, the window starts at a relative number (`before_val`)
#' of time units (`before_unit`) before the current position.
#' Window type `'start_values'`, the window starts at the last date (same year or year before)
#' for which the month and day are equal to `start_month` and `start_day`, respectively.
#' Window type `'yday'`, the window starts at the last date (same year or year before)
#' for which the day of the year is equal to `start_yday`.
#' @examples
#' times <- as.Date(as.Date("2021-12-28"):as.Date("2022-01-03"))
#' values <- c(10, 5, 8, 12, 6, 14, 11)
#' expr="sum(., na.rm=TRUE)"
#' time_slider(values, times, expr, "before", before_val='3', complete=TRUE)
#' time_slider(values, times, expr, "before", before_val='3', complete=FALSE)
#' time_slider(values, times, expr, "fixed", fixed_date='2021-12-29', complete=TRUE)
#' time_slider(values, times, expr, "start_values", start_month=12, start_day=29, complete=TRUE)
#' time_slider(values, times, expr, "yday", start_yday=363, complete=TRUE)
#' @export
# ==============================================================================
time_slider <- function(values, times, expr="sum(., na.rm=TRUE)",
                        window_type="yday",
                        fixed_date=NA, before_val=NA, before_unit="days",
                        start_yday=1, start_month=NA, start_day=NA, complete=TRUE){

  if(before_unit %in% c("days", "weeks", "years")){before_unit=paste0("lubridate::", before_unit)}
  start_month_chr <- sprintf("%02d", start_month)
  start_day_chr <- sprintf("%02d", start_day)
  if(inherits(fixed_date, "character")){fixed_date=as.Date(fixed_date)}

  before_expr <- dplyr::case_when(
    window_type=='yday' ~ "~ dplyr::if_else(lubridate::yday(.x)>=start_yday, as.Date(start_yday-1, origin=paste(lubridate::year(.x), '01', '01', sep='-')), as.Date(start_yday-1, origin=paste(lubridate::year(.x)-1, '01', '01', sep='-')))",
    window_type=='start_values' ~ "~ dplyr::if_else(lubridate::month(.x)>start_month | (lubridate::month(.x)==start_month & lubridate::day(.x)>=start_day) , as.Date(paste(lubridate::year(.x), start_month_chr, start_day_chr, sep='-')), as.Date(paste(lubridate::year(.x)-1, start_month_chr, start_day_chr, sep='-')))",
    window_type=='before' ~ paste0(before_unit, "(", before_val, ")"),
    window_type=='fixed' ~ "~ dplyr::if_else(.x>=fixed_date, fixed_date, .x)"
  )
  fixed_date_res <- slider::slide_index_dbl(
    .x=values,
    .i=times,
    .f= ~ eval(rlang::parse_expr(expr)),
    .before = eval(rlang::parse_expr(before_expr)),
    .complete = complete
  )
  if(window_type=='fixed'){
    fixed_date_res[times<fixed_date] <- NA
  }
  return(fixed_date_res)
}


# ==============================================================================
#' Binary Indexes consecutive leading values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the leading group are kept otherwise
#' @return (boolean) vector (same length as `x`): TRUE for consecutive leading values complying to the condition, FALSE otherwise
#' @examples
#' y <- c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04")
#' y <- rev(y)
#' x <- c(1, 7, 8, 4)
#' index_leading_values_if(x=x, operator="==", ref.value=1, order.by=NA)
#' index_leading_values_if(x=x, operator="<=", ref.value=1, order.by=NA)
#' x[order(y)]
#' index_leading_values_if(x=x, operator="<=", ref.value=7, order.by=y)
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_leading_values_if(x=x, operator="<", ref.value=8)
#' index_leading_values_if(x=x, operator="<", ref.value=8, keep.na.values=TRUE)
#' # Using a second vector y from which we take the order
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=FALSE)
#' index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=TRUE)
#' @export
# ==============================================================================
index_leading_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  # sum and length are equal to 1 if order.by=NA, to 0 if order.by=c(), to 2 order.by=c(NA, NA)
  if(sum(is.na(order.by))==length(order.by)){order.by=1:length(x)}
  my.order <- order(order.by)
  x_ordered <- x[my.order]

  op.res <- is.na(x_ordered)
  if(operator %in% c("==", "<", "<=", ">", ">=")){
    op.res <- get(operator)(x_ordered, ref.value)
  }
  na.mask <- op.res
  if(!keep.na.values){na.mask <- TRUE}
  my.index=na.mask & normalized_cumsum(!op.res, normalize = TRUE, na.replace = 0)==0

  return(my.index[order(my.order)])
}

# # Original !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# index_leading_values_if <- function(x, operator="==", ref.value=0, keep.na.values=FALSE) {
#   op.res <- is.na(x)
#   if(operator %in% c("==", "<", "<=", ">", ">=")){
#     op.res <- get(operator)(x, ref.value)
#   }
#   na.mask <- op.res
#   if(!keep.na.values){na.mask <- TRUE}
#   my.index=na.mask & normalized_cumsum(!op.res, normalize = TRUE, na.replace = 0)==0
#   my.index
#
#   return(my.index)
# }


# ==============================================================================
#' Counts consecutive leading values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the leading group are kept otherwise
#' @return (numeric): number of consecutive leading values complying to the condition
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_leading_values_if(x=x, operator="<", ref.value=8)
#' n_leading_values_if(x=x, operator="<", ref.value=8)
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_leading_values_if(x=x[order(y)], operator="<", ref.value=8, order.by=NA)
#' index_leading_values_if(x=x, operator="<", ref.value=8, order.by=y)
#' n_leading_values_if(x=x, operator="<", ref.value=8, order.by=y)
#' @export
# ==============================================================================
n_leading_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  my.index <- index_leading_values_if(x=x, operator=operator, ref.value=ref.value, order.by=order.by, keep.na.values=keep.na.values)
  return(sum(my.index, na.rm=TRUE))
}


# ==============================================================================
#' Binary Indexes consecutive trailing values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the trailing group are kept otherwise
#' @return (boolean) vector (same length as `x`): TRUE for consecutive trailing values complying to the condition, FALSE otherwise
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' index_trailing_values_if(x=x, operator=">=", ref.value=2, keep.na.values=TRUE)
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_trailing_values_if(x=x, operator="<=", ref.value=4, order.by=y, keep.na.values=FALSE)
#' @export
# ==============================================================================
index_trailing_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  if(sum(is.na(order.by))==length(order.by)){order.by=1:length(x)}
  my.order <- order(order.by)
  x_ordered <- x[my.order]
  # my.index <- rev(index_leading_values_if(x=rev(x), operator=operator, ref.value=ref.value, order.by=order.by, keep.na.values=keep.na.values))
  my.index <- rev(index_leading_values_if(x=rev(x_ordered), operator=operator, ref.value=ref.value, order.by=NA, keep.na.values=keep.na.values))
  # return(my.index)
  return(my.index[order(my.order)])
}


# ==============================================================================
#' Counts consecutive trailing values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the trailing group are kept otherwise
#' @return (numeric): number of consecutive trailing values complying to the condition
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' n_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_trailing_values_if(x=x, operator="<=", ref.value=4, order.by=y, keep.na.values=FALSE)
#' n_trailing_values_if(x=x, operator="<=", ref.value=4, order.by=y, keep.na.values=FALSE)
#' @export
# ==============================================================================
n_trailing_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  my.index <- index_trailing_values_if(x=x, operator=operator, ref.value=ref.value, order.by=order.by, keep.na.values=keep.na.values)
  return(sum(my.index, na.rm=TRUE))
}


# ==============================================================================
#' Binary Indexes the first value in a vector complying to a condition.
#' The value is from a given numerical vector and the condition is tested by
#' comparing the vector values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param complete.to.end (bool) change the output by returning TRUE for the
#' first value complying to the condition and also for all values after the first,
#' FALSE otherwise
#' @return (boolean) vector (same length as `x`): TRUE for the first value complying to the condition, FALSE otherwise
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=FALSE)
#' index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=TRUE)
#' index_first_value_if(x=x, operator="is.na")
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_first_value_if(x=x[order(y)], operator=">", ref.value=6, order.by=NA, complete.to.end=FALSE)
#' index_first_value_if(x=x[order(y)], operator=">", ref.value=6, order.by=NA, complete.to.end=TRUE)
#' index_first_value_if(x=x, operator=">", ref.value=6, order.by=y, complete.to.end=FALSE)
#' index_first_value_if(x=x, operator=">", ref.value=6, order.by=y, complete.to.end=TRUE)
#' @export
# ==============================================================================
index_first_value_if <- function(x, operator="==", ref.value=0, order.by=NA, complete.to.end=FALSE) {
  # sum and length are equal to 1 if order.by=NA, to 0 if order.by=c(), to 2 order.by=c(NA, NA)
  if(sum(is.na(order.by))==length(order.by)){order.by=1:length(x)}
  my.order <- order(order.by)
  x_ordered <- x[my.order]

  op.res <- is.na(x_ordered)
  if(operator %in% c("==", "<", "<=", ">", ">=")){
    op.res <- get(operator)(x_ordered, ref.value)
  }
  my.index <- which(op.res)
  y <- x_ordered & FALSE
  if(!length(my.index)==0){
    my.first.val.index <- min(my.index)
    if(!complete.to.end){
      y[my.first.val.index] <- TRUE
    } else{
      y[my.first.val.index:length(y)] <- TRUE
    }
  }
  # return(y)
    return(y[order(my.order)])
}

# # Original
# index_first_value_if <- function(x, operator="==", ref.value=0, complete.to.end=FALSE) {
#   op.res <- is.na(x)
#   if(operator %in% c("==", "<", "<=", ">", ">=")){
#     op.res <- get(operator)(x, ref.value)
#   }
#   my.index <- which(op.res)
#   y <- x & FALSE
#   if(!length(my.index)==0){
#     my.first.val.index <- min(my.index)
#     if(!complete.to.end){
#       y[my.first.val.index] <- TRUE
#     } else{
#       y[my.first.val.index:length(y)] <- TRUE
#     }
#   }
#   return(y)
# }


