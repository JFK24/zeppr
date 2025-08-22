# ______________________________________________________________________________
#' Normalizes Trap Data to Exposition Time
#'
#' Trap data are here numerical values recorded at different time points (dates).
#' Interval length between time points, the exposition time duration (in days),
#' may be irregular (e.g. every Mondays and Saturdays).
#' Traps are initialized at the beginning of each interval (value=0).
#' The end of each interval is a time point where values are recorded (e.g. insect counts).
#' This function normalizes the values as averages per day during the exposition time.
#'
#' Important: the function can normalize values for multiple traps ONLY IF the
#' input data frame is grouped accordingly (using function `dplyr::group_by()`).
#' Otherwise (no groups), the function expects the data for a single trap.
#' In any case, the function will likely bug with NA values,
#' or with duplicate data points (multiple values associated
#' with the same time point). It is not necessary to order the data by date.
#'
#' @param df (data.frame) table of trap data containing at least a column for the
#' date and a column for the values
#' @param date non-quoted column name containing dates (`Date` class)
#' @param values non-quoted column name containing values (`dbl` class)
#' @param default.duration (integer) number of days defining the first interval
#' time duration
#' @param use.mean.days (boolean) the number of days defining the first interval
#' time duration is set to the mean duration of all other exposition times
#' if `TRUE`, to `default.duration` otherwise
#' @param values.to (chr) name of the new column to be added for the normalized values
#' @param ndays.to (chr) name of the new column to be added for the interval
#' time duration values in days, no column added if `NA`
#' @param max.duration (integer) maximal number of days between intervals if positive integer, no maximum otherwise
#' @return (data.frame) mutated table with an additional column for the normalized
#' values, optionally with an other additional column for the interval
#' durations in days. Normalizing a NA value will result in a NA value.
#' Duplicated data points (multiple rows with same date within a group)
#' will produce Inf values.
#' @examples
#' # given a simple data.frame with date and counts
#' data <- data.frame(
#'   date=as.Date(c("2022-01-01", "2022-01-05", "2022-01-06", "2022-01-11")),
#'   counts=c(12, 14, 20, 12))
#' normalize_trap_data(data, date, counts)
#' # Using magrittr's pipes and keeping derived interval durations
#' library(magrittr)
#' data %>% normalize_trap_data(date, counts, ndays.to="n_days")
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ______________________________________________________________________________
normalize_trap_data <- function(
    df, date, values, default.duration=7, use.mean.days=TRUE, values.to="norm_val", ndays.to=NA, max.duration=NA){

  df <- df %>%
    dplyr::mutate(id.198445789832= dplyr::with_order(order_by = {{date}}, fun = dplyr::row_number, x = {{date}})) %>%
    dplyr::mutate(lag.198445789832 = dplyr::with_order(order_by = {{date}}, fun = dplyr::lag, x = {{date}})) %>%
    dplyr::mutate(diff.198445789832 = as.numeric({{date}} - .data$lag.198445789832)) %>%
    dplyr::mutate(
      diff.198445789832 = dplyr::if_else(
        !is.na(.data$diff.198445789832) & !is.na(max.duration) & max.duration>0,
        ifelse(.data$diff.198445789832>max.duration, max.duration, .data$diff.198445789832),
        .data$diff.198445789832
      )
    ) %>%
    dplyr::mutate(med.198445789832=round(mean(.data$diff.198445789832, na.rm=T), 0)) %>%
    dplyr::mutate(med.198445789832=ifelse(use.mean.days, .data$med.198445789832, default.duration)) %>%
    dplyr::mutate(diff.198445789832=ifelse(.data$id.198445789832==1, .data$med.198445789832, .data$diff.198445789832)) %>%
    dplyr::mutate({{values.to}}:={{values}}/.data$diff.198445789832) %>%
    dplyr::select(-"id.198445789832", -"lag.198445789832", -"med.198445789832")
  if(!is.na(ndays.to)){
    df <- df %>% dplyr::rename({{ndays.to}}:=.data$diff.198445789832)
  } else{
    df <- df %>% dplyr::select(-"diff.198445789832")
  }
  return(df)
}
