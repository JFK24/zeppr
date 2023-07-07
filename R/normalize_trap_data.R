# ______________________________________________________________________________
#' Normalizes Trap Data to Exposition Time
#'
#' Trap data are numerical values collected at different time points (dates).
#' Interval length between time points, the exposition time duration (in days),
#' may be irregular (e.g. every Mondays and Saturdays).
#' Traps are set empty at the beginning of each interval (count=0).
#' The end of each interval is a time point where values are collected (e.g. insect counts).
#' This function normalizes the values as averages per day during the exposition time.
#'
#' Important: the function can normalize values for multiple traps ONLY IF the
#' input data frame is grouped accordingly (using function `dplyr::group_by()`).
#' Otherwise, the function expects the data for a single trap.
#' In any case, the function will likely bug with NA values,
#' non ordered time points, or duplicate data points (multiple values associated
#' with the same time point).
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
#' @return (data.frame) mutated table with an additional column for the normalized
#' values, optionally with an other additional column for the interval time
#' duration values in days
#' @examples
#' # given a simple data.frame with date and counts
#' data <- data.frame(
#'   date=as.Date(c("2022-01-01", "2022-01-05", "2022-01-06", "2022-01-11")),
#'   counts=c(12, 14, 20, 12))
#' normalize_trap_data(data, date, counts)
#' # Using magrittr's pipes
#' library(magrittr)
#' data %>% normalize_trap_data(date, counts, ndays.to="n_days")
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ______________________________________________________________________________
normalize_trap_data <- function(
    df, date, values, default.duration=7, use.mean.days=TRUE, values.to="norm_val", ndays.to=NA){

  df <- df %>%
    dplyr::mutate(id.198445789832=dplyr::row_number()) %>%
    dplyr::mutate(diff.198445789832 = as.numeric({{date}} - dplyr::lag({{date}}))) %>%
    dplyr::mutate(med.198445789832=round(mean(.data$diff.198445789832, na.rm=T), 0)) %>%
    dplyr::mutate(diff.198445789832=ifelse(.data$id.198445789832==1, .data$med.198445789832, .data$diff.198445789832)) %>%
    dplyr::mutate({{values.to}}:={{values}}/.data$diff.198445789832) %>%
    dplyr::select(-"id.198445789832", -"med.198445789832")
  if(!is.na(ndays.to)){
    df <- df %>% dplyr::rename({{ndays.to}}:=.data$diff.198445789832)
  } else{
    df <- df %>% dplyr::select(-"diff.198445789832")
  }
  return(df)
}


# ______________________________________________________________________________
#' Normalizes Trap Data to Exposition Time (Alternative Implementation)
#'
#' Trap data are numerical values collected at different time points (dates).
#' Interval length between time points, the exposition time duration (in days),
#' may be irregular (e.g. every Mondays and Saturdays).
#' Traps are set empty at the beginning of each interval (count=0).
#' The end of each interval is a time point where values are collected (e.g. insect counts).
#' This function normalizes the values as averages per day during the exposition time.
#'
#' Important: the function can normalize values for multiple traps ONLY IF the
#' input data frame is grouped accordingly (using function `dplyr::group_by()`).
#' Otherwise, the function expects the data for a single trap.
#' In any case, the function will likely bug with NA values,
#' non ordered time points, or duplicate data points (multiple values associated
#' with the same time point).
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
#' @return (data.frame) mutated table with an additional column for the normalized
#' values, optionally with an other additional column for the interval time
#' duration values in days
#' @examples
#' # given a simple data.frame with date and counts
#' data <- data.frame(
#'   date=as.Date(c("2022-01-01", "2022-01-05", "2022-01-06", "2022-01-11")),
#'   counts=c(12, 14, 20, 12))
#' normalize_trap_data_2(data, date, counts)
#' # Using magrittr's pipes
#' library(magrittr)
#' data %>% normalize_trap_data_2(date, counts, ndays.to="n_days")
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ______________________________________________________________________________
normalize_trap_data_2 <- function(
    df, date, values, default.duration=7, use.mean.days=TRUE, values.to="norm_val", ndays.to=NA){

  df <- df %>%
    dplyr::mutate(monitoring_day.198445789832=1) %>%
    tidyr::complete({{date}} := seq.Date(min({{date}})-default.duration, max({{date}}), by="day")) %>%
    dplyr::mutate(monitoring_day.198445789832=ifelse(!is.na(.data$monitoring_day.198445789832), 1, 0)) %>%
    dplyr::mutate(monitoring_id.198445789832=slider::slide_index_sum(x=.data$monitoring_day.198445789832, i={{date}}, after=-1, before=Inf)+1) %>%
    tidyr::fill({{values}}, .direction = "up") %>%
    dplyr::group_by(.data$monitoring_id.198445789832, .add = TRUE) %>%
    dplyr::mutate(expo_days.198445789832=dplyr::n()) %>%
    dplyr::ungroup(.data$monitoring_id.198445789832)  %>%
    dplyr::mutate(mean_expo_days.198445789832=dplyr::if_else(use.mean.days, round((sum(.data$expo_days.198445789832)-default.duration)/dplyr::n(), 0), default.duration)) %>%
    dplyr::mutate(expo_days.198445789832=dplyr::if_else(.data$monitoring_id.198445789832==1, .data$mean_expo_days.198445789832, .data$expo_days.198445789832)) %>%
    dplyr::mutate({{values.to}}:= {{values}}/.data$expo_days.198445789832) %>%
    dplyr::filter(.data$monitoring_day.198445789832==1) %>%
    dplyr::select(-"monitoring_day.198445789832", -"monitoring_id.198445789832", -"mean_expo_days.198445789832")
  if(!is.na(ndays.to)){
    df <- df %>% dplyr::rename({{ndays.to}}:=.data$expo_days.198445789832)
  } else{
    df <- df %>% dplyr::select(-"expo_days.198445789832")
  }
  return(df)
}
