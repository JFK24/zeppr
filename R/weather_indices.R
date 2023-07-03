# ==============================================================================
#' Day Temperature Index
#'
#' Calculates weather temperature indices from daily weather data.
#' It is based on definitions provided by the German Weather Service.
#' The requested index can be in English or German.
#'
#' The following indices can be calculated:
#' ice (`t.max`<0), frost (`t.min`<0), vegetation ((`t.max`+`t.min`)/2)>=5),
#' heating ((`t.max`+`t.min`)/2)<15), summer (`t.max`>=25),
#' tropical (`t.max`>=30) or desertic day (`t.max`>=35).
#'
#' Sources in German:
#'
#' https://de.wikipedia.org/wiki/Klimatologie
#'
#' https://www.dwd.de/DE/service/lexikon/Functions/glossar.html?nn=103346&lv2=101334&lv3=101452
#'
#' @param t.min (numeric) vector of minimum day temperatures
#' @param t.max (numeric) vector of maximum day temperatures (same length as `t.min`)
#' @param index (chr) type of temperature index to calculate in English or German.
#' One of the following in English:
#' "ice", "frost", "vegetation", "heating", "summer", "tropical", "desertic".
#' Or one of the corresponding in German:
#' "Eistag", "Frosttag", "Vegetationstag", "Heiztag", "Sommertag",
#' "Tropentag", or "Wuestentag".
#' @return (boolean) vector of TRUE or FALSE values: TRUE if `t.min` or `t.max`
#' are compatible to the requested `index` definition, FALSE otherwise (same length as `t.min`).
#' @examples
#' day_temp_index(t.min=12, t.max=32, index="tropical")
#' day_temp_index(t.min=c(12, 15), t.max=c(15, 30), index="heating")
#' @export
# ==============================================================================
day_temp_index <- function(t.min, t.max, index="heating") {
  if(index=="ice"        | index=="Eistag"){return(t.max<0)}
  if(index=="frost"      | index=="Frosttag"){return(t.min<0)}
  if(index=="vegetation" | index=="Vegetationstag"){return(((t.max+t.min)/2)>=5)}
  if(index=="heating"    | index=="Heiztag"){return(((t.max+t.min)/2)<15)}
  if(index=="summer"     | index=="Sommertag"){return(t.max>=25)}
  if(index=="tropical"   | index=="Tropentag"){return(t.max>=30)}
  if(index=="desertic"   | index=="Wuestentag"){return(t.max>=35)}
  warning("Unknown temperature index")
  return(rep(NA, length(t.min)))
}


# ==============================================================================
#' Past Day Temperature Index
#'
#' For a set of consecutive days (represented by vectors of dates, min and max
#' temperatures) and a given time window (smaller than the set), this function
#' calculates for each day either the number of days matching the definition of
#' a temperature index, or the longest period (smaller than the window) where
#' days matched the index definition. By default, it computes the index of each
#' day independently from other days (time window of 1 day). The daily data must
#' be complete (no missing or duplicated days) and sorted by date.
#'
#' The time window is defined either by a number of days (accounting for the
#' current day and past days if required), a date in a year (a month and a
#' day of month; whatever the year), or a given full date (e.g. "2022-01-12").
#'
#' Temperature indices are calculated by the function [day_temp_index()].
#' The following indices can be used:
#' ice (`t.max`<0), frost (`t.min`<0), vegetation ((`t.max`+`t.min`)/2)>=5),
#' heating ((`t.max`+`t.min`)/2)<15), summer (`t.max`>=25),
#' tropical (`t.max`>=30) or desertic day (`t.max`>=35).
#'
#' @param date (Date) vector of consecutive days (no missing values allowed)
#' @param t.min (numeric) vector of minimum day temperatures (same length as `date`) (no missing values allowed)
#' @param t.max (numeric) vector of maximum day temperatures (same length as `date`) (no missing values allowed)
#' @param n.days (integer) time window size in days (past and current days)
#' @param from.month.day (integer) past day in month to start
#' counting (overwrites `n.days` if not `NA`). To be defined together with `from.month`.
#' Year is not taken into account (may count past indices across several years).
#' @param from.month (integer) past month to start counting
#' (overwrites `n.days` if not `NA`). To be defined together with `from.month.day`.
#' Year is not taken into account (may count past indices across several years).
#' @param from.date (chr) past date to start counting (format: YYYY-MM-DD)
#' (overwrites `n.days`, `from.month.day` and `from.month` if not `NA`)
#' @param longest_period (boolean) returns the maximal number of consecutive days
#' matching the index definition within the time window if TRUE, the number of
#' days matching the index definition within the time window otherwise.
#' @param index (chr) type of temperature index to calculate in English or German.
#' One of the following in English:
#' "ice", "frost", "vegetation", "heating", "summer", "tropical", "desertic".
#' Or one of the corresponding in German:
#' "Eistag", "Frosttag", "Vegetationstag", "Heiztag", "Sommertag",
#' "Tropentag", or "Wuestentag".
#' @return (integer) a vector of values counting the number of days matching the
#' index definition. First values may be equal to `NA` if the time window can
#' not fit the number of available days (same length as `t.min`).
#' @examples
#' data <- data.frame(
#'   Date=as.Date(c("2022-01-10", "2022-01-11", "2022-01-12", "2022-01-13", "2022-01-14")),
#'   Tmin=c(4, 6, 11, 10, 20),
#'   Tmax=c(12, 14, 20, 12, 25))
#' print(data)
#' # Annotate each day (1 if heating day, 0 otherwise)
#' past_day_temp_index(data$Date, data$Tmin, data$Tmax, index="heating")
#' # Count past heating days in the last 2 days
#' past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=2, index="heating")
#' # Longest period of heating days in the last 5 days
#' past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=5,
#'                     index="heating", longest_period=TRUE)
#' # Count days matching index definition since a given date in year (all years together)
#' past_day_temp_index(data$Date, data$Tmin, data$Tmax,
#'                     index="vegetation", from.month=1, from.month.day=11)
#' # Count days matching index definition since a given date
#' past_day_temp_index(data$Date, data$Tmin, data$Tmax,
#'                     index="vegetation", from.date="2022-01-12")
#' @export
# ==============================================================================
past_day_temp_index <- function(date, t.min, t.max, n.days=1,
                                from.month.day=NA, from.month=NA,
                                from.date=NA,
                                longest_period=FALSE,
                                index="heating") {

  # date <- as.Date(c("2022-01-10", "2022-01-11", "2022-01-12", "2022-01-13", "2022-01-14", "2022-01-15"))
  # t.min <- c(4, 6, 11, 15, 20, 10)
  # t.max <- c(12, 14, 20, 17, 25, 12)
  # date <- as.Date(c("2022-01-10"))
  # t.min <- c(4)
  # t.max <- c(12)
  # n.days=2
  # from.month.day=NA
  # from.month=NA
  # from.date=NA
  # longest_period=TRUE
  # index="summer"

  index.values <- day_temp_index(t.min, t.max, index)
  if(!is.na(from.date)){
    index.values[date<as.Date(from.date)] <- FALSE
  } else if(!is.na(from.month.day) & !is.na(from.month)){
    my.months <- lubridate::month(date)
    my.mday <- lubridate::mday(date)
    index.values[my.months<from.month] <- FALSE
    index.values[my.months==from.month & my.mday<from.month.day] <- FALSE
  }
  if(longest_period){
    res <- slider::slide_index(
      .x=as.numeric(index.values),
      .i=date,
      .f=function(x){
        my.rle <-rle(x)
        if(sum(my.rle$values==1, na.rm=TRUE)>0){
            return(max(my.rle$lengths[my.rle$values==1]))
        }
        return(0)
      },
      .before=n.days-1,
      .complete=TRUE)
    res[sapply(res, is.null)] <- NA
    res <- unlist(res)
  } else {
    res <- slider::slide_index_sum(x=as.numeric(index.values), i=date, before=n.days-1, complete=TRUE)
  }
  return(res)
}


# ==============================================================================
#' Adds Columns for Current or Past Daily Temperature Indices to a Data Frame
#'
#' The input data frame must contain daily weather data (1 row per day) and
#' include columns for date, min and max temperatures. A new column will be
#' added for each requested temperature index. The daily data must be complete
#' (no missing or duplicated days) and sorted by date.
#' For a set of consecutive days (annotated by columns for dates, min and max
#' temperatures) and a given time window (smaller than the set), this function
#' calculates for each day either the number of days matching the definition of
#' a temperature index, or the longest period (smaller than the window) where
#' days matched the index definition. By default (time window of 1 day),
#' it will return 1 if a day match the definition of the index independently
#' from other days, 0 otherwise.
#'
#' The time window is defined either by a number of days (accounting for the
#' current day and past days if required), a date in a year (a month and a
#' day of month; whatever the year), or a given full date (e.g. "2022-01-12").
#'
#' Temperature indices are calculated by function [day_temp_index()].
#' The following indices can be calculated and summed up:
#' ice (`t.max`<0), frost (`t.min`<0), vegetation ((`t.max`+`t.min`)/2)>=5),
#' heating ((`t.max`+`t.min`)/2)<15), summer (`t.max`>=25),
#' tropical (`t.max`>=30) or desertic day (`t.max`>=35).
#'
#' @param df (data.frame) a data.frame object with columns for date, min and
#' max temperatures (no missing values allowed)
#' @param date non-quoted column name containing dates (Date column)
#' @param t.min non-quoted column name containing minimum day temperatures (numeric column)
#' @param t.max non-quoted column name containing maximum day temperatures (numeric column)
#' @param n.days (integer) time window size in days (past and current days)
#' @param from.month.day (integer) past day in month to start counting
#' (if not `NA`, it overwrites `n.days`). To be defined together with `from.month`.
#' Year is not taken into account (may count past indices across several years).
#' @param from.month (integer) past month to start counting
#' (if not `NA`, it overwrites `n.days`). To be defined together with `from.month.day`.
#' Year is not taken into account (may count past indices across several years).
#' @param from.date (chr) past date to start counting (format: YYYY-MM-DD)
#' (if not `NA`, it overwrites `n.days`, `from.month.day` and `from.month`)
#' @param longest_period (boolean) returns the maximal number of consecutive days
#' matching the index definition within the time window if TRUE, the number of
#' days matching the index definition within the time window otherwise.
#' @param german.output (boolean) returns column names in German if TRUE, in English otherwise
#' @param suffix (chr) string added at the end of new column names
#' @param no.suffix (boolean) keep simple col names if TRUE, add suffixes otherwise
#' @param indices (chr) vector of index names in English to add to the
#' data frame. All available indices are added by Default. Available indices:
#' "ice", "frost", "vegetation", "heating", "summer", "tropical", "desertic".
#' @return (data.frame) table copying `df` but adding a numeric column for
#' each requested index: either the number of days matching the index definition
#' within the given time window, or the length of the longest time period within
#' the time window where days matched the index definition.
#' @examples
#' data <- data.frame(
#'     Date=as.Date( c("2022-01-10", "2022-01-11", "2022-01-12",
#'                   "2022-01-13", "2022-01-14", "2022-01-15")),
#'     Tmin=c(-10, -1, 1, 15, 20, 6),
#'     Tmax=c(-1, 5, 5, 17, 25, 10))
#' # Annotate each day (1 if matching index definition, 0 otherwise)
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, n.days=1)
#' # Annotate each day and get column names without suffixes
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, n.days=1, no.suffix=TRUE)
#' # Count days matching index definition in the last 3 days
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, n.days=3)
#' # maximal number of consecutive days matching index definition in the last 3 days
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, n.days=3, longest_period=TRUE)
#' # Count days matching index definition since a given date in year (all years together)
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, from.month=1, from.month.day=11)
#' # Count days matching index definition since a given date
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, from.date="2022-01-12")
#' # Annotate each day with German column names (1 if matching index definition, 0 otherwise)
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, german.output=TRUE)
#' # Annotate each day with selected indices
#' mutate_past_day_temp_indices(data, Date, Tmin, Tmax, indices=c("frost", "heating"))
#' # Use tidyverse-style pipes
#' library(magrittr)
#' data %>% mutate_past_day_temp_indices(Date, Tmin, Tmax)
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
# ==============================================================================
mutate_past_day_temp_indices <- function(
    df, date, t.min, t.max, n.days=1, from.month.day=NA,
    from.month=NA, from.date=NA, longest_period=FALSE,
    german.output=FALSE, suffix="", no.suffix=FALSE,
    indices=c("ice", "frost", "vegetation", "heating", "summer", "tropical", "desertic")){

  if(german.output){
    german <- c("Eistag", "Frosttag", "Vegetationstag", "Heiztag", "Sommertag", "Tropentag", "Wuestentag")
    german <- stats::setNames(german, c("ice", "frost", "vegetation", "heating", "summer", "tropical", "desertic"))
    indices <- german[indices]
  }

  if(longest_period){suffix <- "_lp"}
  if(!is.na(from.date)){
    suffix <- paste0(as.character(from.date), suffix)
  }
  else if(!is.na(from.month.day) & !is.na(from.month)){
    suffix <- paste0(from.month, "_", from.month.day, suffix)
  } else {
    if(german.output){suffix <- paste0("letzte_", n.days, "T", suffix)}
    else {suffix <- paste0("last_", n.days, "d", suffix)}
  }
  for (index in indices) {
    my.col.name <- ifelse(no.suffix, index, paste0(index, "_", suffix))
    df <- df %>%
      dplyr::mutate({{my.col.name}}:=past_day_temp_index(
          date={{date}}, t.min={{t.min}}, t.max={{t.max}},
          n.days=n.days, from.month.day=from.month.day, from.month=from.month,
          from.date=from.date, longest_period=longest_period, index=index))
  }
  return(df)
}

