# ==============================================================================
#' Reads ISIP hourly weather export data file
#'
#' Reads ISIP hourly weather export data file (one row per hour) and returns
#' a data frame with following columns: `date.time`, `location`,
#' `temperature`, `humidity`, `precipitation`, `radiation`, `wind_speed`.
#' All sheets from the Excel file are processed and gathered in the same table.
#' Each Excel sheet contains the data for a particular geographical location
#' and the sheet name provides the location id after trimming a suffix
#' starting from the last underscore character.
#'
#' @param excel.path (chr) path to ISIP hourly weather export data file (Excel
#' file). ISIP's Excel Data format as of Dec 2022.
#' @return (data.frame) with columns: location (chr),
#' date.time (dttm object), temperature, humidity, precipitation, radiation,
#' wind_speed
#' @examples
#' path <- system.file("extdata",
#'   "20221215_isip_hourly_weather_data_export.xlsx", package = "zeppr")
#' read_isip_hourly_weather_data(path)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
# ==============================================================================
read_isip_hourly_weather_data <- function(excel.path){
  sheet.names <- sort(readxl::excel_sheets(excel.path))
  my.table <- tibble::tibble()
  for(sheet.name in sheet.names){
    my.location <- stringr::str_remove(sheet.name, "_.\\d+$")
    d <- readxl::read_excel(excel.path, sheet=sheet.name, .name_repair = "minimal")
    d <- d[,-7] %>% tibble::tibble()
    names(d) <- c("ID", "date.time", "temperature", "humidity", "precipitation", "radiation", "wind_speed")
    d <- d %>%
      dplyr::select(-.data$ID) %>%
      dplyr::mutate(location=my.location) %>%
      dplyr::select(.data$location, .data$date.time, tidyselect::everything())
    my.table <- dplyr::bind_rows(my.table, d)
  }
  return(my.table)
}


# ==============================================================================
#' Reads ISIP weather export data file (from hourly to daily data)
#'
#' Reads ISIP hourly weather export data file (one row per hour), averages
#' values per day (except for min and max temperatures), and returns a data
#' frame with following columns of daily data (one row per day):
#' `date`, `location`, `Tmin` (minimum temperature),
#' `Tavg` (average temperature), `Tmax` (maximum temperature),
#' `humidity`, `precipitation`, `radiation`, `wind_speed`,
#' `n.hours` (number of hours with data).
#' All sheets from the Excel file are processed and gathered in the same table.
#' Each Excel sheet contains the data for a particular geographical location
#' and the sheet name provides the location id after trimming a suffix
#' starting from the last underscore character.
#'
#' @param excel.path (chr) path to ISIP hourly weather export data file (Excel
#' file). ISIP's Excel Data format as of Dec 2022.
#' @return (data.frame) table with columns: location (chr), date (dttm object),
#' Tmin, Tavg, Tmax, humidity, precipitation, radiation, wind_speed, n.hours
#' @examples
#' path <- system.file("extdata",
#'   "20221215_isip_hourly_weather_data_export.xlsx", package = "zeppr")
#' read_isip_hourly_to_daily_weather_data(path)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
# ==============================================================================
read_isip_hourly_to_daily_weather_data <- function(excel.path){
  my.table <- read_isip_hourly_weather_data(excel.path)
  my.table <- my.table %>%
    dplyr::mutate(date=as.Date(.data$date.time)) %>%
    dplyr::select(.data$date, .data$date.time, .data$location, tidyselect::everything()) %>%
    dplyr::group_by(.data$location, .data$date) %>%
    dplyr::summarise(
      Tmin=min(.data$temperature),
      Tavg=mean(.data$temperature),
      Tmax=max(.data$temperature),
      humidity=mean(.data$humidity),
      precipitation=mean(.data$precipitation),
      radiation=mean(.data$radiation),
      wind_speed=mean(.data$wind_speed),
      n.hours=dplyr::n(),
      .groups="drop"
    )
  return(my.table)
}


# ==============================================================================
#' mutates ISIP daily weather data table to add the cumulative sum of growing degree-days
#'
#' The ISIP data should be loaded by function [read_isip_hourly_to_daily_weather_data()].
#' For each location and each year in the data, daily time points (rows of the
#' table) are expected to be complete starting from 1st January to last time
#' point of interest (e.g. every day from 01 January to 30 June).
#' The data will be ordered by location and date, temporarily grouped by
#' location and year, and then the cumulative sum of growing degree-days at
#' each time point (each day) will be stored in a new column.
#' Growing degree-day formula: ((max + min temperature)/2) - base temperature
#'
#' @param isip.df (data.frame) table of weather data created with function [read_isip_hourly_to_daily_weather_data()]
#' @param t.ceiling (dbl) ceiling temperature value for the min and max temperatures
#' @param t.base (dbl) base temperature
#' @param use.floor (boolean) if `TRUE`, `t.base` is a floor value for the min and max temperatures
#' @param values_to (chr) name of the new column to be added
#' @return (data.frame) mutated table with an additional column for the cumulative sum of the growing degree-days
#' @examples
#' # Open example file:
#' file <- "20221215_isip_hourly_weather_data_export.xlsx"
#' path <- system.file("extdata", file, package = "zeppr")
#' d <- read_isip_hourly_to_daily_weather_data(path)
#' # Apply function with default parameters:
#' mutate_isip_daily_weather_with_cumsum_gdd(d)
#' # Apply 2 times the function with magrittr pipes and tuned parameters:
#' library(magrittr)
#' d %>%
#'   mutate_isip_daily_weather_with_cumsum_gdd(
#'     t.ceiling=26, t.base=10, use.floor=TRUE, values_to="cs_gdd")
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ==============================================================================
mutate_isip_daily_weather_with_cumsum_gdd <- function(isip.df, t.ceiling=30, t.base=5, use.floor=FALSE, values_to="cumsum_gdd"){
  isip.df <- isip.df %>%
      dplyr::arrange(.data$location, .data$date) %>%
      dplyr::mutate(year.198445789832=lubridate::year(.data$date)) %>%
      dplyr::with_groups(
        .groups = c(.data$location, .data$year.198445789832),
        dplyr::mutate,
        gdd.198445789832=growing_degree_days(.data$Tmin, .data$Tmax, t.ceiling, t.base, use.floor)
      ) %>%
      dplyr::with_groups(
        .groups = c(.data$location, .data$year.198445789832),
        dplyr::mutate,
        {{values_to}}:=slider::slide_index_sum(x=.data$gdd.198445789832, i=.data$date, before=Inf, complete=T)
      ) %>%
      dplyr::select(-.data$gdd.198445789832, -.data$year.198445789832)
  return(isip.df)
}


# ==============================================================================
#' mutates ISIP hourly weather data table to add the cumulative sum of growing degree-days
#'
#' The ISIP data should be loaded by function [read_isip_hourly_weather_data()].
#' For each location and each year in the data, hourly time points (rows of the
#' table) are expected to be complete starting from 1st January to last time
#' point of interest (e.g. every hour from 01 January to 30 June).
#' The data will be ordered by location and time, temporarily grouped by
#' location and year, and then the cumulative sum of growing degree-days at
#' each time point (each hour) will be stored in a new column.
#' Growing degree-day formula: ((max + min temperature)/2) - base temperature
#'
#' @param isip.df (data.frame) table of weather data created with function [read_isip_hourly_weather_data()]
#' @param t.ceiling (dbl) ceiling temperature value
#' @param t.base (dbl) base temperature
#' @param use.floor (boolean) if `TRUE`, `t.base` is a floor temperature value
#' @param values_to (chr) name of the new column to be added
#' @param max_per_day (boolean) outputs the maximum per day if `TRUE`, reports for each hour otherwise
#' @return (data.frame) mutated table with an additional column for the cumulative sum of the growing degree-days
#' @examples
#' # Open example file:
#' path <- system.file(
#'   "extdata",
#'   "20221215_isip_hourly_weather_data_export.xlsx",
#'   package = "zeppr")
#' d <- read_isip_hourly_weather_data(path)
#' # Apply function with default parameters:
#' mutate_isip_hourly_weather_with_cumsum_gdd(d)
#' # Apply 2 times the function with magrittr pipes and tuned parameters:
#' library(magrittr)
#' d %>%
#'   mutate_isip_hourly_weather_with_cumsum_gdd(
#'     t.ceiling=26, t.base=6, use.floor=TRUE, values_to="cs_gdd_1") %>%
#'   mutate_isip_hourly_weather_with_cumsum_gdd(
#'     t.ceiling=26, t.base=6, use.floor=TRUE, values_to="cs_gdd_2",
#'     max_per_day=TRUE) %>%
#'   print(n=48)
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ==============================================================================
mutate_isip_hourly_weather_with_cumsum_gdd <- function(isip.df, t.ceiling=30, t.base=5, use.floor=FALSE, values_to="cumsum_gdd", max_per_day=FALSE){
  isip.df <- isip.df %>%
    dplyr::arrange(.data$location, .data$date.time) %>%
    dplyr::mutate(year.198445789832=lubridate::year(.data$date.time)) %>%
    dplyr::with_groups(
      .groups = c(.data$location, .data$year.198445789832),
      dplyr::mutate,
      gdd.198445789832=growing_degree_days(.data$temperature, .data$temperature, t.ceiling, t.base, use.floor)
    ) %>%
    dplyr::with_groups(
      .groups = c(.data$location, .data$year.198445789832),
      dplyr::mutate,
      {{values_to}}:=slider::slide_index_sum(x=.data$gdd.198445789832, i=.data$date.time, before=Inf, complete=T)/24
    ) %>%
    dplyr::select(-.data$gdd.198445789832, -.data$year.198445789832)
  if(max_per_day){
    isip.df <- isip.df %>%
      dplyr::mutate(day.198445789832=as.Date(.data$date.time)) %>%
      dplyr::with_groups(
        .groups = c(.data$location, .data$day.198445789832),
        dplyr::mutate,
        {{values_to}}:=max(.data[[values_to]])
        ) %>%
      dplyr::select(-.data$day.198445789832)
  }
  return(isip.df)
}


