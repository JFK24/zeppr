# ==============================================================================
#' Reads ISIP hourly weather export data file
#'
#' Reads ISIP hourly weather export data file and returns a data frame with
#' following columns: date.time, location, temperature, humidity,
#' precipitation, radiation, wind_speed. All sheets from the
#' Excel file are processed and gathered in the same table. Each Excel sheet
#' contains the data for a particular geographical location and the sheet name
#' provides the location id after trimming a suffix starting from the last
#' underscore character.
#'
#' @param file path to ISIP hourly weather export data file (Excel file)
#' @return data.frame/tibble with columns: location (chr),
#' date.time (dttm object), temperature, humidity, precipitation, radiation,
#' wind_speed
#' @examples
#' path <- system.file("extdata", "20221215_isip_hourly_weather_data_export.xlsx", package = "zeppr")
#' read_isip_hourly_weather_data(path)
#' @importFrom magrittr %>%
#' @export
# ==============================================================================
read_isip_hourly_weather_data <- function(file){
  sheet.names <- sort(readxl::excel_sheets(file))
  my.table <- tibble::tibble()
  for(sheet.name in sheet.names){
    my.location <- stringr::str_remove(sheet.name, "_.\\d+$")
    d <- readxl::read_excel(file, sheet=sheet.name, .name_repair = "minimal")
    d <- d[,-7] %>% tibble::tibble()
    names(d) <- c("ID", "date.time", "temperature", "humidity", "precipitation", "radiation", "wind_speed")
    d <- d %>%
      dplyr::select(-ID) %>%
      dplyr::mutate(location=my.location) %>%
      dplyr::select(location, date.time, everything())
    my.table <- dplyr::bind_rows(my.table, d)
  }
  return(my.table)
}


# ==============================================================================
#' Reads ISIP weather export data file (from hourly to daily data)
#'
#' Reads ISIP hourly weather export data file, averages values per day, and
#' returns a data frame with following columns: date, location, Tmin
#' (minimum temperature), Tavg (average temperature), Tmax (maximum temperature),
#' humidity, precipitation, radiation, wind_speed, n.hours (number of hours
#' with data). All sheets from the Excel file are processed and gathered in the
#' same table. Each Excel sheet contains the data for a particular geographical
#' location and the sheet name provides the location id after trimming a suffix
#' starting from the last underscore character.
#'
#' @param file path to ISIP hourly weather export data file (Excel file)
#' @return data.frame/tibble with columns: location (chr), date (dttm object),
#' Tmin, Tavg, Tmax, humidity, precipitation, radiation, wind_speed, n.hours
#' @examples
#' path <- system.file("extdata", "20221215_isip_hourly_weather_data_export.xlsx", package = "zeppr")
#' read_isip_daily_from_hourly_weather_data(path)
#' @importFrom magrittr %>%
#' @export
# ==============================================================================
read_isip_daily_from_hourly_weather_data <- function(file){
  my.table <- read_isip_hourly_weather_data(file)
  my.table <- my.table %>%
    dplyr::mutate(date=as.Date(date.time)) %>%
    dplyr::select(date, date.time, location, everything()) %>%
    dplyr::group_by(location, date) %>%
    dplyr::summarise(
      Tmin=min(temperature),
      Tavg=mean(temperature),
      Tmax=max(temperature),
      humidity=mean(humidity),
      precipitation=mean(precipitation),
      radiation=mean(radiation),
      wind_speed=mean(wind_speed),
      n.hours=dplyr::n(),
      .groups="drop"
    )
  return(my.table)
}


# ==============================================================================
#' mutates ISIP daily weather data table to add the cumulative sum of growing degree-days
#'
#' The ISIP data should be loaded by function [read_isip_daily_from_hourly_weather_data()]
#' Groups the table by location and year and then calculates the cumulative sum
#' of growing degree-days for each day. Growing degree-day
#' formula: ((max + min temperature)/2) - base temperature
#'
#' @param df data.frame of weather data created with function read_isip_daily_from_hourly_weather_data()
#' @param t.ceiling ceiling temperature value for t.max and t.min (numeric value)
#' @param t.base base growth temperature (numeric value)
#' @param use.floor if TRUE, t.base is a floor value for t.max and t.min (boolean)
#' @param values_to name of the new column to be added (string)
#' @return mutated table with an additional column for the cumulative sum of the growing degree-days
#' @examples
#' path <- system.file("extdata", "20221215_isip_hourly_weather_data_export.xlsx", package = "zeppr")
#' d <- read_isip_daily_from_hourly_weather_data(path)
#' mutate_isip_daily_weather_data_cumsum_gdd(d)
#' d %>% mutate_isip_daily_weather_data_cumsum_gdd(t.ceiling=26, t.base=10, use.floor=TRUE, values_to="cs_gdd")
#' @importFrom magrittr %>%
#' @export
# ==============================================================================
mutate_isip_daily_weather_data_cumsum_gdd <- function(df, t.ceiling=30, t.base=5, use.floor=FALSE, values_to="cumsum_dd"){
    df <- df %>%
      dplyr::mutate(year=lubridate::year(date)) %>%
      dplyr::group_by(location, year) %>%
      dplyr::arrange(location, date) %>%
      dplyr::mutate(gdd.198445789832=growing_degree_days(Tmin, Tmax, t.ceiling, t.base, use.floor)) %>%
      dplyr::mutate({{values_to}}:=slider::slide_index_sum(x=gdd.198445789832, i=date, before=Inf, complete=T)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-gdd.198445789832, -year)
  return(df)
}


# ==============================================================================
#' mutates ISIP hourly weather data table to add the cumulative sum of growing degree-days
#'
#' The ISIP data should be loaded by function [read_isip_hourly_weather_data()]
#' Groups the table by location and year and then calculates the cumulative sum
#' of growing degree-days for each day. Growing degree-day
#' formula: ((max + min temperature)/2) - base temperature
#'
#' @param df data.frame of weather data created with function read_isip_hourly_weather_data()
#' @param t.ceiling ceiling temperature value for t.max and t.min (numeric value)
#' @param t.base base growth temperature (numeric value)
#' @param use.floor if TRUE, t.base is a floor value for t.max and t.min (boolean)
#' @param values_to name of the new column to be added (string)
#' @param max_per_day (boolean) outputs the maximum per day if TRUE, reports for each hour otherwise
#' @return mutated table with an additional column for the cumulative sum of the growing degree-days
#' @examples
#' path <- system.file("extdata", "20221215_isip_hourly_weather_data_export.xlsx", package = "zeppr")
#' d <- read_isip_hourly_weather_data(path)
#' mutate_isip_hourly_weather_data_cumsum_gdd(d)
#' d %>% mutate_isip_hourly_weather_data_cumsum_gdd(t.ceiling=26, t.base=6, use.floor=TRUE, values_to="cs_gdd_1") %>%
#' mutate_isip_hourly_weather_data_cumsum_gdd(t.ceiling=26, t.base=6, use.floor=TRUE, values_to="cs_gdd_2", max_per_day=TRUE) %>% print(n=48)
#' @importFrom magrittr %>%
#' @export
# ==============================================================================
mutate_isip_hourly_weather_data_cumsum_gdd <- function(df, t.ceiling=30, t.base=5, use.floor=FALSE, values_to="cumsum_dd", max_per_day=FALSE){
  d <- df %>%
    dplyr::mutate(year.198445789832=lubridate::year(date.time)) %>%
    dplyr::group_by(location, year.198445789832) %>%
    dplyr::arrange(location, date.time) %>%
    dplyr::mutate(gdd.198445789832=growing_degree_days(temperature, temperature, t.ceiling, t.base, use.floor)) %>%
    dplyr::mutate({{values_to}}:=slider::slide_index_sum(x=gdd.198445789832, i=date.time, before=Inf, complete=T)/24) %>%
    dplyr::ungroup() %>%
    dplyr::select(-gdd.198445789832, -year.198445789832)
  if(max_per_day){
    d <- d %>%
      dplyr::mutate(day.198445789832=as.Date(date.time)) %>%
      dplyr::group_by(location, day.198445789832) %>%
      dplyr::mutate({{values_to}}:=max(.data[[values_to]])) %>%
      dplyr::ungroup() %>%
      dplyr::select(-day.198445789832)
  }
  return(d)
}


