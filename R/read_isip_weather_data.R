# ==============================================================================
#' Reads ISIP Hourly Weather Data File
#'
#' Reads an ISIP hourly weather export data file (Excel file; one row per hour)
#' and returns a data frame with either hourly or daily data.
#' The daily data is derived from the hourly data by averaging values per day.
#' All sheets from the Excel file are processed and gathered in the same table.
#' Within an ISIP Excel file, each sheet represents a particular geographical
#' location and contains weather data for the same date and time range.
#' Sheet names provide the location ids after trimming a suffix
#' starting from the last underscore character.
#'
#' @param excel.path (chr) path to ISIP hourly weather export data file (Excel
#' file). ISIP's Excel Data format as of Dec 2022.
#' @param returns.daily.data (boolean) returns daily data if `TRUE`,
#' hourly data otherwise
#' @param drop.irrelvant.cols (boolean) remove irrelevant columns if `TRUE`,
#' keep all columns otherwise
#' @return (data.frame) table with numerical columns except for location (`chr`)
#' and date (`Date` class for daily output or `POSIXct` class for hourly
#' output). Columns: `location`, `date`, `Tmin` (min daily temperature),
#' `Tavg` (average temperature), `Tmax` (max daily temperature), `humidity`,
#' `precipitation`, `radiation`, `wind_speed` and
#' `n_hours` (scope of the data as number of hours, e.g. used to calculate averages).
#' @examples
#' # Let us use an example dataset:
#' file.name <- "20221215_isip_hourly_weather_data_export.xlsx"
#' path <- system.file("extdata", file.name, package = "zeppr")
#' # Get hourly data:
#' hourly.table <- read_isip_hourly_weather_data(path)
#' head(hourly.table)
#' # Get daily data:
#' daily.table <- read_isip_hourly_weather_data(path, returns.daily.data=TRUE)
#' head(daily.table)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
# ==============================================================================
read_isip_hourly_weather_data <- function(
    excel.path,
    returns.daily.data=FALSE,
    drop.irrelvant.cols=TRUE
    ){

  col.names=c("ID", "date", "Tavg", "humidity", "precipitation", "radiation", "irrelevant.radiation.2", "wind_speed")
  sheet.names <- sort(readxl::excel_sheets(excel.path))
  my.table <- tibble::tibble()
  for(sheet.name in sheet.names){
    my.location <- stringr::str_remove(sheet.name, "_.\\d+$")
    d <- readxl::read_excel(excel.path,
                            sheet=sheet.name,
                            col_names=col.names,
                            skip=1,
                            .name_repair = "minimal") %>%
      dplyr::select(-"ID") %>%
      dplyr::mutate(location=my.location) %>%
      dplyr::mutate(Tmin=as.numeric(NA)) %>%
      dplyr::mutate(Tmax=as.numeric(NA)) %>%
      dplyr::mutate(n_hours=as.integer(1)) %>%
      dplyr::select("location", "date", "Tmin", "Tavg", "Tmax", tidyselect::everything())
    if(drop.irrelvant.cols){
      d <- d %>% dplyr::select(-dplyr::starts_with("irrelevant"))
    }
    my.table <- dplyr::bind_rows(my.table, d)
  }
  if(returns.daily.data){
    my.table <- my.table %>%
      dplyr::mutate(date=as.Date(.data$date)) %>%
      dplyr::select("date", "location", tidyselect::everything()) %>%
      dplyr::group_by(.data$location, .data$date) %>%
      dplyr::summarise(
        Tmin=min(.data$Tavg),
        Tmax=max(.data$Tavg),
        Tavg=mean(.data$Tavg),
        humidity=mean(.data$humidity),
        precipitation=mean(.data$precipitation),
        radiation=mean(.data$radiation),
        wind_speed=mean(.data$wind_speed),
        n_hours=dplyr::n(),
        .groups="drop"
      ) %>%
      dplyr::select("location", "date", "Tmin", "Tavg", "Tmax", dplyr::everything())
  }
  return(my.table %>% dplyr::arrange(.data$location, .data$date))
}


# ==============================================================================
#' Adds the Cumulative Sum of Growing Degree-Days to an ISIP Weather Data Table
#'
#' The ISIP data should be loaded by function [read_isip_hourly_weather_data()].
#' For each location and each year in the data, time points (rows of the
#' table) are expected to be complete starting from 1st January to last time
#' point of interest (e.g. every hour or every day from 01 January to 30 June).
#' The data will be ordered by location and time, temporarily grouped by
#' location and year, and then the cumulative sum of growing degree-days at
#' each time point (each day or each hour) will be stored in a new column.
#' Make sure that the `daily.data` parameter is appropriately set to fit
#' your data.
#' Growing degree-days are calculated as defined in function [growing_degree_days()].
#'
#' @param isip.df (data.frame) table of weather data created with
#' function [read_isip_hourly_weather_data()]
#' @param t.ceiling (dbl) ceiling temperature value
#' @param t.base (dbl) base growth temperature
#' @param use.floor (boolean) `t.base` is also a floor temperature value
#' if `TRUE`, nothing more otherwise
#' @param values.to (chr) name of the new column to be added
#' @param daily.data (boolean) process data as daily data if `TRUE`, as hourly
#' data otherwise. Processing hourly data as daily data will likely result in
#' only NA values in the new column. The other way around will return values
#' but they will not be correct.
#' @param max.per.day (boolean) outputs the maximum per day if `TRUE`,
#' reports for each hour otherwise
#' @return (data.frame) mutated table with an additional column for the
#' cumulative sum of the growing degree-days
#' @examples
#' # ---------------------------------------------------------------------------
#' # Choose an example file with hourly weather data:
#' # ---------------------------------------------------------------------------
#' file <- "20221215_isip_hourly_weather_data_export.xlsx"
#' path <- system.file("extdata", file, package = "zeppr")
#' # ---------------------------------------------------------------------------
#' # Reads hourly data and derive results by hours with or without day summary:
#' # ---------------------------------------------------------------------------
#' # reads file and keep 10 rows (10 hours) only
#' hourly.table <- read_isip_hourly_weather_data(path)[1:5,]
#' # adds cumulative growing degree-days per row divided by 24
#' mutate_isip_weather_with_cumsum_gdd(hourly.table)
#' # keep maximum value per day
#' mutate_isip_weather_with_cumsum_gdd(hourly.table, max.per.day=TRUE)
#' # ---------------------------------------------------------------------------
#' # Reads as daily data and derive results by days with or without floor value:
#' # ---------------------------------------------------------------------------
#' # reads file and keep 10 rows (10 days) only
#' daily.table <- read_isip_hourly_weather_data(path, returns.daily.data=TRUE)[1:5,]
#' # adds cumulative growing degree-days per row
#' mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE)
#' # use floor value for min and max temperatures
#' mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE, use.floor=TRUE)
#' # ---------------------------------------------------------------------------
#' # Apply 2 times the function with magrittr pipes and tuned parameters:
#' # ---------------------------------------------------------------------------
#' library(magrittr)
#' hourly.table <- read_isip_hourly_weather_data(path)[1:48,]
#' hourly.table %>%
#'   mutate_isip_weather_with_cumsum_gdd(
#'     t.ceiling=26, t.base=6, use.floor=TRUE, values.to="cs_gdd_1") %>%
#'   mutate_isip_weather_with_cumsum_gdd(
#'     t.ceiling=26, t.base=6, use.floor=TRUE, values.to="cs_gdd_2",
#'     max.per.day=TRUE) %>%
#'   print(n=48)
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ==============================================================================
mutate_isip_weather_with_cumsum_gdd <- function(
    isip.df,
    t.ceiling=30,
    t.base=5,
    use.floor=FALSE,
    values.to="cumsum_gdd",
    daily.data=FALSE,
    max.per.day=FALSE){

  my.tmin <- isip.df$Tmin
  my.tmax <- isip.df$Tmax
  my.divider <- 1
  if(!daily.data) {
    # my.tmin <- isip.df$temperature
    # my.tmax <- isip.df$temperature
    my.tmin <- isip.df$Tavg
    my.tmax <- isip.df$Tavg
    my.divider <- 24
  }
  isip.df <- isip.df %>%
    dplyr::arrange(.data$location, .data$date) %>%
    dplyr::mutate(year.198445789832=lubridate::year(.data$date)) %>%
    dplyr::with_groups(
      # .groups = c(.data$location, .data$year.198445789832),
      .groups = c("location", "year.198445789832"),
      dplyr::mutate,
      gdd.198445789832=growing_degree_days(my.tmin, my.tmax, t.ceiling, t.base, use.floor)
    ) %>%
    dplyr::with_groups(
      # .groups = c(.data$location, .data$year.198445789832),
      .groups = c("location", "year.198445789832"),
      dplyr::mutate,
      {{values.to}}:=slider::slide_index_sum(x=.data$gdd.198445789832, i=.data$date, before=Inf, complete=TRUE)/my.divider
    ) %>%
    # dplyr::select(-.data$gdd.198445789832, -.data$year.198445789832)
    dplyr::select(-"gdd.198445789832", -"year.198445789832")

  if(max.per.day){
    isip.df <- isip.df %>%
      dplyr::mutate(day.198445789832=as.Date(.data$date)) %>%
      dplyr::with_groups(
        # .groups = c(.data$location, .data$day.198445789832),
        .groups = c("location", "day.198445789832"),
        dplyr::mutate,
        {{values.to}}:=max(.data[[values.to]])
      ) %>%
      # dplyr::select(-.data$day.198445789832)
      dplyr::select(-"day.198445789832")
  }
  return(isip.df)
}
