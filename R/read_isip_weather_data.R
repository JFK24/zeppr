# ==============================================================================
#' Creates an Input File for ISIP Weather Data Export Service
#'
#' Creates an Excel file to be used as input for the ISIP Weather Data Export
#' Service.
#'
#' @param locations (chr) vector of location names
#' @param latitudes (dbl) vector of latitudes
#' @param longitudes (dbl) vector of longitudes
#' @param start_date (chr) starting date for weather data (format: "2021-12-31"; set to 2*365 days ago if NA)
#' @param end_date (chr) ending date for weather data (format: "2022-12-31"; set to 7 days ago if NA)
#' @param output.excel.path (chr) path to the output Excel file
#' @param overwrite (boolean) overwrite file if true, no overwrite otherwise
#' @examples
#' # Let us use an example dataset:
#' locs <- c("A", "B", "C")
#' lats <- c(50, 49, 48)
#' lons <- c(10, 9, 8)
#' # create_isip_weather_data_input_table(locs, lats, lons, "2022-01-01", "2022-12-31")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
create_isip_weather_data_input_table <- function(
    locations, latitudes, longitudes, start_date=NA, end_date=NA,
    output.excel.path="isip.input.xlsx", overwrite=FALSE){

  my.end_date <- ifelse(is.na({{end_date}}), as.character(Sys.Date()-7), {{end_date}})
  my.start_date <- ifelse(is.na({{start_date}}), as.character(Sys.Date()-365*2), {{start_date}})
  my.table <- tibble::tibble(
    Schlagname=as.character(locations),
    `geographische Breite`=as.character(latitudes),
    # "geographische L\u00E4nge"=as.character(longitudes),
    "geographische Lange"=as.character(longitudes),
    `von (inkl.)`=as.Date(my.start_date, "%Y-%m-%d"),
    `bis (exkl.)`=as.Date(my.end_date, "%Y-%m-%d"),
    Intervall="Stunde"
  ) %>%
    dplyr::distinct()
  a <- stringi::stri_enc_tonative("geographische L\u00E4nge")
  names(my.table) <- c("Schlagname", "geographische Breite", a, "von (inkl.)", "bis (exkl.)", "Intervall")
  openxlsx::write.xlsx(my.table, output.excel.path, overwrite=overwrite)
}


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
#' @param daily.data (boolean) process data as daily data if `TRUE`, as hourly
#' data otherwise. Processing hourly data as daily data will likely result in
#' only NA values in the new column. The other way around will return values
#' but they will not be correct.
#' @param max.per.day (boolean) outputs the maximum per day if `TRUE`,
#' reports for each hour otherwise (relevant only for hourly data)
#' @param start.month (integer from 1 to 12) within a year, calculations start from this given month (before this month all values are set to 0)
#' @param start.monthday (integer from 1 to 31) within the starting month of a year (`start.month`), calculations start from this given day (before this day all values are set to 0)
#' @param values.to (chr) name of the new column to be added
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
#' # reads file and keeps only 5 columns and 6 rows (6 hours)
#' hourly.table <- read_isip_hourly_weather_data(path)[1:6,1:5]
#' # adds cumulative growing degree-days per row divided by 24
#' mutate_isip_weather_with_cumsum_gdd(hourly.table)
#' # keep maximum value per day
#' mutate_isip_weather_with_cumsum_gdd(hourly.table, max.per.day=TRUE)
#' # ---------------------------------------------------------------------------
#' # Reads as daily data and derive results by days with or without floor value:
#' # ---------------------------------------------------------------------------
#' # reads file and keeps only 5 columns and 6 rows (6 days)
#' daily.table <- read_isip_hourly_weather_data(path, returns.daily.data=TRUE)[1:6,1:5]
#' # adds cumulative growing degree-days per row
#' mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE)
#' # use floor value for min and max temperatures
#' mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE, use.floor=TRUE)
#' # ---------------------------------------------------------------------------
#' # Apply 2 times the function with magrittr pipes and tuned parameters:
#' # ---------------------------------------------------------------------------
#' library(magrittr)
#' hourly.table <- read_isip_hourly_weather_data(path)[1:48,1:5]
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
    daily.data=FALSE,
    max.per.day=FALSE,
    start.month=1,
    start.monthday=1,
    values.to="cumsum_gdd"){

  isip.df <- isip.df %>%
    dplyr::arrange(.data$location, .data$date) %>%
    dplyr::mutate(year.198445789832=lubridate::year(.data$date)) %>%
    dplyr::mutate(month.198445789832=lubridate::month(.data$date)) %>%
    dplyr::mutate(day.198445789832b=lubridate::day(.data$date)) %>%
    dplyr::mutate(counting=ifelse(.data$month.198445789832<start.month, 0, 1)) %>%
    dplyr::mutate(counting=ifelse(.data$month.198445789832==start.month & .data$day.198445789832b<start.monthday, 0, .data$counting))

  if(daily.data){
    isip.df <- isip.df %>%
      dplyr::with_groups(
        .groups = c("location", "year.198445789832", "counting"),
        zeppr::mutate_cumsum_gdd,
        date=.data$date, t.min=.data$Tmin, t.max=.data$Tmax, t.ceiling=t.ceiling,
        t.base=t.base, use.floor=use.floor, hourly.data=!daily.data,
        max.per.day=max.per.day, values.to={{values.to}}
      ) %>%
      dplyr::mutate({{values.to}}:= .data[[values.to]] * .data$counting) %>%
      dplyr::select(-"year.198445789832", -"month.198445789832", -"day.198445789832b", -"counting")
  } else{
    isip.df <- isip.df %>%
      dplyr::with_groups(
        .groups = c("location", "year.198445789832", "counting"),
        zeppr::mutate_cumsum_gdd,
        date=.data$date, t.min=.data$Tavg, t.max=.data$Tavg, t.ceiling=t.ceiling,
        t.base=t.base, use.floor=use.floor, hourly.data=!daily.data,
        max.per.day=max.per.day, values.to={{values.to}}
      ) %>%
      dplyr::mutate({{values.to}}:= .data[[values.to]] * .data$counting) %>%
      dplyr::select(-"year.198445789832", -"month.198445789832", -"day.198445789832b", -"counting")
  }
  return(isip.df)
}
