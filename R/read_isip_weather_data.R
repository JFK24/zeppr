# ______________________________________________________________________________
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
#' @param daily (boolean) returns daily data if TRUE, hourly data otherwise
#' @param output.excel.path (chr) path to the output Excel file
#' @param overwrite (boolean) overwrite file if true, no overwrite otherwise
#' @examples
#' # Let us use an example dataset:
#' locs <- c("A", "B", "C")
#' lats <- c(50, 49, 48)
#' lons <- c(10, 9, 8)
#' # create_isip_weather_data_input_table(locs, lats, lons, "2022-01-01", "2022-01-10")
#' # create_isip_weather_data_input_table(locs, lats, lons, "2022-01-01", "2022-01-10", daily=T)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
create_isip_weather_data_input_table <- function(
    locations, latitudes, longitudes, start_date=NA, end_date=NA, daily=FALSE,
    output.excel.path="isip.input.xlsx", overwrite=FALSE){

  my.end_date <- ifelse(is.na({{end_date}}), as.character(Sys.Date()-7), {{end_date}})
  my.start_date <- ifelse(is.na({{start_date}}), as.character(Sys.Date()-365*2), {{start_date}})
  my.intervall <- ifelse(daily, "Tag", "Stunde")
  my.table <- tibble::tibble(
    Schlagname=as.character(locations),
    `geographische Breite`=as.character(latitudes),
    # "geographische L\u00E4nge"=as.character(longitudes),
    "geographische Lange"=as.character(longitudes),
    `von (inkl.)`=as.Date(my.start_date, "%Y-%m-%d"),
    `bis (exkl.)`=as.Date(my.end_date, "%Y-%m-%d"),
    Intervall=my.intervall
  ) %>%
    dplyr::distinct()
  a <- stringi::stri_enc_tonative("geographische L\u00E4nge")
  names(my.table) <- c("Schlagname", "geographische Breite", a, "von (inkl.)", "bis (exkl.)", "Intervall")
  openxlsx::write.xlsx(my.table, output.excel.path, overwrite=overwrite)
}


# ______________________________________________________________________________
#' Reads ISIP Weather Data File
#'
#' Reads an ISIP hourly or daily weather export data file (Excel file;
#' one row per hour or per day, respectively)
#' and returns a data frame corresponding data. The function can convert hourly
#' to daily data. In this case, the daily data is derived from the hourly data
#' by averaging or summing up the values per day (sum for precipitation and
#' radiation, average otherwise). Minimum and maximum daily temperatures are
#' derived from this calculation and are not available directly from the file.
#' All sheets from the Excel file are processed and gathered in the same table.
#' Within an ISIP Excel file, each sheet represents a particular geographical
#' location and contains weather data for the same date (or time) range.
#' Sheet names provide the location ids after trimming a suffix
#' starting from the last underscore character.
#'
#' @param excel.path (chr) path to ISIP weather export data file (Excel
#' file). ISIP's Excel Data format as of Dec 2022.
#' @param read.daily.data (boolean) input file is expected to contain daily data
#' if TRUE, hourly data otherwise
#' @param hourly.to.daily (boolean) returns daily data if `TRUE`,
#' hourly data otherwise
#' @param drop.irrelvant.cols (boolean) remove irrelevant columns if `TRUE`,
#' keep all columns otherwise (columns whose name start with 'irrelevant')
#' @param col.names (chr) vector of column names for the output table
#' (must match file content)
#' @return (data.frame) table with numerical columns except for location (`chr`)
#' and date (`Date` class for daily output or `POSIXct` class for hourly
#' output). Columns: `location`, `date`, `Tmin` (min daily temperature),
#' `Tavg` (average temperature), `Tmax` (max daily temperature), `humidity`,
#' `precipitation`, `radiation`, `wind_speed` and
#' `n_hours` (scope of the data as number of hours, e.g. used to calculate averages).
#' @examples
#' # Let us use an example hourly dataset:
#' file.name <- "20221215_isip_hourly_weather_data_export.xlsx"
#' path <- system.file("extdata", file.name, package = "zeppr")
#' # Read hourly data:
#' hourly.table <- read_isip_weather_data(path)
#' head(hourly.table)
#' # Read hourly data but summarise it as daily data:
#' daily.table <- read_isip_weather_data(path, hourly.to.daily=TRUE)
#' head(daily.table)
#' # Let us use an example daily dataset:
#' file.name.2 <- "20230330_isip_daily_weather_data_export.xlsx"
#' path.2 <- system.file("extdata", file.name.2, package = "zeppr")
#' # Read daily data:
#' daily.table.2 <- read_isip_weather_data(path.2, read.daily.data=TRUE)
#' head(daily.table.2)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
# ______________________________________________________________________________
read_isip_weather_data <- function(
    excel.path,
    read.daily.data=FALSE,
    hourly.to.daily=FALSE,
    drop.irrelvant.cols=TRUE,
    col.names=c("ID", "date", "Tavg", "humidity", "precipitation", "radiation", "irrelevant", "wind_speed")
    ){

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
  if(read.daily.data){
    my.table <- my.table %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::mutate(n_hours = 24)
  }

  if(!read.daily.data & hourly.to.daily){
    my.table <- my.table %>%
      dplyr::mutate(date=as.Date(.data$date)) %>%
      # dplyr::select("date", "location", tidyselect::everything()) %>%
      dplyr::group_by(.data$location, .data$date) %>%
      dplyr::summarise(
        Tmin=round(min(.data$Tavg), 2),
        Tmax=round(max(.data$Tavg), 2),
        Tavg=round(mean(.data$Tavg), 2),
        humidity=round(mean(.data$humidity), 2),
        precipitation=round(sum(.data$precipitation), 2),
        radiation=round(sum(.data$radiation), 2),
        wind_speed=round(mean(.data$wind_speed), 2),
        n_hours=dplyr::n(),
        .groups="drop"
      ) %>%
      dplyr::select("location", "date", "Tmin", "Tavg", "Tmax", dplyr::everything())
  }
  return(my.table %>% dplyr::arrange(.data$location, .data$date))
}


# ______________________________________________________________________________
#' Copies ISIP Weather Data from Excel to TSV or TSV.GZ file
#'
#' Reads an ISIP hourly or daily weather export data file (Excel file;
#' one row per hour or per day, respectively)
#' and writes the resulting data frame into a TSV or TSV.GZ file.
#' This function is based on function `read_isip_weather_data()`
#' (see documentation).
#' Thus, same options are available when reading the input file.
#' E.g., it can convert hourly to daily data.
#' The new file can be read with function `read_tsv` from package `readr` and
#' the resulting data frame will be compatible with functions dedicated to
#' ISIP weather data in ZEPPR (e.g. `mutate_isip_weather_with_cumsum_gdd()`).
#'
#' @param input.excel.path (chr) path to ISIP weather export data file (Excel
#' file). ISIP's Excel Data format as of Dec 2022.
#' @param output.tsv.path (chr) path to output TSV file
#' @param append If FALSE, will overwrite existing file. If TRUE, will append
#' to existing file. In both cases, if the file does not exist a new file is created.
#' @param read.daily.data (boolean) input file is expected to contain daily data
#' if TRUE, hourly data otherwise
#' @param hourly.to.daily (boolean) returns daily data if `TRUE`,
#' hourly data otherwise
#' @param drop.irrelvant.cols (boolean) remove irrelevant columns if `TRUE`,
#' keep all columns otherwise (columns whose name start with 'irrelevant')
#' @param col.names (chr) vector of column names for the output table
#' (must match file content)
#' @return (data.frame) table with numerical columns except for location (`chr`)
#' and date (`Date` class for daily output or `POSIXct` class for hourly
#' output). Columns: `location`, `date`, `Tmin` (min daily temperature),
#' `Tavg` (average temperature), `Tmax` (max daily temperature), `humidity`,
#' `precipitation`, `radiation`, `wind_speed` and
#' `n_hours` (scope of the data as number of hours, e.g. used to calculate averages).
#' @examples
#' # Let us use example hourly and daily datasets:
#' file.name.1 <- "20221215_isip_hourly_weather_data_export.xlsx"
#' file.name.2 <- "20230330_isip_daily_weather_data_export.xlsx"
#' path.1 <- system.file("extdata", file.name.1, package = "zeppr")
#' path.2 <- system.file("extdata", file.name.2, package = "zeppr")
#' out.path.1 <- tempfile(fileext = ".tsv.gz")
#' out.path.2 <- tempfile(fileext = ".tsv.gz")
#' # Copy hourly data to TSV:
#' copy_isip_weather_data_to_tsv(path.1, out.path.1)
#' # Copy daily data to TSV:
#' copy_isip_weather_data_to_tsv(path.2, out.path.2, read.daily.data=TRUE)
#' # Read hourly data but write daily data:
#' copy_isip_weather_data_to_tsv(path.1, out.path.2, hourly.to.daily=TRUE)
#' # Read hourly data:
#' hourly.table <- readr::read_tsv(out.path.1, show_col_types = FALSE)
#' head(hourly.table)
#' # Read daily data:
#' daily.table <- readr::read_tsv(out.path.2, show_col_types = FALSE)
#' head(daily.table)
#' @export
# ______________________________________________________________________________
copy_isip_weather_data_to_tsv <- function(
    input.excel.path,
    output.tsv.path,
    append=FALSE,
    read.daily.data=FALSE,
    hourly.to.daily=FALSE,
    drop.irrelvant.cols=TRUE,
    col.names=c("ID", "date", "Tavg", "humidity", "precipitation", "radiation", "irrelevant", "wind_speed")
){
  isip.data <- read_isip_weather_data(
    excel.path=input.excel.path,
    read.daily.data=read.daily.data,
    hourly.to.daily=hourly.to.daily,
    drop.irrelvant.cols=drop.irrelvant.cols,
    col.names=col.names
    )
  readr::write_tsv(isip.data, output.tsv.path, append=append)
}


# ______________________________________________________________________________
#' Adds the Cumulative Sum of Growing Degree-Days to an ISIP Weather Data Table
#'
#' The ISIP data should be loaded by function [read_isip_weather_data()].
#' For each location and each year in the data, time points (rows of the
#' table) are expected to be complete starting from 1st January to last time
#' point of interest (e.g. every hour or every day from 01 January to 30 June).
#' The data will be ordered by location and time, temporarily grouped by
#' location and year, and then the cumulative sum of growing degree-days at
#' each time point (each day or each hour) will be stored in a new column.
#' If the data has min and max temperatures, they are used for the calculation,
#' otherwise the given average temperature is used.
#' Make sure that the `daily.data` parameter is appropriately set to fit
#' your data.
#' Growing degree-days are calculated as defined in function [growing_degree_days()].
#'
#' @param isip.df (data.frame) table of weather data created with
#' function [read_isip_weather_data()]
#' @param t.ceiling (dbl) ceiling temperature value
#' @param t.base (dbl) base growth temperature
#' @param use.floor (boolean) `t.base` is also a floor temperature value
#' if `TRUE`, nothing more otherwise
#' @param daily.data (boolean) process data as daily data if `TRUE`, as hourly
#' data otherwise. Processing hourly data as daily data will likely result in
#' only NA values in the new column. The other way around will return values
#' but they will not be correct.
# #' @param max.per.day (boolean) outputs the maximum per day if `TRUE`,
# #' reports for each hour otherwise (relevant only for hourly data)
#' @param start.month (integer from 1 to 12) within a year, calculations start from this given month (before this month all values are set to 0)
#' @param start.monthday (integer from 1 to 31) within the starting month of a year (`start.month`), calculations start from this given day (before this day all values are set to 0)
#' @param values.to (chr) name of the new column to be added
#' @param show.warnings (boolean) shown warnings if TRUE, nothing otherwise.
#' There is a warning if a daily data has no min temperature. It is expected for
#' daily ISIP data files but not for hourly ISIP data files that were converted
#' to daily data.
#' @return (data.frame) mutated table with an additional column for the
#' cumulative sum of the growing degree-days
#' @examples
#' # ---------------------------------------------------------------------------
#' # Choose an example file with hourly weather data:
#' # ---------------------------------------------------------------------------
#' file <- "20221215_isip_hourly_weather_data_export.xlsx"
#' path <- system.file("extdata", file, package = "zeppr")
#' # reads file and keeps only 5 columns and 6 rows (6 hours) as example
#' hourly.table <- read_isip_weather_data(path)[1:6,1:5]
#' # adds cumulative growing degree-days per row divided by 24
#' mutate_isip_weather_with_cumsum_gdd(hourly.table)
#'
#' # ---------------------------------------------------------------------------
#' # Reads as daily data and derive results by days with or without floor value:
#' # ---------------------------------------------------------------------------
#' # reads file and keeps only 5 columns and 6 rows (6 days)
#' daily.table <- read_isip_weather_data(path, hourly.to.daily=TRUE)[1:6,1:5]
#' # adds cumulative growing degree-days per row
#' mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE)
#' # use floor value for min and max temperatures
#' mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE, use.floor=TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Reads daily data file without Tmin and Tmax values and apply the function:
#' # ---------------------------------------------------------------------------
#' # reads file and keeps only 5 columns and 6 rows (6 days)
#' file.name.2 <- "20230330_isip_daily_weather_data_export.xlsx"
#' path.2 <- system.file("extdata", file.name.2, package = "zeppr")
#' daily.table <- read_isip_weather_data(path.2, read.daily=TRUE)[1:6,1:5]
#' # adds cumulative growing degree-days per row
#' mutate_isip_weather_with_cumsum_gdd(daily.table, daily.data=TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Apply 2 times the function with magrittr pipes and tuned parameters:
#' # ---------------------------------------------------------------------------
#' library(magrittr)
#' hourly.table <- read_isip_weather_data(path)[1:48,1:5]
#' hourly.table %>%
#'   mutate_isip_weather_with_cumsum_gdd(
#'     t.ceiling=26, t.base=6, use.floor=TRUE, values.to="cs_gdd_1") %>%
#'   mutate_isip_weather_with_cumsum_gdd(
#'     t.ceiling=30, t.base=4, use.floor=TRUE, values.to="cs_gdd_2") %>%
#'   print(n=48)
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ______________________________________________________________________________
mutate_isip_weather_with_cumsum_gdd <- function(
    isip.df,
    t.ceiling=30,
    t.base=5,
    use.floor=FALSE,
    daily.data=FALSE,
    # max.per.day=FALSE,
    start.month=1,
    start.monthday=1,
    values.to="cumsum_gdd",
    show.warnings=TRUE){

  isip.df <- isip.df %>%
    dplyr::arrange(.data$location, .data$date) %>%
    dplyr::mutate(year.198445789832=lubridate::year(.data$date)) %>%
    dplyr::mutate(month.198445789832=lubridate::month(.data$date)) %>%
    dplyr::mutate(day.198445789832b=lubridate::day(.data$date)) %>%
    dplyr::mutate(counting=ifelse(.data$month.198445789832<start.month, 0, 1)) %>%
    dplyr::mutate(counting=ifelse(.data$month.198445789832==start.month & .data$day.198445789832b<start.monthday, 0, .data$counting))

  # detect NAs in Tmin or Tmax
  noTMinValues <- ifelse(length(which(is.na(isip.df$Tmin)))==nrow(isip.df), TRUE, FALSE)

  if(daily.data & !noTMinValues){
    isip.df <- isip.df %>%
      dplyr::with_groups(
        .groups = c("location", "year.198445789832", "counting"),
        zeppr::mutate_cumsum_gdd,
        date=.data$date, t.min=.data$Tmin, t.max=.data$Tmax, t.ceiling=t.ceiling,
        t.base=t.base, use.floor=use.floor, hourly.data=!daily.data,
        # max.per.day=max.per.day,
        values.to={{values.to}}
      ) %>%
      dplyr::mutate({{values.to}}:= round(.data[[values.to]] * .data$counting, 2)) %>%
      dplyr::select(-"year.198445789832", -"month.198445789832", -"day.198445789832b", -"counting")
  } else if(daily.data & noTMinValues){
    if(noTMinValues & show.warnings){message("zeppr::mutate_isip_weather_with_cumsum_gdd: no Tmin values")}
    isip.df <- isip.df %>%
      dplyr::with_groups(
        .groups = c("location", "year.198445789832", "counting"),
        zeppr::mutate_cumsum_gdd,
        date=.data$date, t.min=.data$Tavg, t.max=.data$Tavg, t.ceiling=t.ceiling,
        t.base=t.base, use.floor=use.floor, hourly.data=!daily.data,
        # max.per.day=max.per.day,
        values.to={{values.to}}
      ) %>%
      dplyr::mutate({{values.to}}:= round(.data[[values.to]] * .data$counting, 2)) %>%
      dplyr::select(-"year.198445789832", -"month.198445789832", -"day.198445789832b", -"counting")
  } else{
    isip.df <- isip.df %>%
      dplyr::with_groups(
        .groups = c("location", "year.198445789832", "counting"),
        zeppr::mutate_cumsum_gdd,
        date=.data$date, t.min=.data$Tavg, t.max=.data$Tavg, t.ceiling=t.ceiling,
        t.base=t.base, use.floor=use.floor, hourly.data=!daily.data,
        # max.per.day=max.per.day,
        values.to={{values.to}}
      ) %>%
      dplyr::mutate({{values.to}}:= round(.data[[values.to]] * .data$counting, 2)) %>%
      dplyr::select(-"year.198445789832", -"month.198445789832", -"day.198445789832b", -"counting")
  }
  return(isip.df)
}
