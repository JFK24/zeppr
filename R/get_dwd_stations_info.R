# ==============================================================================
#' Get DWD Category Code
#'
#' Returns 1-2 letter code for a metrics category (e.g. TU for air_temperature,
#' and TD for dew_point).
#'
#' @param category (chr) choices: air_temperature, cloud_type, cloudiness,
#' dew_point, extreme_wind, moisture, precipitation, pressure, soil_temperature,
#' solar, sun, visibility, weather_phenomena, wind, wind_synop
#' @param lower_case (boolean) output lowercase code if TRUE, upper case otherwise
#' @return (chr) code corresponding to the supported categories, NA otherwise
#' @examples
#' dwd_category_code("air_temperature")
#' dwd_category_code("air_temperature", lower_case=TRUE)
#' @export
# ==============================================================================
dwd_category_code <- function(category, lower_case=FALSE){
  code <- NA
  if(category=="air_temperature"){code <- "TU"} else
    if(category=="cloud_type"){code <- "CS"} else
      if(category=="cloudiness"){code <- "N"} else
        if(category=="dew_point"){code <- "TD"} else
          if(category=="extreme_wind"){code <- "FX"} else
            if(category=="moisture"){code <- "TF"} else
              if(category=="precipitation"){code <- "RR"} else
                if(category=="pressure"){code <- "P0"} else
                  if(category=="soil_temperature"){code <- "EB"} else
                    if(category=="solar"){code <- "ST"} else # no "recent" subdir !
                      if(category=="sun"){code <- "SD"} else
                        if(category=="visibility"){code <- "VV"} else
                          if(category=="weather_phenomena"){code <- "WW"} else
                            if(category=="wind"){code <- "FF"} else
                              if(category=="wind_synop"){code <- "F"}
  if(!is.na(code) & lower_case){code=tolower(code)}
  return(code)
}

# ==============================================================================
#' Read DWD Stations Info File
#'
#' Reads a local file describing DWD stations downloaded from a subfolder of
#' the following URL
#' https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/
#'
#' @param path (chr) file path to a DWD Stations Info file such as
#' "TU_Stundenwerte_Beschreibung_Stationen.txt"
#' @return (data.frame) data frame of the stations info with the following
#' columns: station_id (chr), start_date (date), end_date (date), altitude (dbl),
#' latitude (dbl), longitude (dbl), station_name (chr), bundesland (chr)
#' @examples
#' # Read an example Stations Info file for Air Temperature
#' file.name.1 <- "TU_Stundenwerte_Beschreibung_Stationen.txt"
#' path.1 <- system.file("extdata", file.name.1, package = "zeppr")
#' head(read_dwd_stations_info_file(path.1))
#' # Read an example Stations Info file for Dew Point
#' file.name.2 <- "TD_Stundenwerte_Beschreibung_Stationen.txt"
#' path.2 <- system.file("extdata", file.name.2, package = "zeppr")
#' head(read_dwd_stations_info_file(path.2))
#' @export
# ==============================================================================
read_dwd_stations_info_file <- function(path){
  stations_info <- readr::read_fwf(
    path,
    comment="--",
    skip = 2,
    col_types="cccdddcc",
    col_positions=readr::fwf_widths(c(6, 9, 9, 15, 12, 10, 41, 98)),
    locale = readr::locale(encoding = "latin1")
  )
  names(stations_info) <- c("station_id", "start_date", "end_date", "altitude",
                            "latitude", "longitude", "station_name", "bundesland")
  stations_info <- stations_info %>%
    dplyr::filter(!is.na(.data$station_id)) %>%
    dplyr::mutate(start_date=as.Date(.data$start_date, format="%Y%m%d")) %>%
    dplyr::mutate(end_date=as.Date(.data$end_date, format="%Y%m%d"))
  return(stations_info)
}


# ==============================================================================
#' Get DWD Stations Info from Internet
#'
#' Download stations info file from DWD recent data (not historical) for a
#' requested metrics category (e.g. air_temperature or dew_point).
#' List of categories
#' https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/
#'
#' @param category (char) a metrics category from: air_temperature, cloud_type,
#' cloudiness, dew_point, extreme_wind, moisture, precipitation, pressure,
#' soil_temperature, solar, sun, visibility, weather_phenomena, wind, wind_synop
#' @param timerange (char) equal to "recent" (historical not supported)
#' @return (data.frame) data frame of the stations info with the following
#' columns: station_id (chr), start_date (date), end_date (date), altitude (dbl),
#' latitude (dbl), longitude (dbl), station_name (chr), bundesland (chr),
#' metrics (chr)
#' @examples
#' get_dwd_stations_info("air_temperature")
#' get_dwd_stations_info("dew_point")
#' @export
# ==============================================================================
get_dwd_stations_info <- function(category="air_temperature", timerange="recent"){
  # category="air_temperature"
  timerange="recent"
  code <- dwd_category_code(category)
  stations_info_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly"
  stations_info_url <- paste(stations_info_url, category, timerange, sep="/")
  stations_info_url <- paste0(stations_info_url, "/", code, "_Stundenwerte_Beschreibung_Stationen.txt")
  stations_info_file <- tempfile()
  utils::download.file(stations_info_url, stations_info_file)
  stations_info_table <- read_dwd_stations_info_file(stations_info_file) %>%
    dplyr::mutate(metrics=category)
  unlink(stations_info_file)
  return(stations_info_table)
}

