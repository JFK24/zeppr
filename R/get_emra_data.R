# ==============================================================================
#' Gets EMRA Interpolated Historical Daily Weather Data
#'
#' EMRA is an online service (emra.julius-kuehn.de).
#' Daily disruption of service expected between 9:30 and 10:00 am (MEZ/MESZ).
#' Historical daily data is available from 1991 to yesterday.
#' Predicted hourly data is provided for today and the next 7 days.
#' Geographic coordinates must be from projection PSG::31467 (https://spatialreference.org/).
#' The daily and hourly data is provided as raster data. A raster has an area of
#' 1 x 1km2 and is defined within a grid over Germany.
#'
#' @param lat (dbl) latitude in degree of point of interest
#' @param lon (dbl) longitude in degree of point of interest
#' @param start_date (chr) Date formatted as YYYY-MM-DD (e.g. 2022-06-30).
#' The function will return historical data from this date if properly defined,
#' or from 1991-01-01 if `NA` (1991-01-01 is the first available date)
#' @param end_date (chr) Date formatted as YYYY-MM-DD (e.g. 2022-06-30).
#' The function will return historical data until this date if properly defined,
#' or until yesterday if `NA` (the date of yesterday is the last available date)
#' @return (data.frame) a table of historical daily weather data defined by the following columns
#' (aggregation over hourly data):
#' gridID, lat, lon, date,
#' tmit (Tavg, 2m, °C), tmin (Tmin, 2m, °C), tmax (Tmax, 2m, °C),
#' emin (Tmin, 5cm, °C), tfmin (min wet-bulb temperature, 2m, °C),
#' rrsum (sum precipitation from RADOLAN, mm),
#' rfmit (avg relative humidity, %),
#' rrmax (max precipitation, mm),
#' rrgt10mm (hours with precipitation > 10mm, h),
#' radolansum (sum precipitation from RADOLAN, mm),
#' radolanmax (max precipitation from RADOLAN, mm),
#' radolangt10mm (hours with precipitation from RADOLAN > 10mm, h),
#' windmit (avg wind speed, m/s),
#' rgmax (max global radiation, J/cm),
#' rgmit (avg global radiation, J/cm),
#' sundur (sum sunlight duration, h),
#' bKultur01 (sum soil humidity 0-60cm under winter wheat, %),
#' etpKultur01 (sum of potential evaporation under winter wheat, mm),
#' etaKultur01 (sum of current (or real?) evaporation under winter wheat, mm),
#' tschale (max apple skin temperature, °C), tapfel (max apple core temperature, °C),
#' tblatt  (min apple leaf temperature, °C).
#' See full documentation at
#' https://emra.julius-kuehn.de/dokumente/upload/b916d_EMRA_Dokumentation__Webdienste.pdf
#' @examples
#'   lon <- 13.211756
#'   lat <- 52.292031
#'   before_yesterday <- format(Sys.Date()-2,"%Y-%m-%d")
#'   # my.table <- get_emra_historical_weather(lat, lon, start_date=before_yesterday)
#'   # head(my.table[,1:8])
#'   # my.table <- get_emra_historical_weather(lat, lon, end_date="1991-01-02")
#'   # head(my.table[,1:8])
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
# ==============================================================================
get_emra_historical_weather <- function(lat, lon, start_date=NA, end_date=NA){
  yesterday <- format(Sys.Date()-1,"%Y-%m-%d")
  start_date <- ifelse(is.na(start_date), "1991-01-01", start_date)
  end_date <- ifelse(is.na(end_date), yesterday, end_date)
  url <- paste("https://synops.julius-kuehn.de/emra/xy", lon, lat, start_date, end_date, sep = "/")
  json_http <- httr::GET(url)
  frame <- jsonlite::fromJSON(httr::content(json_http, as="text"))
  frame$date <- as.Date(frame$date)
  frame$lon <- lon
  frame$lat <- lat
  frame <- frame %>%
    dplyr::select("gridId", "lat", "lon", tidyselect::everything()) %>%
    dplyr::arrange(.data$date)
  return(frame)
}


# ==============================================================================
#' Gets EMRA Predicted Interpolated Hourly Weather Data
#'
#' EMRA is an online service (emra.julius-kuehn.de).
#' Daily disruption of service expected between 9:30 and 10:00 am (MEZ/MESZ).
#' Historical daily data is available from 1991 to yesterday.
#' Predicted hourly data is provided for today and the next 7 days.
#' Geographic coordinates must be from projection PSG::31467 (https://spatialreference.org/).
#' The daily and hourly data is provided as raster data. A raster has an area of
#' 1 x 1km2 and is defined within a grid over Germany.
#'
#' @param lat (dbl) latitude in degree of point of interest
#' @param lon (dbl) longitude in degree of point of interest
#' @return (data.frame) a table of predicted hourly weather data defined by the following columns:
#' date, doy (day of year), hourDB (hour), gridID, lat, lon,
#' tl (Tavg, 2m, °C), tf (Tmin, 2m, °C), rr (sum precipitation, mm), vv (max wind speed, m/s),
#' rg (max global radiation, J/cm),
#' bKultur01 (sum soil humidity 0-60cm under winter wheat, %),
#' etpKultur01 (sum of potential evaporation under winter wheat, mm),
#' etaKultur01 (sum of current (or real?) evaporation under winter wheat, mm),
#' tschale (max apple skin temperature, °C), tapfel (max apple core temperature, °C),
#' tblatt  (min apple leaf temperature, °C).
#' See full documentation at
#' https://emra.julius-kuehn.de/dokumente/upload/b916d_EMRA_Dokumentation__Webdienste.pdf
#' @examples
#'   lon <- 13.211756
#'   lat <- 52.292031
#'   # my.table <- get_emra_predicted_weather(lat, lon)
#'   # head(my.table[,1:8])
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
# ==============================================================================
get_emra_predicted_weather <- function(lat, lon){
  url <- paste("https://synops.julius-kuehn.de/emra/prediction/xy",lon, lat, sep = "/")
  json_http <- httr::GET(url)
  frame <- jsonlite::fromJSON(httr::content(json_http, as="text"))
  frame$lon <- lon
  frame$lat <- lat
  frame <- frame %>%
    dplyr::select("gridId", "lat", "lon", tidyselect::everything()) %>%
    dplyr::arrange(.data$date, .data$hourDb)
  return(frame)
}
