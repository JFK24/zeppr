# ==============================================================================Gets EMRA Online Historical Weather Data
#' Retrieves interpolated weather data from the EMRA online service (emra.julius-kuehn.de).
#'
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
#' or from 1991-01-01 if `NA`
#' @param end_date (chr) Date formatted as YYYY-MM-DD (e.g. 2022-06-30).
#' The function will return historical data until this date if properly defined,
#' or until yesterday if `NA`
#' @return (data.frame) a table of weather data defined by the following columns:
#' gridID, lat, lon, date, tmit (Tavg 2m), tmin (Tmin 2m), tmax (Tmax 2m),
#' emin (Tmin 5cm), tfmin (min Feuchttemperatur 2m), rrsum (sum precipitation mm),
#' rfmit (avg relative humidity %), rrmax (max hourly precipitation mm),
#' rrgt10mm (n hours with precipitation > 10mm), radolansum (precipitation mm),
#' radolanmax (max hourly precipitation mm), radolangt10mm (n hours with precipitation > 10mm),
#' windmit (avg wind speed), rgmax (max hourly radiation), rgmit (avg hourly radiation),
#' sundur (sunlight duration h), bKultur01,
#' etpKultur01, etaKultur01, tschale, tapfel, tblatt
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
#' @export
# ==============================================================================
get_emra_historical_weather <- function(lat, lon, start_date=NA, end_date=NA){

  yesterday <- format(Sys.Date()-1,"%Y-%m-%d")
  start_date <- ifelse(is.na(start_date), "1991-01-01", start_date)
  end_date <- ifelse(is.na(end_date), yesterday, end_date)

  url <- paste0("https://synops.julius-kuehn.de/emra/xy/",
                lon, "/", lat, "/", start_date, "/", end_date)
  json <- httr::GET(url)
  frame <- jsonlite::fromJSON(httr::content(json, as="text"))
  frame$date <- as.Date(frame$date)
  frame$lon <- lon
  frame$lat <- lat
  frame <- dplyr::select(frame, "gridId", "lat", "lon", tidyselect::everything())
  frame <- dplyr::arrange(frame, "date")
  return(frame)
}


# ==============================================================================
#' Gets EMRA Online Predicted Weather Data
#'
#' Retrieves interpolated weather data from the EMRA online service (emra.julius-kuehn.de).
#'
#' Historical daily data is available from 1991 to yesterday.
#' Predicted hourly data is provided for today and the next 7 days.
#' Geographic coordinates must be from projection PSG::31467 (https://spatialreference.org/).
#' The daily and hourly data is provided as raster data. A raster has an area of
#' 1 x 1km2 and is defined within a grid over Germany.
#'
#' @param lat (dbl) latitude in degree of point of interest
#' @param lon (dbl) longitude in degree of point of interest
#' @return (data.frame) a table of weather data defined by the following columns:
#' date, doy (day of year), hourDB (hour), gridID, lat, lon,
#' tl (Tavg 2m), tf (Tmin 2m), rr (precipitation mm), vv (max wind speed m/s),
#' rg (max radiation J/cm),
#' bKultur01, etpKultur01, etaKultur01, tschale, tapfel, tblatt
#' See full documentation at
#' https://emra.julius-kuehn.de/dokumente/upload/b916d_EMRA_Dokumentation__Webdienste.pdf
#' @examples
#'   lon <- 13.211756
#'   lat <- 52.292031
#'   # my.table <- get_emra_predicted_weather(lat, lon)
#'   # head(my.table[,1:8])
#' @export
# ==============================================================================
get_emra_predicted_weather <- function(lat, lon){
  url <- paste0("https://synops.julius-kuehn.de/emra/prediction/xy/",lon, "/", lat)
  json <- httr::GET(url)
  frame <- jsonlite::fromJSON(httr::content(json, as="text"))
  frame$lon <- lon
  frame$lat <- lat
  frame <- dplyr::select(frame, "gridId", "lat", "lon", tidyselect::everything())
  frame <- dplyr::arrange(frame, "date", "hourDB")
  return(frame)
}
