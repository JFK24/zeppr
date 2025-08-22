# ==============================================================================
#' Derives JKI internal GridIDs from given geographic points
#'
#' A JKI internal GridID for a given geographic point is a string composed
#' of the first 4 digits of the X and Y coordinates in the projection EPSG::31467.
#' It is used in various online services of JKI.
#'
#' @param x vector of x coordinates of the input geographic points
#' @param y vector of y coordinates of the input geographic points
#' @param crs (int/chr) coordinate reference system of `x` and `y`: integer with the EPSG code,
#' or character with proj4stringcrs of the input coordinates (default: 4326 for WGS 84)
#' @return (chr) JKI internal GridIDs
#' @examples
#'   get_jki_gridid(x=13.211756, y=52.292031) # expecting: 37875803
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_jki_gridid <- function(x, y, crs = 4326){
  # x <- c(13.211756, 14.211756 )
  # y <- c(52.292031, 51.292031)
  # crs <- 4326
  multipoints = sf::st_multipoint(matrix(c(x, y), ncol=2), dim = "XY")
  sfc1 = sf::st_sfc(multipoints, crs = crs)
  sfc2 = sf::st_transform(sfc1, 31467)
  ids <- sf::st_coordinates(sfc2) %>%
    as.data.frame() %>%
    # dplyr::select(tidyselect::all_of("X", "Y")) %>%
    dplyr::select(tidyselect::all_of(c("X", "Y"))) %>%
    # dplyr::mutate(gridid = paste0(substr(as.character(X), 1, 4), substr(as.character(Y), 1, 4))) %>%
    dplyr::mutate(gridid = paste0(substr(as.character(.data$X), 1, 4), substr(as.character(.data$Y), 1, 4))) %>%
    dplyr::pull("gridid")
  return(ids)
}


# ==============================================================================
#' Gets JANSEN Interpolated Daily Weather Data
#'
#' JANSEN is an online service (synops.julius-kuehn.de).
#' Daily data is available from 1990 to yesterday.
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
#' @param url (chr) API URL
#' @param retries (int) number of times to retry if an error occurs during download
#' @param sleep_time (int) number of seconds between 2 retries
#' @return (data.frame) a table of daily weather data defined by the following columns:
#' gridID (chr) (JKI grid ID),
#' lat (dbl) (latitude),
#' lon (dbl) (longitude),
#' date (date) (YYYY-MM-DD),
#' temperature (dbl) (average air temperature, °C),
#' temperatureMin (dbl) (minimum air temperature, °C),
#' temperatureMax (dbl) (maximum air temperature, °C),
#' precipitation (dbl) (sum precipitation, mm),
#' vpgsl (dbl) (potential evaporation over grass, mm)
#' zsbnas (dbl) (hours of leaf wetness, h)
#' zsu70 (dbl) (hours above 70% relative humidity, h),
#' zsu80 (dbl) (hours above 80% relative humidity, h),
#' zsu90 (dbl) (hours above 90% relative humidity, h),
#' windspeed (dbl) (daily average wind speed, m/s),
#' globalRadiation (dbl) (daily average global radiation, J/cm2),
#' tmu (dbl) (daily average relative humidity, %),
#' sd (dbl) (sum sunlight duration, h),
#' tmtd (dbl) (daily average dew point, °C),
#' radolangt10mm (dbl) (hours with precipitation from RADOLAN > 10mm, h),
#' radolanmax (dbl) (max precipitation from RADOLAN, mm),
#' radolansum (dbl)  (sum precipitation from RADOLAN, mm),
#' bf_kultur_01 (dbl) (sum soil humidity 0-60cm under winter wheat, %),
#' etp_kultur_01 (dbl) (sum of potential evaporation under winter wheat, mm),
#' eta_kultur_01 (dbl) (sum of current (or real?) evaporation under winter wheat, mm),
#' rr24 (dbl) (24h precipitation sum, from yesterday to today 6:00 UTC, mm)
#' @examples
#'   \dontrun{
#'     # Input parameters
#'     secrets <- read.delim("secrets.tsv", sep="\t")
#'     jansen_api_url <- secrets[secrets$key=="jansen_api_url",]$value
#'     before_yesterday <- format(Sys.Date()-2,"%Y-%m-%d")
#'     # Requests
#'     get_jansen_historical_weather(
#'       52.292031, 13.211756, start_date=before_yesterday,
#'       url=jansen_api_url)
#'     get_jansen_historical_weather(
#'     52.292031, 13.211756, end_date="1990-01-03",
#'     url=jansen_api_url)
#'   }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
# ==============================================================================
get_jansen_historical_weather <- function(lat, lon, start_date=NA, end_date=NA, url, retries=3, sleep_time=3){
  yesterday <- format(Sys.Date()-1,"%Y-%m-%d")
  start_date <- ifelse(is.na(start_date) | start_date<"1990-01-01", "1990-01-01", start_date)
  end_date <- ifelse(is.na(end_date) | end_date>yesterday, yesterday, end_date)
  grid_id <- get_jki_gridid(lon, lat)
  url <- paste(url, grid_id, "DE", start_date, end_date, sep = "/")
  iteration <- 1
  success <- FALSE
  frame <- data.frame()
  while(iteration<=retries & success==FALSE){
    frame <- tryCatch(
      {
        json_http <- httr::GET(url)
        jsonlite::fromJSON(httr::content(json_http, as="text"))
      },
      error = function(cond) {
        message(paste("GET JANSEN Error with:", url))
        message("Here's the original error message:")
        message(conditionMessage(cond))
        Sys.sleep(sleep_time)
        data.frame()
      }
    )
    iteration <- iteration + 1
    success <- ifelse(nrow(frame)==0, FALSE, TRUE)
  }
  if(success==TRUE){
    frame$gridId <- grid_id
    frame$lon <- lon
    frame$lat <- lat
    frame <-frame %>%
      dplyr::select("gridId", "lat", "lon", "date", tidyselect::everything()) %>%
      dplyr::select(-tidyselect::all_of("id")) %>%
      dplyr::mutate(dplyr::across(-tidyselect::all_of(c("gridId", "lat", "lon", "date")), as.numeric)) %>%
      dplyr::mutate(date=as.Date(date)) %>%
      dplyr::arrange(.data$date)
  } else{
    message(paste("Failed to get JANSEN weather data", url))
  }
  return(frame)
}
