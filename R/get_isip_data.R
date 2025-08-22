# ==============================================================================
#' Gets Token from ISIP Charts API
#'
#' Retrieves online a token to access the ISIP Charts API.
#' If already locally stored, reloads and returns an existing token.
#' Valid credentials are required to get a token!
#'
#' @param user (chr) API user name
#' @param passwd (chr) API password
#' @param url (chr) API URL
#' @param path (chr) directory path to store the token for reuse,
#' the working directory if `path` is `NA`.
#' @param file_name (chr) output file name (default: ISIP.charts.token.txt)
#' @param verbose (bool) prints messages if TRUE, nothing otherwise
#' @return (chr) a 24h-valid token
#' @examples
#'   \dontrun{
#'     # Required credentials and URL are stored in a local file
#'     secrets <- read.delim("secrets.tsv", sep="\t")
#'     user <- secrets[secrets$key=="charts_api_user",]$value
#'     passwd <- secrets[secrets$key=="charts_api_pass",]$value
#'     charts_api_token_url <- secrets[secrets$key=="charts_api_token_url",]$value
#'     token <- get_isip_charts_token(
#'       user=user, passwd=passwd, url=charts_api_token_url, verbose = TRUE)
#'   }
#' @export
# ==============================================================================
get_isip_charts_token <- function(user, passwd, url, path = NA, file_name = "ISIP.charts.token.txt", verbose = FALSE) {
  token_file <- file_name
  if(is.na(path)){
    path <- getwd()
  } else{
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    token_file <- file.path(path, file_name)
  }
  # If file exists
  if (file.exists(token_file)) {
    text <- readLines(token_file, n = 1)
    text <- strsplit(text, " ")[[1]]
    expire <- text[2]
    now <- as.numeric(Sys.time()) * 1000

    if (now < as.numeric(expire)) {
      token <- text[1]
      if(verbose){message(paste('token from local file:', token))}
      return(token)
    }
  }
  # If file does not exists
  headers <- c('Content-Type' = 'application/json')
  access_data <- list(username = user, password = passwd)
  response <- httr::POST(
    url = url,
    body = jsonlite::toJSON(access_data, auto_unbox = TRUE),
    httr::add_headers(.headers = headers)
  )
  if (httr::status_code(response) == 200) {
    response_data <- httr::content(response, "parsed")
    if(verbose){message(paste('token: ', response_data$token))}
    token <- response_data$token
    expire <- response_data$expires

    # Write token to file
    writeLines(paste(token, expire), token_file)
  } else {
    if(verbose){message(paste('error requesting token', httr::content(response)))}
    stop("Failed to get token")
  }
  return(token)
}


# ==============================================================================
#' Gets ISIP Interpolated Hourly Weather Data
#'
#' ISIP is an online service (isip.de).
#' Hourly data is available from past dates today and the next2 days.
#' The hourly data is provided as raster data. A raster has an area of
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
#' @param name (chr) location name
#' @param token (chr) Valid API token retrieved from `get_isip_charts_token`
#' @param url (chr) API URL
#' @param retries (int) number of times to retry if an error occurs during download
#' @param sleep_time (int) number of seconds between 2 retries
#' @return (data.frame) a table of hourly weather data defined by the following columns:
#' id (chr) (the input `name` value),
#' lat (dbl) (latitude),
#' lon (dbl) (longitude),
#' date_time (chr) (e.g. 2022-04-03T00:00:00),
#' TAIR200 (dbl) (average air temperature, °C),
#' HUMAIR200 (dbl) (daily average relative humidity, %),
#' GRH200 (dbl) (daily sum global radiation, W/m2),
#' PRECIP100 (dbl) (hourly sum precipitation, mm)
#' @examples
#'   \dontrun{
#'     # Get token
#'     secrets <- read.delim("secrets.tsv", sep="\t")
#'     user <- secrets[secrets$key=="charts_api_user",]$value
#'     passwd <- secrets[secrets$key=="charts_api_pass",]$value
#'     charts_api_token_url <- secrets[secrets$key=="charts_api_token_url",]$value
#'     token <- get_isip_charts_token(
#'       user=user, passwd=passwd, url=charts_api_token_url, verbose = TRUE)
#'
#'     # Input parameters
#'     charts_api_server_url <- secrets[secrets$key=="charts_api_server_url",]$value
#'     field_name <- 'test_near_ingelheim'
#'     lat <- '49.998521'
#'     lon <- '8.070207'
#'     start_date <- '2022-04-03T00:00'
#'     end_date <- '2025-04-04T00:00'
#'
#'     # Request hourly values
#'     weather_data <- get_isip_historical_weather(
#'       lat=lat, lon=lon, start_date=start_date, end_date=end_date, name=field_name,
#'       token=token, url=charts_api_server_url
#'     )
#'   }
#' @export
# ==============================================================================
get_isip_historical_weather <- function(lat, lon, start_date=NA, end_date=NA, name="Test", token, url, retries=3, sleep_time=3){
  # name="Test"
  # token=token
  # url=charts_api_server_url
  yesterday <- format(Sys.Date()-1,"%Y-%m-%d")
  start_date <- ifelse(is.na(start_date) | start_date<"1990-01-01", "1990-01-01", start_date)
  end_date <- ifelse(is.na(end_date) | end_date>yesterday, yesterday, end_date)
  error <- 0

  url <- paste0(url, "?")
  url <- paste0(url, 'field_name=', name)
  url <- paste0(url, '&longitude=', lon)
  url <- paste0(url, '&latitude=', lat)
  url <- paste0(url, '&sensor_ids=TAIR200,HUMAIR200,GRH200,PRECIP100,WSPE250')
  url <- paste0(url, '&from_incl=', start_date)
  url <- paste0(url, '&to_excl=', end_date)
  url <- paste0(url, '&interval=hour')
  url <- paste0(url, '&pld=', token)

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
    success <- ifelse(length(frame)==0, FALSE, TRUE)
  }
  if(success==TRUE){
    if (httr::status_code(json_http) == 200) {
      tryCatch({
        response_data <- httr::content(json_http, "parsed")
        datetime <- response_data$nodes$result$nodes$hour$variables$hour$data
        TAIR200 <- response_data$nodes$result$nodes$hour$variables$TAIR200$data
        HUMAIR200 <- response_data$nodes$result$nodes$hour$variables$HUMAIR200$data
        GRH200 <- response_data$nodes$result$nodes$hour$variables$GRH200$data
        PRECIP100 <- response_data$nodes$result$nodes$hour$variables$PRECIP100$data
      }, error = function(e) {
        error <<- 1
      })
    } else {
      error <- 1
    }
    if (error == 1) {
      print(paste('error requesting weather data', url))
    }
    return(
      data.frame(
        id=name,
        lat=lat,
        lon=lon,
        date_time=unlist(datetime),
        TAIR200=unlist(TAIR200),
        HUMAIR200=unlist(HUMAIR200),
        GRH200=unlist(GRH200),
        PRECIP100=unlist(PRECIP100)
      )
    )
  }
  message("get_isip_historical_weather: ERROR => empty output")
  return(data.frame)
}


