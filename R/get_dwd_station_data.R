# ==============================================================================
#' Read DWD Stations Data File
#'
#' Reads a local file containing DWD station data extracted from a zip file
#' downloaded from a subfolder of the following URL
#' https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/.
#' Recent and historical data are supported.
#' For example, the recent station data file related to air temperature will be found
#' from the URL above at air_temperature/recent/stundenwerte_TU_00044_akt.zip
#'
#' @param station_zip_path (`chr`) local file path to a DWD Station Data zip file
#' such as "stundenwerte_TU_00044_akt.zip"
#' @return (`data.frame`) data frame of the station data with the following
#' columns: station_id, timestamp, and a variable number of columns depending on
#' the file. All columns are of type `chr` except timestamp of type `POSIXct`.
#' @examples
#' # Read an example Station Data file for Air Temperature
#' file.name.1 <- "stundenwerte_TU_00044_akt.zip"
#' path.1 <- system.file("extdata", file.name.1, package = "zeppr")
#' head(read_dwd_station_data_file(path.1))
#' # Read an example Stations Info file for Dew Point
#' file.name.2 <- "stundenwerte_TF_00044_akt.zip"
#' path.2 <- system.file("extdata", file.name.2, package = "zeppr")
#' head(read_dwd_station_data_file(path.2))
#' # Read an example historical Station Data file for Air Temperature
#' file.name.3 <- "stundenwerte_TU_00044_20070401_20211231_hist.zip"
#' path.3 <- system.file("extdata", file.name.3, package = "zeppr")
#' head(read_dwd_station_data_file(path.3))
#' @export
# ==============================================================================
read_dwd_station_data_file <- function(station_zip_path){
  # station_zip_path <-   system.file("extdata", "stundenwerte_F_00096_akt.zip", package = "zeppr")
  intern_file_names <- (
    utils::unzip(zipfile = station_zip_path, list = TRUE)
  )$Name
  intern_data_file <- grep("produkt", intern_file_names, value = TRUE)
  intern_metadata_file <- grep(".*Param.*txt", intern_file_names, perl=TRUE, value=TRUE)

  stations_metadata <-
    utils::read.delim(
      unz(station_zip_path, intern_metadata_file),
      header = T,
      sep = ";",
      fileEncoding = "latin1",
      encoding = "latin1",
      stringsAsFactors=FALSE,
      colClasses = "character"
    ) %>%
    dplyr::filter(!stringr::str_starts(.data$Stations_ID, "Legende")) %>%
    dplyr::filter(!stringr::str_starts(.data$Stations_ID, "generiert")) %>%
    dplyr::mutate(Bis_Datum=as.character(.data$Bis_Datum)) %>%
    dplyr::group_by(.data$Parameter) %>%
    dplyr::slice_max(order_by=.data$Bis_Datum) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "berechnete Stundenwerte der ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "berechnete Stundenwerte des ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "berechnete der ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "berechnete des ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "Stundenwerte der ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "Stundenwerte des ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "Stundenwerte ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, "stdl. ")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, " \\(=Niederschlagshoehe_ind\\)")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, " ja/nein")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_remove(.data$Parameterbeschreibung, " uml.,windstill DD=00")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_replace_all(.data$Parameterbeschreibung, " ", "_")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_replace_all(.data$Parameterbeschreibung, "__", "_")) %>%
    dplyr::mutate(Parameterbeschreibung=stringr::str_replace_all(.data$Parameterbeschreibung, "[-]+", "_"))

  stations_data <-
    utils::read.delim(
      unz(station_zip_path, intern_data_file),
      header = T,
      sep = ";",
      fileEncoding = "latin1",
      encoding = "latin1",
      stringsAsFactors=FALSE,
      colClasses = "character"
    ) %>%
    dplyr::mutate(MESS_DATUM=as.POSIXct(.data$MESS_DATUM, format="%Y%m%d%H")) %>%
    dplyr::mutate(STATIONS_ID = sprintf("%05i", as.numeric(.data$STATIONS_ID))) %>%
    dplyr::rename(station_id="STATIONS_ID") %>%
    dplyr::rename(timestamp="MESS_DATUM") %>%
    dplyr::select(-"eor") %>%
    dplyr::select(-tidyselect::starts_with("QN")) %>%
    dplyr::mutate_at(stations_metadata$Parameter, trimws) %>%
    dplyr::mutate_at(stations_metadata$Parameter, function(x) ifelse(x=="-999", NA, x) )

  setdiff_res <- setdiff(names(stations_data), stations_metadata$Parameter)
  map <- stats::setNames(c(setdiff_res, stations_metadata$Parameterbeschreibung),
                 c(setdiff_res, stations_metadata$Parameter))
  names(stations_data) <- as.vector(map[names(stations_data)])
  return(stations_data)
}

#
# names(read_dwd_station_data_file(system.file("extdata", "stundenwerte_EB_00044_akt.zip", package = "zeppr")))
# names(read_dwd_station_data_file(system.file("extdata", "stundenwerte_RR_00044_akt.zip", package = "zeppr")))
# names(read_dwd_station_data_file(system.file("extdata", "stundenwerte_TD_00044_akt.zip", package = "zeppr")))
# names(read_dwd_station_data_file(system.file("extdata", "stundenwerte_TF_00044_akt.zip", package = "zeppr")))
# names(read_dwd_station_data_file(system.file("extdata", "stundenwerte_TU_00044_akt.zip", package = "zeppr")))
# names(read_dwd_station_data_file(system.file("extdata", "stundenwerte_FF_00011_akt.zip", package = "zeppr")))
#
# file_names <- c(
# "stundenwerte_CS_00096_akt.zip",
# "stundenwerte_N_00096_akt.zip",
# "stundenwerte_TD_00044_akt.zip",
# "stundenwerte_EB_00044_akt.zip",
# "stundenwerte_P0_00096_akt.zip",
# "stundenwerte_TF_00044_akt.zip",
# "stundenwerte_FF_00011_akt.zip",
# "stundenwerte_RR_00044_akt.zip",
# "stundenwerte_TU_00044_akt.zip",
# "stundenwerte_FX_00011_akt.zip",
# "stundenwerte_SD_00044_akt.zip",
# "stundenwerte_VV_00096_akt.zip",
# "stundenwerte_F_00096_akt.zip",
# "stundenwerte_ST_00183_row.zip",
# "stundenwerte_WW_00096_akt.zip"
# )
# for (f in file_names) {
#   print("===========================================================")
#   print(f)
#   # print(names(read_dwd_station_data_file(system.file("extdata", f, package = "zeppr"))))
#   print(
#     read_dwd_station_data_file(system.file("extdata", f, package = "zeppr")) %>%
#       tibble::tibble() %>% head()
#     )
#
# }


# ==============================================================================
#' Get DWD Station Data from Internet
#'
#' Retrieve data of a weather station of the German Weather Service (DWD)
#' from its FTP site for a requested metrics category
#' (e.g. air_temperature or dew_point). The original data file is not saved
#' locally.
#'
#' @param station_id (`chr`) DWD station ID as 5-character string e.g. "00044"
#' @param category (`chr`) a metrics category from: air_temperature, cloud_type,
#' cloudiness, dew_point, extreme_wind, moisture, precipitation, pressure,
#' soil_temperature, solar, sun, visibility, weather_phenomena, wind, wind_synop
#' @param timerange (`chr`) either "recent" or "historical"
#' @return (`data.frame`) data frame of the stations data or an empty data frame
#'  if download failed. The data frame has the following columns:
#'  station_id, timestamp, and a variable number of columns depending on the file.
#' All columns are of type chr except timestamp of type `POSIXct`.
#' @examples
#' head(get_dwd_station_data(station_id="00044", category="air_temperature"))
#' head(get_dwd_station_data(station_id="00183", category="solar"))
#' head(get_dwd_station_data(station_id="00044", category="dew_point", timerange="historical"))
#' @export
# ==============================================================================
get_dwd_station_data <- function(station_id="00044", category="air_temperature", timerange="recent"){
  # category="air_temperature"
  # station_id="00044"
  # timerange="recent"
  # timerange="historical"
  # category="solar"
  # station_id="00183"
  code <- dwd_category_code(category)
  if(is.na(code)){
    message("get_dwd_station_data(): unsupported category!")
    message(paste("parametrers: ", station_id, category))
    return(data.frame())
  }
  suffix <- ifelse(timerange=="recent", "akt", "hist")

  station_data_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly"
  station_data_url <- ifelse(
    category=="solar",
    paste(station_data_url, category, sep="/"),
    paste(station_data_url, category, timerange, sep="/")
  )
  web.page <- RCurl::getURL(paste0(station_data_url, "/"),verbose=F)
  ftp.files <- XML::getHTMLLinks(web.page)
  station.file.name <- grep(paste(code, station_id, sep="_"), ftp.files, value = T)[1]
  station_data_url <- paste0(station_data_url, "/", station.file.name)
  print(station_data_url)

  stations_data_file <- tempfile()
  out <- tryCatch(
    expr={utils::download.file(station_data_url, stations_data_file, quiet = TRUE)},
    error=function(e) {
      download_success <- FALSE
      message("get_dwd_station_data(): download failed!")
      message(paste("parametrers: ", station_id, category))
      message(e)
      return(data.frame())
    }    ,
    warning=function(w) {
      message("get_dwd_station_data(): download may have failed!")
      message(paste("parametrers: ", station_id, category))
      message(w)
      return(data.frame())
    }
    )
  if(is.na(out)) {return(data.frame())}
  stations_data_table <- read_dwd_station_data_file(stations_data_file)
  unlink(stations_data_file)
  return(stations_data_table)
}

