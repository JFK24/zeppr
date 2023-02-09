# ==============================================================================
#' Distance in kilometers between 2 points on earth
#'
#' Calculates the distance in kilometers between 2 points on earth defined by
#' their degree coordinates (decimal) using the grand sphere method
#' (at the equator, 1 longitude degree = 111 km)
#'
#' @param lat1 (dbl) latitude in degree of point 1
#' @param lat2 (dbl) latitude in degree of point 2
#' @param lon1 (dbl) longitude in degree of point 1
#' @param lon2 (dbl) longitude in degree of point 2
#' @param output_unit (chr) set to "miles" to get the distance in miles, it is in kilometers otherwise
#' @return (dbl) distance in kilometer
#' @examples
#' dist_on_earth(48.856667, 52.518611, 2.3516670, 13.4083330) # Paris - Berlin
#' dist_on_earth(40.712778, 34.052222, 74.005833, 118.243611) # New York - LA
#' dist_on_earth(40.712778, 34.052222, 74.005833, 118.243611, output_unit="miles")
#' @export
# ==============================================================================
dist_on_earth <- function(lat1, lat2, lon1, lon2, output_unit="km"){
  lon1 = lon1 * pi / 180
  lon2 = lon2 * pi / 180
  lat1 = lat1 * pi / 180
  lat2 = lat2 * pi / 180
  # Haversine formula
  dlon = lon2 - lon1
  dlat = lat2 - lat1
  a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
  c = 2 * asin(sqrt(a))
  # Radius of earth in kilometers. Use 3956 for miles
  r = 6371
  if(output_unit=="miles"){r=3956}
  return(c * r)
}


# ==============================================================================
#' Information on closer DWD station
#'
#' Identify the closest DWD station from a given point on earth and return
#' selected information about the station.
#'
#' @param lat (dbl) latitude in degree of point of interest
#' @param lon (dbl) longitude in degree of point of interest
#' @param start_date (chr) stations must have data from this date or earlier (e.g. 2022-06-30)
#' @param end_date (chr) stations must have data until this date or later (e.g. 2023-01-30)
#' @param stations_table (data.frame) table of stations info as provided by
#' functions `get_dwd_stations_info()` or `read_dwd_stations_info_file()`
#' @param return_string (chr) will return station id if set to "id",
#' name if set to "name", a string with all info separated by commas otherwise
#' @return (chr) information about the station (see parameter return_string)
#' @examples
#' # Read an example Stations Info file for Air Temperature
#' file.name.1 <- "TU_Stundenwerte_Beschreibung_Stationen.txt"
#' path.1 <- system.file("extdata", file.name.1, package = "zeppr")
#' my.stations <- read_dwd_stations_info_file(path.1)
#' closer_dwd_station(52.518611, 13.4083330, stations_table=my.stations)
#' closer_dwd_station(52.518611, 13.4083330, stations_table=my.stations, return_string = "name")
#' closer_dwd_station(52.518611, 13.4083330, stations_table=my.stations, return_string = "all")
#' @export
# ==============================================================================
closer_dwd_station <- function(lat, lon,
                               start_date="2023-01-01", end_date="2023-01-31",
                               stations_table, return_string="id"){
  my_station_table <- stations_table %>%
    dplyr::mutate(dist_km=dist_on_earth(.data$latitude, lat, .data$longitude, lon)) %>%
    dplyr::filter(.data$start_date <= {{start_date}} & .data$end_date >= {{end_date}}) %>%
    dplyr::slice_min(order_by = .data$dist_km) %>%
    dplyr::mutate(start_date=as.character(.data$start_date)) %>%
    dplyr::mutate(end_date=as.character(.data$end_date))
  return_value=paste(names(my_station_table), my_station_table[1,], sep="=", collapse = ",")
  if(return_string=="name"){return_value=my_station_table[1,]$station_name}
  if(return_string=="id"){return_value=my_station_table[1,]$station_id}
  return(return_value)
}
