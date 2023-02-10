# library(openxlsx)
# library(tidyverse)
#
# # ==============================================================================
# # Variables
# # ==============================================================================
# jourdelan <- paste0(lubridate::year(Sys.Date()), "-01-01")
# today <- Sys.Date() - 1
# yesterday <- today - 1
#
# stations_base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
# stations_info_url <- paste0(stations_base_url, "TU_Stundenwerte_Beschreibung_Stationen.txt")
# # station_zip_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/stundenwerte_TU_00150_akt.zip"
#
# my.input.dir <- file.path("data", "2023", "locations")
# data.path <- file.path(my.input.dir, "EntoProg_Raps_Standorte_2022_v2.xlsx")
#
# my.output.dir <- file.path("output", "2023", "locations")
# dir.create(my.output.dir, recursive = T, showWarnings = F)
# # data.path <- file.path(my.out.dir, "EntoProg_Raps_Standorte_2022_v2.xlsx")
# my.output.dir.dwd <- file.path(my.output.dir, today)
# dir.create(my.output.dir.dwd, recursive = T, showWarnings = F)
#
# locations.output.file <- file.path(my.output.dir, today, "locations.xlsx")
# data.output.file <- file.path(my.output.dir, today, "locations.and.weather.xlsx")
#
# # ==============================================================================
# # Data
# # ==============================================================================
#
# d1 <- read.xlsx(data.path, detectDates = T) %>%
#   tibble() %>%
#   filter(SchlagID!="test") %>%
#   rename(location=Bundesland) %>%
#   group_by(location) %>%
#   summarise(lat=mean(Lat), lon=mean(Lon)) %>%
#   select(location, lat, lon) %>%
#   mutate(start_date=jourdelan) %>%
#   mutate(end_date=as.character(yesterday))
# # write.xlsx(d1, file.path(my.out.dir, "EntoProg_Raps_Standorte_2022_v2_isip_weather_input.xlsx"))
#
#
# # ==============================================================================
# # FUNCTIONS
# # ==============================================================================
# read_stations_info_file <- function(path){
#   stations_info <- read_fwf(
#     path,
#     comment="--",
#     skip = 2,
#     col_types="cccdddcc",
#     col_positions=fwf_widths(c(6, 9, 9, 15, 12, 10, 41, 98)),
#     locale = locale(encoding = "latin1")
#   )
#   names(stations_info) <- c("Stations_id", "von_datum", "bis_datum", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
#   stations_info <- stations_info %>%
#     filter(!is.na(Stations_id)) %>%
#     mutate(von_datum=as.Date(von_datum, format="%Y%m%d")) %>%
#     mutate(bis_datum=as.Date(bis_datum, format="%Y%m%d"))
#   return(stations_info)
# }
#
# degrees_to_meters_dist <- function(lat1, lat2, lon1, lon2){
#   lon1 = lon1 * pi / 180
#   lon2 = lon2 * pi / 180
#   lat1 = lat1 * pi / 180
#   lat2 = lat2 * pi / 180
#   # Haversine formula
#   dlon = lon2 - lon1
#   dlat = lat2 - lat1
#   a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
#   c = 2 * asin(sqrt(a))
#   # Radius of earth in kilometers. Use 3956 for miles
#   r = 6371
#   return(c * r)
# }
#
# get_closer_station <- function(input_table, stations_table){
#   location <- input_table["location"]
#   long_coord <- as.numeric(input_table["lon"])
#   wide_coord <- as.numeric(input_table["lat"])
#   start_date <- input_table["start_date"]
#   end_date <- input_table["end_date"]
#   stations_table=stations_info_table
#
#   my_station_table <- stations_table %>%
#     mutate(location=location) %>%
#     # mutate(dist=sqrt( (geoLaenge - long_coord)^2 + (geoBreite - wide_coord)^2)) %>%
#     mutate(dist_km=degrees_to_meters_dist(geoLaenge, long_coord, geoBreite, wide_coord)) %>%
#     filter(von_datum <= start_date & bis_datum >= end_date) %>%
#     slice_min(order_by = dist_km) %>%
#     select(Stations_id, Stationsname, Bundesland, geoBreite, geoLaenge, dist_km, location)
#   # my.string=paste(as.vector(my_station_table[1,]), collapse = "-")
#   return(as.vector(my_station_table[1,]))
# }
#
#
# # ==============================================================================
# # STATION IDS
# # ==============================================================================
# stations_info_file <- tempfile()
# download.file(stations_info_url, stations_info_file)
# stations_info_table <- read_stations_info_file(stations_info_file)
# # unlink(stations_info_file)
# # stations_info_table
#
#
# # ==============================================================================
# # GET CLOSER STATION
# # ==============================================================================
# closer_stations_list <- apply(d1, 1, get_closer_station, stations_table=stations_info_table)
# closer_stations_table <- do.call(rbind.data.frame, closer_stations_list)
# d <- d1 %>% left_join(closer_stations_table, by = "location") %>%
#   mutate(url=paste0(stations_base_url, "stundenwerte_TU_", Stations_id, "_akt.zip")) %>%
#   mutate(out.file.zip=file.path(my.output.dir.dwd, paste0(Stations_id, ".zip"))) %>%
#   mutate(out.file.xls=file.path(my.output.dir.dwd, paste0(Stations_id, "_dwd_weather.xlsx")))
#
# write.xlsx(d, locations.output.file)
#
#
# # ==============================================================================
# # DOWNLOAD STATION DATA
# # ==============================================================================
# stations_base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
# stations_info_url <- paste0(stations_base_url, "TU_Stundenwerte_Beschreibung_Stationen.txt")
# # station_zip_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/stundenwerte_TU_00150_akt.zip"
# d
# my.output.dir.dwd
#
# for (i in 1:nrow(d)) {
#   my.row=d[i,]
#   message(paste0("_ Donwloading: ", my.row$out.file.zip))
#   if(!file.exists(my.row$out.file.zip)){
#     download.file(my.row$url, my.row$out.file.zip, quiet = T)
#   } else{
#     message("  _ File exists!")
#   }
# }
#
#
# # ==============================================================================
# # UNZIP AND PARSE STATION DATA
# # ==============================================================================
#
# for (i in 1:nrow(d)) {
#   my.row=d[i,]
#   station_zip_file <- my.row$out.file.zip
#   station_xls_file <- my.row$out.file.xls
#
#   message(paste0("_ Processing: ", station_zip_file))
#   intern_file_name <- (unzip(zipfile = station_zip_file, list = TRUE) %>% filter(str_starts(Name, "produkt")))$Name
#   # print(intern_file_name)
#
#   stations_data <- read_delim(
#     unz(station_zip_file, intern_file_name),
#     trim_ws = TRUE,
#     delim = ";",
#     col_types = "icdddc"
#   ) %>%
#     # mutate(MESS_DATUM=as.POSIXct(MESS_DATUM, format="%Y%m%d%H")) %>%
#     mutate(MESS_DATUM=as.POSIXct(MESS_DATUM, format="%Y%m%d%H")) %>%
#     mutate(STATIONS_ID = sprintf("%05i", STATIONS_ID)) %>%
#     rename(Stations_id=STATIONS_ID) %>%
#     rename(timestamp=MESS_DATUM) %>%
#     rename(Tavg=TT_TU) %>%
#     rename(humidity=RF_TU) %>%
#     select(-QN_9, -eor)
#
#   write.xlsx(stations_data, station_xls_file)
# }
#
# # ==============================================================================
# # MERGE DATA
# # ==============================================================================
#
# final.table <- data.frame()
# for (i in 1:nrow(d)) {
#   my.row=d[i,]
#   station_xls_file <- my.row$out.file.xls
#   message(paste0("_ Processing: ", station_xls_file))
#   if(file.exists(station_xls_file)){
#     message("  _ OK")
#
#     station_xls_data <- read.xlsx(station_xls_file) %>%
#       tibble() %>%
#       mutate(timestamp=openxlsx::convertToDateTime(timestamp)) %>%
#       mutate(bundesland=my.row$Bundesland) %>%
#       mutate(location=my.row$location) %>%
#       mutate(lat=my.row$lat) %>%
#       mutate(lon=my.row$lon) %>%
#       mutate(station_name=my.row$Stationsname) %>%
#       mutate(dist_to_station_km=my.row$dist_km) %>%
#       select(location, lat, lon, timestamp, Tavg, humidity, dist_to_station_km, Stations_id, station_name, bundesland)
#
#     final.table <- bind_rows(final.table, station_xls_data)
#
#   } else{
#     message("  _ File missing!")
#   }
# }
#
# write.xlsx(final.table, data.output.file)
