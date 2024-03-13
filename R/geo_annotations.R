# use_package("zip", min_version = TRUE)

#' Downloads and optionally decompresses a ZIP file
#' @param target.url URL of the ZIP file
#' @param out.dir output directory (will be created if not exists)
#' @param overwrite download and overwrite existing file if TRUE, do nothing otherwise
#' @param unzip descompress the zip file if TRUE, do not decompress otherwise
#' @examples
#' # # Download NUTS files
#' # my.nuts.url=
#' # "https://gisco-services.ec.europa.eu/distribution/v2/nuts/shp/NUTS_RG_01M_2021_4326.shp.zip"
#' # download_and_unzip(my.nuts.url, out.dir="tmp/nuts", overwrite=FALSE, unzip=TRUE)
#' # # Download GRID file
#' # my.grid.url="https://gisco-services.ec.europa.eu/grid/grid_100km_surf.gpkg"
#' # download_and_unzip(my.grid.url, out.dir="tmp/grids", overwrite=FALSE, unzip=FALSE)
#' @export
download_and_unzip <- function(target.url, out.dir, overwrite=FALSE, unzip=TRUE){
  output.path <- file.path(out.dir, basename(target.url))
  if(unzip){output.path <- tools::file_path_sans_ext(output.path)}
  if(!file.exists(output.path) | overwrite==TRUE){
    dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
    my.dest.file <- tempfile("entoprog_temp_")
    utils::download.file(target.url, my.dest.file)
    if(unzip){
      zip::unzip(zipfile=my.dest.file, exdir=out.dir, junkpaths = TRUE)
    } else{
      file.copy(from=my.dest.file, to=output.path, overwrite=overwrite)
    }
    status <- file.remove(my.dest.file)
  }
}


#' gets European administrative area names for given geographic points.
#' Source 2021 shape files to be downloaded from
#' https://gisco-services.ec.europa.eu/distribution/v2/nuts/shp/NUTS_RG_01M_2021_4326.shp.zip
#' and decompressed
#' @param lat (numeric) vector of latitudes of the geographic point
#' @param lon (numeric) vector of longitudes of the geographic point
#' @param level (numeric) NUTS annotation level equal to 0, 1, 2 or 3.
#' Depending on the country, the level fits approximately to the following entities:
#' 0 for country, 1 for region/state, 2 for district/state, 3 for communalities/district
#' @param german.iso.codes (boolean) for NUTS level 1 in Germany, returns the 2-letter German ISO codes if TRUE, full federal state names otherwise
#' @param nuts.path (chr) local non-compressed NUTS shape file path (e.g. NUTS_RG_01M_2021_4326.shp)
#' @examples
#' # # Download 2021 NUTS files in temporary folder
#' # my.nuts.url=
#' # "https://gisco-services.ec.europa.eu/distribution/v2/nuts/shp/NUTS_RG_01M_2021_4326.shp.zip"
#' # download_and_unzip(my.nuts.url, out.dir="tmp/nuts", overwrite=FALSE, unzip=TRUE)
#' # # Use local NUTS files
#' # my.nuts.path="tmp/nuts/NUTS_RG_01M_2021_4326.shp"
#' # map_nuts_name(lat=49.991, lon=8.256, level=0, nuts.path=my.nuts.path)
#' # map_nuts_name(lat=49.991, lon=8.256, level=1, german.iso.codes=FALSE, nuts.path=my.nuts.path)
#' # map_nuts_name(lat=49.991, lon=8.256, level=1, german.iso.codes=TRUE, nuts.path=my.nuts.path)
#' # map_nuts_name(lat=50.96, lon=10.78, level=1, german.iso.codes=TRUE, nuts.path=my.nuts.path)
#' # map_nuts_name(lat=48.42, lon=8.96, level=1, german.iso.codes=TRUE, nuts.path=my.nuts.path)
#' @export
map_nuts_name <- function(lat, lon, level=0, german.iso.codes=FALSE, nuts.path){
  my.layer <- sf::st_layers(nuts.path)$name[1]
  NUTS <- sf::st_read(nuts.path, layer = my.layer, quiet = TRUE)
  NUTS <- NUTS %>% dplyr::filter(.data$LEVL_CODE==level)
  POI <- data.frame(lat=lat, lon=lon)
  POI <- sf::st_as_sf(POI, coords = c('lon', 'lat'), crs = 4326)
  POI <- sf::st_transform(POI, crs = sf::st_crs(NUTS))
  POI <- POI %>%
    dplyr::mutate(intersection = as.integer(sf::st_intersects(.data$geometry, NUTS))) %>%
    dplyr::mutate(region = dplyr::if_else(is.na(.data$intersection), '', NUTS$NAME_LATN[.data$intersection]))
  POI <- as.data.frame(POI) %>%
    tibble::tibble()
  if(german.iso.codes){
    POI <- POI %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Baden-W\u00fcrttemberg", "BW", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Bayern", "BY", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Berlin", "BE", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Brandenburg", "BB", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Bremen", "HB", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Hamburg", "HH", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Hessen", "HE", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Mecklenburg-Vorpommern", "MV", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Niedersachsen", "NI", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Nordrhein-Westfalen", "NW", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Rheinland-Pfalz", "RP", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Saarland", "SL", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Sachsen", "SN", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Sachsen-Anhalt", "ST", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Schleswig-Holstein", "SH", .data$region)) %>%
      dplyr::mutate(region=dplyr::if_else(.data$region=="Th\u00fcringen", "TH", .data$region))
  }
  return(POI$region)
}


#' gets Eurostat's GISCO grid IDs for given geographic points.
#' Source surf.gpkg files of various resolution available at
#'  https://gisco-services.ec.europa.eu/grid/
#' @param lat (numeric) vector of latitudes of the geographic point
#' @param lon (numeric) vector of longitudes of the geographic point
#' @param grid_id_var (chr) variable name to use in the NUTS file (default='GRD_ID')
#' @param grid.gpkg.path (chr) local gpkg file path (e.g. grid_5km_surf.gpkg)
#' @examples
#' # # Download 100km grid file in temporary folder
#' # my.grid.url="https://gisco-services.ec.europa.eu/grid/grid_100km_surf.gpkg"
#' # download_and_unzip(my.grid.url, out.dir="tmp/grids", overwrite=TRUE, unzip=FALSE)
#' # # Use local grid file
#' # map_gisco_grid_id(lat=49.991, lon=10, grid.gpkg.path="tmp/grids/grid_100km_surf.gpkg")
#' @export
map_gisco_grid_id <- function(lat, lon, grid_id_var="GRD_ID", grid.gpkg.path){
  suppressWarnings(my.layer <- sf::st_layers(grid.gpkg.path)$name[1])
  suppressWarnings(Grid <- sf::st_read(grid.gpkg.path, layer = my.layer, quiet = TRUE))
  POI <- data.frame(lat=lat, lon=lon)
  POI <- sf::st_as_sf(POI, coords = c('lon', 'lat'), crs = 4326)
  POI <- sf::st_transform(POI, crs = sf::st_crs(Grid))
  POI <- POI %>%
    dplyr::mutate(intersection = as.integer(unlist(lapply(sf::st_intersects(.data$geometry, Grid), function(x) x[[1]])))) %>%
    dplyr::mutate(grid_id = dplyr::if_else(is.na(.data$intersection), '', Grid[[grid_id_var]][.data$intersection]))
  POI <- as.data.frame(POI) %>% tibble::tibble()
  return(POI$grid_id)
}
