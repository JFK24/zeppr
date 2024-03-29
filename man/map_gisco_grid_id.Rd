% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_annotations.R
\name{map_gisco_grid_id}
\alias{map_gisco_grid_id}
\title{gets Eurostat's GISCO grid IDs for given geographic points.
Source surf.gpkg files of various resolution available at
https://gisco-services.ec.europa.eu/grid/}
\usage{
map_gisco_grid_id(lat, lon, grid_id_var = "GRD_ID", grid.gpkg.path)
}
\arguments{
\item{lat}{(numeric) vector of latitudes of the geographic point}

\item{lon}{(numeric) vector of longitudes of the geographic point}

\item{grid_id_var}{(chr) variable name to use in the NUTS file (default='GRD_ID')}

\item{grid.gpkg.path}{(chr) local gpkg file path (e.g. grid_5km_surf.gpkg)}
}
\description{
gets Eurostat's GISCO grid IDs for given geographic points.
Source surf.gpkg files of various resolution available at
https://gisco-services.ec.europa.eu/grid/
}
\examples{
# # Download 100km grid file in temporary folder
# my.grid.url="https://gisco-services.ec.europa.eu/grid/grid_100km_surf.gpkg"
# download_and_unzip(my.grid.url, out.dir="tmp/grids", overwrite=TRUE, unzip=FALSE)
# # Use local grid file
# map_gisco_grid_id(lat=49.991, lon=10, grid.gpkg.path="tmp/grids/grid_100km_surf.gpkg")
}
