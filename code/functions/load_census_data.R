#' Loads the NYC 2010 census data and joins it with the appropriate shapefile
#' 
#' \code{load_census_data} loads the NYC 2010 census data and joins it with the appropriate shapefile.
#' 
#' @return A SpatialPolygonsDataFrame with the census data joined to it.
#' 
#' @examples 
#' nyc.shp <- load_census_data()
#' 
#' @export
load_census_data <- function() {
  
  nyc.shp <- rgdal::readOGR(dsn = here::here("data", "raw", "shapefilesNYC2010"))
  nyc.census <- readr::read_csv(here::here("data", "clean", "nyc_census_2010.csv"), col_types = paste0(paste(rep("c", 6), collapse = ""), paste(rep("d", 108), collapse = "")))
  nyc.shp@data <- dplyr::left_join(x = nyc.shp@data, y = nyc.census)
  return(nyc.shp)
  
}
