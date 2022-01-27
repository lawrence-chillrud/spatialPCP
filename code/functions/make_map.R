#' Plots maps from shapefile data.
#'
#' \code{make_map} plots maps from shapefile data using the tmap package.
#' 
#' @param data.shp A SpatialPolygonsDataFrame object with the spatial data to be plotted as a map / maps. 
#' @param vars A character or character vector detailing which variable(s) in data.shp to map. If a character vector is given, 
#' multiple maps will be plotted to the same figure.
#' @param cols An optional numeric indicating # of columns to arrange the maps into in the output plot By default, \code{cols = NA}.
#' @param rows An optional numeric indicating # of rows to arrange the maps into in the output plot By default, \code{rows = NA}.
#' @param tm_mode An optional character (either "plot" or "view") identifying what mode the output plot should be in. By default, \code{tm_mode = "plot"}.
#' @param dir An optional character giving the directory the plot should be saved in. By default, \code{dir = NULL}, in which case the plot is not saved.
#' @param filename An optional character giving the name of the plot to be saved. By default, \code{filename = NULL}, in which case the plot is 
#' named after the contents of \code{vars} (only used if \code{!is.null(dir)}).
#' @param tm_w An optional numeric giving the width of the plot in inches. By default, \code{tm_w = NA}.
#' @param tm_h An optional numeric giving the height of the plot in inches. By default, \code{tm_h = NA}.
#' @param tm_dpi An optional numeric giving the dots per inch of the plot. The larger the dpi, the better the resolution. By default, \code{tm_dpi = 100}.
#' 
#' @return If the plot was saved to a .png file, then a character is returned with the filepath. Else, the plot itself is returned. 
#' 
#' @export
#' @import tmap
make_map <- function(
  data.shp,
  vars, 
  cols = NA,
  rows = NA,
  tm_mode = "plot", 
  dir = NULL, 
  filename = NULL,
  tm_w = NA,
  tm_h = NA,
  tm_dpi = 100
) {
  
  # 1. plot map(s):
  max_break <- max(data.shp@data[vars], na.rm = T)
  
  map <- tm_shape(
    data.shp 
  ) + 
    tm_fill(
      vars, 
      popup.vars = c("Area" = "NTAName", vars),
      style = "cont",
      #breaks = seq(0, max_break, length.out = 10)
    ) +
    tm_layout(
      frame=FALSE, 
      bg.color="gray60"
    ) +
    tm_facets(ncol = cols, nrow = rows) # by = "BoroName"
  
  # 2. save / return plot:
  if (!is.null(dir)) {
    
    dir.create(here::here(dir), showWarnings = FALSE)
    
    tmap_mode("plot")
    
    if (!is.null(filename)) {
      
      if (endsWith(filename, ".png")) {
        fn <- here::here(dir, filename)
      } else {
        fn <- here::here(dir, paste0(filename, ".png"))
      }
      
    } else {
      
      fn <- here::here(dir, paste0(paste0(vars, collapse = "-"), ".png"))
      
    }
    
    tmap_save(map, asp = 0, width=tm_w, height=tm_h, dpi = tm_dpi, filename = fn)
    
    msg <- paste0("\nSaved the following maps to ", here::here(dir, fn), "\n")
    cat(msg)
    cat(paste0(vars, collapse = ", "))
    return(msg)
    
  } else {
    
    tmap_mode(tm_mode)
    return(map)
    
  }
  
}
