#' Converts a shapefile into an adjacency matrix.
#' 
#' \code{make_adjacency_mat} converts a shapefile into an adjacency matrix.
#' 
#' @param shapefile A SpatialPolygonsDataFrame object OR a character giving the filepath to the shapefile.
#' @param names An optional character giving the name of the variable in the shapefile that should be used as row/column
#' names in the output adjacency matrix. By default, \code{names = NULL}.
#' @param self_adj An optional logical indicating if you can be adjacent to yourself. By default, \code{self_adj = FALSE}.
#' @param save_as An optional character giving the filepath to save the adjacency matrix as a .rds file. 
#' Must end in .rds. By default, \code{save_as = NULL}, in which case no .rds file will be created.
#' 
#' @return A binary matrix in which entry (i, j) = 1 if locations i and j are adjacent in the given shapefile, 0 if not.
#' 
#' @examples 
#' nyc.shp <- readOGR(dsn = here::here("data", "shapefilesNYC2010"))
#' A <- make_adjacency_mat(nyc.shp, names = "BoroCT2010")
#' 
#' @export
make_adjacency_mat <- function(
  shapefile, 
  names = NULL, 
  self_adj = FALSE, 
  save_as = NULL
) {
  
  # 0a. error handling: can take a SpatialPolygonsDataFrame object or 
  # a character giving the filepath to the shapefile to be read in.
  if (class(shapefile) == "character") {
    shp <- rgdal::readOGR(dsn = shapefile)
  } else {
    shp <- shapefile
  }
  
  # 0b. more error handling for the names variable
  if (!is.null(names)) {
    row_names <- shp[[names]]
  } else {
    row_names <- NULL
  }
  
  # 1a. convert shapefile to nearest neighbours list (nnl)
  shp.nnl <- spdep::poly2nb(shp, queen = F, row.names = row_names)
  
  # 1b. convert nnl to adjacency matrix
  adj_mat <- spdep::nb2mat(shp.nnl, style = "B", zero.policy = T)
  colnames(adj_mat) <- rownames(adj_mat)
  
  # 1c. if being a neighbour to yourself is meaningful, record that:
  if (self_adj) diag(adj_mat) <- rep(1, nrow(adj_mat))
  
  # 2a. save
  if (!is.null(save_as)) saveRDS(adj_mat, file = save_as)
  
  # 2b. return
  adj_mat
  
}
