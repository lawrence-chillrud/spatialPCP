make_adjacency_mat <- function(shapefile, names = NULL) {
	# error handling: can take a SpatialPolygonsDataFrame object or a character giving
  # the filepath to the shapefile to be read in.
  if (class(shapefile) == "character") {
    shp <- rgdal::readOGR(dsn = shapefile)
  } else {
    shp <- shapefile
  }
  
  # more error handling for the names variable
  if (!is.null(names)) {
    row_names <- shp[[names]]
  } else {
    row_names = NULL
  }
  shp.nnl <- spdep::poly2nb(shp, queen = F, row.names = row_names)
  adj_mat <- spdep::nb2mat(shp.nnl, style = "B", zero.policy = T)
  colnames(adj_mat) <- rownames(adj_mat)
  #diag(adj_mat) <- rep(1,nrow(adj_mat)) # only useful if being a neighbour to yourself is meaningful in our context
  
  adj_mat
  
}
