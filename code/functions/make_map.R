make_map <- function(
  vars, 
  data.shp, 
  tm_mode = "plot", 
  dir = NULL, 
  filename = NULL,
  tm_w = NA,
  tm_h = NA,
  tm_dpi = 100,
  cols = NA,
  rows = NA
) {
  
  max_break <- max(data.shp@data[vars], na.rm = T)
  
  map <- tm_shape(
    data.shp 
  ) + 
    tm_fill(
      vars, 
      popup.vars = c("Area" = "NTAName", vars),
      style = "cont",
      breaks = seq(0, max_break, length.out = 10)
    ) +
    tm_layout(
      frame=FALSE, 
      bg.color="gray60"
    ) +
    tm_facets(ncol = cols, nrow = rows) # by = "BoroName"
  
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
