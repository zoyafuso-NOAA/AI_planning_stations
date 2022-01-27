add_graticule_labels <- function(side = c("bottom", "top", "left", "right"), 
                                 lon_labs_ = lon_labs, 
                                 lat_labs_ = lat_labs) {
  
  if ("bottom" %in% side) {
    bottom_line <- raster::extent(par()$usr[c(1, 2, 3, 3)] )
    bottom_line <- as(bottom_line, 'SpatialLines')
    crs(bottom_line) <- AK_land@proj4string
    
    axis_labs <- ( rgeos::gIntersects(aea_grat_lon, bottom_line, byid = T) )
    axis_locs <- ( rgeos::gIntersection(aea_grat_lon, bottom_line) )
    
    text(axis_locs@coords, 
         labels =  bquote(expr = .(paste0(lon_labs_[axis_labs], 
                                          intToUtf8(176), "E") )),
         xpd = NA, pos = 1)
  }
  
  if ("top" %in%  side) {
    top_line <- raster::extent(par()$usr[c(1, 2, 4, 4)] )
    top_line <- as(top_line, 'SpatialLines')
    crs(top_line) <- AK_land@proj4string
    
    axis_labs <- ( rgeos::gIntersects(aea_grat_lon, top_line, byid = T) )
    axis_locs <- ( rgeos::gIntersection(aea_grat_lon, top_line) )
    
    text(axis_locs@coords, 
         labels =  bquote(expr = .(paste0(lon_labs_[axis_labs], 
                                          intToUtf8(176), "E") )),
         xpd = NA, pos = 1)
  }
  
  if ("left" %in% side) {
    left_line <- raster::extent(par()$usr[c(1, 1, 3, 4)] )
    left_line <- as(left_line, 'SpatialLines')
    crs(left_line) <- AK_land@proj4string
    
    axis_labs <- ( rgeos::gIntersects(aea_grat_lat, left_line, byid = T) )
    axis_locs <- sp::remove.duplicates( rgeos::gIntersection(aea_grat_lat, left_line) )
    
    text(axis_locs@coords, 
         labels =  bquote(expr = .(paste0(lat_labs_[axis_labs], intToUtf8(176), "N") )),
         xpd = NA, pos = 2)
  }
  
  if ("right" %in% side) {
    right_line <- raster::extent(par()$usr[c(2, 2, 3, 4)] )
    right_line <- as(right_line, 'SpatialLines')
    crs(right_line) <- AK_land@proj4string
    
    axis_labs <- ( rgeos::gIntersects(aea_grat_lat, right_line, byid = T) )
    axis_locs <- sp::remove.duplicates( rgeos::gIntersection(aea_grat_lat, right_line) )
    
    text(axis_locs@coords, 
         labels =  bquote(expr = .(paste0(lat_labs_[axis_labs], 
                                          intToUtf8(176), "N") )),
         xpd = NA, pos = 2)
  }
}



