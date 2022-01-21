###############################################################################
## Project:       Strata choices to find new stations
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   1) Divide strata into "large" or "thin" strata
##                2) Randomly pick one large and one thin strata from each
##                   management area
##                3) Create output plots
###############################################################################
rm(list = ls())

###############################################################################
##   Import libraries
###############################################################################  
library(rgdal)
library(sp)
library(raster)
library(graticule)

###############################################################################
##   Import 420 station allocation
###############################################################################  
AI_stations <- read.csv("data/AIallocation420.csv")
strata_ids <- sort(unique(AI_stations$STRATUM))


AI_strata_poly <- rgdal::readOGR("G:\\AI-GOA\\shapefiles\\ai_strata.shp")

AK_land <- rgdal::readOGR("G:\\AI-GOA\\shapefiles\\alaska_dcw.shp")

landmarks <- subset(x = read.csv("G:\\AI-GOA\\shapefiles\\AKPlaces.csv"),
                    subset = survey == "AI")
landmarks_shp <- 
  sp::SpatialPointsDataFrame(coords = landmarks[, c("LONGITUDE", "LATITUDE")],
                             data = landmarks, 
                             proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
landmarks_shp <- sp::spTransform(x = landmarks_shp, CRSobj = AK_land@proj4string)

AI_dissolved <- rgdal::readOGR("G:\\AI-GOA\\shapefiles\\AIdissolved.shp")

###############################################################################
##   Crop Land shapefile
###############################################################################  
AK_land <- raster::crop(x = AK_land, y = AI_dissolved)

###############################################################################
##   
###############################################################################  
AI_regions <- rgdal::readOGR("G:\\AI-GOA\\shapefiles\\AI_Regions_Poly.shp")
AI_regions <- sp::spTransform(x = AI_regions, AK_land@proj4string)
AI_regions@data$Region2 <- c("Western", "Western",
                             "Central", "Central", "Central", "Central",
                             "Eastern", "Eastern",
                             "SBS", "SBS", "SBS", "SBS")

##################################################
#### Graticules
##################################################
aea_grat <- graticule::graticule(lons = seq(from = 200, to = 150, by = -1),
                                 lats = seq(from = 50, to = 56, by = 1), 
                                 proj = AK_land@proj4string)
# aea_grat_labs <- graticule::graticule_labels(lons = seq(from = -175, 
#                                                         to = -160, by = 5),
#                                              lats = seq(from = 70, 
#                                                         to = 72.5, by = 2.5),
#                                              xline = -171, yline = 72.5,
#                                              proj = aea_crs)

##################################################

###############################################################################
##   Calculate strata area in km^2
###############################################################################  
strata_area <- tapply(X = AI_strata_poly@data$AREA, 
                      INDEX = AI_strata_poly@data$STRATUM, 
                      FUN = sum) 
strata_edge <- tapply(X = AI_strata_poly@data$PERIMETER, 
                      INDEX = AI_strata_poly@data$STRATUM, 
                      FUN = sum)
strata_edge_to_area <- strata_edge / 2 / sqrt(strata_area * pi)
# strata_edge_to_area <- strata_edge / strata_area
strata_edge_to_area <- strata_edge_to_area[names(strata_edge_to_area) %in% paste0(strata_ids)] 

strata_area <- strata_area[names(strata_area) %in% paste0(strata_ids)] 

###############################################################################
##   Assign region and stratum type (large or thin) for each stratum
###############################################################################  

region <- sapply(X = substr(x = strata_ids, 
                            start = 1, 
                            stop = 1), 
                 FUN = function(x) 
                   switch(x, 
                          "2" = "Western",
                          "3" = "Central", "4" = "Central",
                          "5" = "Eastern", "6" = "Eastern",
                          "7" = "SBS"))

# strata_type <- ifelse(test = substr(x = strata_ids,
#                                     start = 3,
#                                     stop = 3) %in% paste(1:2),
#                       yes = "large", no = "thin")

strata_type <- ifelse(test = strata_edge_to_area < mean(strata_edge_to_area) & (strata_area / 1e6) > 1000,
                      yes = "large", no = "thin")

strata_char <- data.frame(stratum = strata_ids,
                          region = region,
                          type = strata_type, 
                          stringsAsFactors = F)

###############################################################################
##   Randomly select strata as strata to find new stations
##   Each each region, one large and one thin strata are randomly chosen
###############################################################################  
set.seed(234234)
chosen_strata <- c()
for (iregion in unique(strata_char$region)){
  for (itype in unique(strata_char$type)) {
    chosen_strata <- 
      c(chosen_strata,
        with(subset(x = strata_char, 
                    subset = region == iregion & type == itype),
             sample(x = stratum, size = 1)) ) 
  }
}

strata_char$new_station <- ifelse(test = strata_char$stratum %in% chosen_strata, 
                                  yes = "TRUE", no = "FALSE")

###############################################################################
##   Plot "Large" vs "thin" strata
###############################################################################  
par(mar = c(0,0,0,0))

{
  pdf("output_plots/large_vs_thin_strata.pdf", onefile = TRUE, 
      width = 6, height = 8, family = "serif")
  for (iregion in unique(strata_char$region)){
    
    plot_these_strata <- strata_char$stratum[strata_char$region == iregion]
    AI_strata_region <- subset(AI_strata_poly, STRATUM %in% plot_these_strata)
    
    new_large_strata <- strata_char$stratum[strata_char$region == iregion & 
                                              strata_char$type == "large"]
    new_thin_strata <- strata_char$stratum[strata_char$region == iregion & 
                                             strata_char$type == "thin"]
    
    
    for(itype in c("large", "thin")) {
      
      par(mfrow = c(4, 3), mar = c(0,0,0,0))
      new_strata <- get(paste0("new_", itype, "_strata"))
      
      for (istratum in new_strata){
        ## Base Plot
        AI_strata_cropped <- subset(AI_strata_poly, STRATUM %in% istratum)
        xlim_ <- switch(iregion,
                        "Western" = extent(AI_strata_region)[1:2],
                        "Central" = c(extent(AI_strata_region)[1], 
                                      extent(AI_regions[6,])[2]),
                        "Eastern" = extent(AI_strata_region)[1:2],
                        "SBS" = c(extent(AI_regions[8,])[1],
                                  extent(AI_strata_region)[2]) )
        
        plot(AI_strata_region, 
             col = "grey", border = "darkgrey",
             xlim = xlim_)
        plot(aea_grat, add = TRUE, col = "lightgray")
        plot(AI_strata_region, 
             col = "grey", border = "darkgrey",
             xlim = xlim_, add = TRUE)
        plot(AI_strata_cropped,
             col = switch(itype, "large" = "red", 
                          "thin" = "blue"), border = "darkgrey",
             xlim = xlim_, add = TRUE)
        plot(AK_land, add = TRUE, col = "black",  border = F)
        plot(AI_regions, add = TRUE)
        box()
        
        legend("topright", 
               legend = paste0(iregion, "\nstratum ", istratum, 
                               "\n'", itype, "' stratum"),
               bty = "n")
        
      }
      
    }
  }
  dev.off()
}

###############################################################################
##   Plot
###############################################################################  
par(mar = c(0,0,0,0))
for (iregion in unique(strata_char$region)){
  
  plot_these_strata <- strata_char$stratum[strata_char$region == iregion]
  new_large_strata <- strata_char$stratum[strata_char$region == iregion & 
                                            strata_char$type == "large" &
                                            strata_char$new_station == TRUE]
  new_thin_strata <- strata_char$stratum[strata_char$region == iregion & 
                                           strata_char$type == "thin" &
                                           strata_char$new_station == TRUE]
  
  ## Base Plot
  AI_strata_cropped <- subset(AI_strata_poly, STRATUM %in% plot_these_strata)
  xlim_ <- switch(iregion,
                  "Western" = extent(AI_strata_cropped)[1:2],
                  "Central" = c(extent(AI_strata_cropped)[1], 
                                extent(AI_regions[6,])[2]),
                  "Eastern" = extent(AI_strata_cropped)[1:2],
                  "SBS" = c(extent(AI_regions[8,])[1],
                            extent(AI_strata_cropped)[2]) )
  
  plot(AI_strata_cropped, 
       col = "grey", border = "darkgrey",
       xlim = xlim_)
  plot(aea_grat, add = TRUE, col = "lightgray")
  plot(AI_strata_cropped, 
       col = "grey", border = "darkgrey",
       xlim = xlim_, add = TRUE)
  plot(AK_land, add = TRUE, col = "black",  border = F)
  plot(AI_regions, add = TRUE)
  
  plot(subset(AI_strata_poly, STRATUM %in% new_large_strata), 
       col = "red", border = "darkgrey", add = TRUE)
  plot(subset(AI_strata_poly, STRATUM %in% new_thin_strata), 
       col = "blue", border = "darkgrey", add = TRUE)
}

plot(AI_dissolved)