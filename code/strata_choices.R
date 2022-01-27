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
library(rgeos)
library(xlsx)

###############################################################################
####   Plotting function
###############################################################################  
source("modified_functions/add_graticule_labels.R")

###############################################################################
##   Import 420 station allocation
###############################################################################  
AI_stations <- read.csv(file = "data/AIallocation420.csv")
resample <- function(x, ...) x[sample.int(length(x), ...)]

###############################################################################
##   Import AI strata and land shapefiles
##   Subset AI_strata_poly to just those strata included in AI_stations
############################################################################### 
AI_strata_poly <- rgdal::readOGR(dsn = "G:\\AI-GOA\\shapefiles\\ai_strata.shp")
strata_ids <- sort(unique(x = AI_stations$STRATUM))
AI_strata_poly <- subset(x = AI_strata_poly, subset = STRATUM %in% strata_ids)

AK_land <- rgdal::readOGR(dsn = "G:\\AI-GOA\\shapefiles\\alaska_dcw.shp")

###############################################################################
####   Change vessel ids for 2022
###############################################################################  
vessel_ids <- c(176, "V2")
AI_stations$VESSEL <- ifelse(test = AI_stations$VESSEL == 143, 
                             yes = vessel_ids[1], 
                             no = vessel_ids[2])

###############################################################################
##   Import boundaries of the INPFC mangagement areas
###############################################################################  
AI_regions <- rgdal::readOGR(dsn = "G:\\AI-GOA\\shapefiles\\AI_Regions_Poly.shp")
AI_regions <- sp::spTransform(x = AI_regions, AK_land@proj4string)
AI_regions@data$Region2 <- c("Western", "Western",
                             "Central", "Central", "Central", "Central",
                             "Eastern", "Eastern",
                             "SBS", "SBS", "SBS", "SBS")

##################################################
#### Create Graticules
##################################################
lon_labs <- seq(from = 165, to = 210, by = 2)
lat_labs <- seq(from = 45, to = 60, by = 1)

aea_grat_lon <- graticule::graticule(lons = lon_labs,
                                     lats = range(lat_labs),
                                     proj = AK_land@proj4string)
aea_grat_lon <- aea_grat_lon[-c(24:25), ]

aea_grat_lat <- graticule::graticule(lons = range(lon_labs),
                                     lats = lat_labs,
                                     proj = AK_land@proj4string)
aea_grat_lat <- aea_grat_lat[-c(1:2), ]

###############################################################################
####   Calculate stratum area and perimeter for each AI stratum
####   Calculate the Diversity Index for the the stratum edge
###############################################################################  
strata_area <- tapply(X = AI_strata_poly@data$AREA, 
                      INDEX = AI_strata_poly@data$STRATUM, 
                      FUN = sum) 
strata_edge <- tapply(X = AI_strata_poly@data$PERIMETER, 
                      INDEX = AI_strata_poly@data$STRATUM, 
                      FUN = sum)

strata_edge_to_area <- strata_edge / 2 / sqrt(strata_area * pi)

###############################################################################
##   Assign region and stratum type (large or thin) for each stratum
##   
##   Strata are characterized by region based on the first digit of the 
##          stratum id. 
##
##   Large stratum: Edge:Area ratio < mean(Edge:Area) ratio across strata AND
##                  Total Area > 1000 km^2
##
##   Thin stratum: otherwise
##
##   Combine strata characteristics into a df (strata_char)
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

strata_type <- ifelse(test = strata_edge_to_area < mean(strata_edge_to_area) & 
                        (strata_area / 1e6) > 1000,
                      yes = "large", 
                      no = "thin")

strata_char <- data.frame(stratum = strata_ids,
                          region = region,
                          type = strata_type, 
                          stringsAsFactors = F)

###############################################################################
##   Randomly select strata as strata to find new stations
##   Each each region, one large and one thin strata are randomly chosen
###############################################################################  
set.seed(234234)
strata_char$new_station <- FALSE
for (iregion in unique(strata_char$region)) { ## Loop over region -- start
  for (itype in unique(strata_char$type)) { ## Loop over stratum type -- start
    chosen_strata <- with(subset(x = strata_char, 
                                 subset = region == iregion & type == itype),
                          sample(x = stratum, size = 1)) 
    strata_char$new_station[strata_char$stratum == chosen_strata] <- TRUE
  } ## Loop over stratum type -- start
} ## Loop over region -- end

###############################################################################
##   Each vessel will find a new station in a large stratum.
##   The four thin strata will be split equally between the vessels and each
###############################################################################  
set.seed(234235)
strata_new_stations <- subset(x = strata_char, new_station == TRUE)
strata_new_stations[, paste0("vessel_", vessel_ids)] <- 0
strata_new_stations[strata_new_stations$type == "large",
                    paste0("vessel_", vessel_ids)] <- 1
strata_new_stations[strata_new_stations$region %in% c("Western", "Eastern") &
                      strata_new_stations$type == "thin",
                    paste0("vessel_", vessel_ids[1])] <- 1
strata_new_stations[strata_new_stations$region %in% c("Central", "SBS") &
                      strata_new_stations$type == "thin",
                    paste0("vessel_", vessel_ids[2])] <- 1

###############################################################################
####   Randomly remove stations in the chosen strata 
###############################################################################  
set.seed(25323)

# AI_stations$TRUE_STATION <- T
for (irow in 1:nrow(strata_new_stations)) {
  istratum <- strata_new_stations$stratum[irow]
  
  if (strata_new_stations$type[irow] == "large") {
    idx <- c(resample(x = which(AI_stations$STRATUM == istratum & 
                                  AI_stations$VESSEL == vessel_ids[1]), 
                      size = 1 ),
             resample(x = which(AI_stations$STRATUM == istratum & 
                                  AI_stations$VESSEL == vessel_ids[2]), 
                      size = 1 ))
  }
  
  if (strata_new_stations$type[irow] == "thin") { 
    vessel_idx <- 
      which(strata_new_stations[irow, paste0("vessel_", vessel_ids)] == 1)
    idx <- resample(x = which(AI_stations$STRATUM == istratum & 
                                AI_stations$VESSEL == vessel_ids[vessel_idx]), 
                    size = 1)
  }
  
  AI_stations[idx, c("STATIONID", "LONGITUDE", "LATITUDE")] <- NA
  
}

###############################################################################
####   Create outputs to be used downstream
###############################################################################  
xlsx::write.xlsx(x = AI_stations, 
                 file = paste0("output_products/AIallocation420.xlsx"), 
                 row.names = FALSE,
                 sheetName = "stations",
                 showNA = FALSE)

xlsx::write.xlsx(x = subset(x = strata_new_stations, 
                            select = c("region", "stratum", "type", 
                                       paste0("vessel_", vessel_ids))), 
                 file = paste0("output_products/AIallocation420.xlsx"), 
                 append = TRUE,
                 sheetName = "strata with new stations",
                 row.names = FALSE)

###############################################################################
##   Plot "Large" vs "thin" strata
###############################################################################  
{
  for (iregion in unique(strata_char$region)) { ## Loop over region -- start
    
    pdf(paste0("output_plots/large_vs_thin_strata_", iregion, ".pdf"), 
        width = 4, height = 6, family = "serif", onefile = TRUE, version = "1.7")
    
    ## Subset strata within iregion 
    plot_these_strata <- strata_char$stratum[strata_char$region == iregion]
    AI_strata_region <- subset(AI_strata_poly, STRATUM %in% plot_these_strata)
    
    ## Subset large and thin strata within iregion
    new_large_strata <- strata_char$stratum[strata_char$region == iregion & 
                                              strata_char$type == "large"]
    new_thin_strata <- strata_char$stratum[strata_char$region == iregion & 
                                             strata_char$type == "thin"]
    
    for (itype in c("large", "thin")) { ## Loop over stratum type -- start 
      
      ## Set up plot panels
      par(mfrow = c(4, 3), mar = c(0,0,0,0), oma = c(2, 3, 2, 2))
      new_strata <- get(paste0("new_", itype, "_strata"))
      
      iplot = 1 # for graticule plotting purposes
      
      for (istratum in new_strata) { ## Loop over strata -- start
        
        ## Base Plot
        xlim_ <- switch(iregion,
                        "Western" = extent(AI_strata_region)[1:2],
                        "Central" = c(extent(AI_strata_region)[1], 
                                      extent(AI_regions[6,])[2]),
                        "Eastern" = extent(AI_strata_region)[1:2],
                        "SBS" = c(extent(AI_regions[8,])[1],
                                  extent(AI_strata_region)[2]) )
        
        plot(AI_strata_region, 
             col = "white", border = F,
             xlim = xlim_)
        
        ## Plot graticules
        plot(aea_grat_lat, add = TRUE, col = "lightgray", lwd = 0.1)
        plot(aea_grat_lon, add = TRUE, col = "lightgray", lwd = 0.1)
        
        ## Plot all strata within region
        plot(AI_strata_region, 
             col = "grey", border = "darkgrey",
             xlim = xlim_, add = TRUE, lwd = 0.25)
        
        ## Highlight istratum  
        AI_strata_cropped <- subset(AI_strata_poly, STRATUM %in% istratum)
        plot(AI_strata_cropped,
             col = switch(itype, "large" = "red", 
                          "thin" = "blue"), border = "darkgrey",
             lwd = 0.25, 
             xlim = xlim_, add = TRUE)
        
        ## Add land
        plot(AK_land, add = TRUE, col = "tan",  border = F)
        
        ## Add graticules
        box()
        if (iplot %in% (ceiling(length(new_strata) / 3) * 3):(ceiling(length(new_strata) / 3) * 3 -2)  ) 
          add_graticule_labels(side = "bottom", cex = 0.75)
        if (iplot %% 3 == 1) 
          add_graticule_labels(side = "left", cex = 0.75)
        if (iplot %in% 1:3 ) 
          add_graticule_labels(side = "top", cex = 0.75)
        
          iplot <- iplot + 1
        
        ## Subset stations allocated to istratum and plot
        allocation <- subset(x = AI_stations, 
                             subset = STRATUM == istratum)
        allocation <- na.omit(allocation)
        
        if (nrow(allocation) > 0) {
          allocation_pts <- 
            sp::SpatialPoints(coords = allocation[, c("LONGITUDE", "LATITUDE")], 
                              proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
          allocation_pts <- sp::spTransform(x = allocation_pts, 
                                            CRSobj = AK_land@proj4string)
          
          points(allocation_pts, 
                 pch = ifelse(test = allocation$VESSEL == vessel_ids[1], 
                              16, 
                              3), 
                 cex = 0.3)
          
          legend_lab <- c() 
          for (vessel_idx in vessel_ids) {
            legend_lab <- c(legend_lab, 
                            paste0("Vessel ", vessel_idx, ": ", 
                                   nrow(subset(allocation, 
                                               VESSEL == vessel_idx)), 
                                   " stations"))
          }
          
          legend("bottomleft", 
                 legend = legend_lab,
                 pch = c(16, 3),
                 bty = "n",
                 cex = 0.75)
          
        }
        
        ## Add stratum label
        legend("topright", 
               legend = paste0( "Stratum ", istratum, "\n",
                               "'", itype, "' stratum"),
               bty = "n")
        
        
      }  ## Loop over strata -- end
    }  ## Loop over stratum type -- end
    dev.off()
  }  ## Loop over region -- end
}

###############################################################################
##   Plot chosen large and thin strata
###############################################################################  

{
  pdf("output_plots/chosen_strata.pdf", onefile = TRUE, version = "1.7",
      width = 6, height = 6, family = "serif", compress = TRUE)
  
  ## Set up panel
  par(mar = c(2, 3, 0.5, 0.5), mfrow = c(1, 1))
  
  for (iregion in unique(strata_char$region)) { ## Loop over region -- start
    
    ## Subset strata within iregion 
    plot_these_strata <- strata_char$stratum[strata_char$region == iregion]
    
    ## Subset large and thin strata within iregion
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
         col = "white", border = F,
         xlim = xlim_)
    
    ## Add graticules
    plot(aea_grat_lat, add = TRUE, col = "lightgray")
    plot(aea_grat_lon, add = TRUE, col = "lightgray")
    
    ## Plot strata in region
    plot(AI_strata_cropped, 
         col = "grey", border = "darkgrey",
         xlim = xlim_, add = TRUE)
    
    ## Plot chosen strata
    plot(subset(AI_strata_poly, STRATUM %in% new_large_strata), 
         col = "red", border = "darkgrey", add = TRUE)
    plot(subset(AI_strata_poly, STRATUM %in% new_thin_strata), 
         col = "blue", border = "darkgrey", add = TRUE)
    
    ## Plot land
    plot(AK_land, add = TRUE, col = "tan",  border = F)
    
    ## Plot station locations
    allocation <- subset(x = AI_stations, 
                         subset = STRATUM %in% plot_these_strata)
    allocation <- na.omit(allocation)
    
    allocation_pts <- 
      sp::SpatialPoints(coords = allocation[, c("LONGITUDE", 
                                                "LATITUDE")], 
                        proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
    allocation_pts <- sp::spTransform(x = allocation_pts, 
                                      CRSobj = AK_land@proj4string)
    
    points(allocation_pts, pch = 16, cex = 0.75)
    
    ## Plot graticule labels
    box()
    add_graticule_labels(side = c("bottom", "left"))
    
    ## Add legend
    legend("bottomleft", 
           fill = c("red", "blue"),
           legend = c(paste0("Large Stratum: ", new_large_strata),
                      paste0("Thin Stratum: ", new_thin_strata)),
           title = iregion,
           bty = "n")
  }
  
  dev.off()
}
