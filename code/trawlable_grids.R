###############################################################################
## Project:       Trawlability across Aleutian Island Grid
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:
###############################################################################

###############################################################################
####   Import Packages
###############################################################################  
library(rgdal)
library(raster)

###############################################################################
####   Run once to pull trawlabiilty data from AIGRID_GIS table
###############################################################################  
# library(RODBC)
# channel <- RODBC::odbcConnect(dsn = "AFSC", uid = "AIGOA_WORK_DATA",
#                               pwd = "maculata_480", believeNRows = FALSE)
# 
# ai_trawl <- sqlQuery(channel = channel, query = "select * from AI.AIGRID_GIS")
# write.csv(x = ai_trawl, file = "data/ai_grid_trawlability.csv", row.names = F)

###############################################################################
####   Import AI grid shapefile and trawlability information
###############################################################################  
ai_trawl <- read.csv(file = "data/ai_grid_trawlability.csv")
ai_grid <- rgdal::readOGR(dsn = "data/GDrive_shapefiles/aigrid_clipped.shp")

###############################################################################
####   Match trawlability information in ai_trawl to the ai_grid by matching the 
####         AIGRID ID
###############################################################################  
ai_grid@data$TRAWLABLE <- ai_trawl$TRAWLABLE[match(ai_grid@data$AIGRID_ID, 
                                                   ai_trawl$AIGRID_ID) ]

###############################################################################
####   Write shapefile
###############################################################################  
if (!dir.exists(paths = "output_products/AI_grid_trawlability/")) 
  dir.create(path = "output_products/AI_grid_trawlability/")

raster::shapefile(x = ai_grid, 
                  filename = paste0("output_products/AI_grid_trawlability/",
                                    "AI_grid_trawlability.shp"))
