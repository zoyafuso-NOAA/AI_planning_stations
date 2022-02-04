###############################################################################
## Project:       Prioritize 
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   
###############################################################################
rm(list = ls())

# library(AIGOASurveyPlanning)
# goa.planning()

allocation_420 <- read.csv(paste0("data/AI_allocation_preprocessed/",
                                  "stations_420/AIallocation420.csv"))
allocation_410 <- read.csv(paste0("data/AI_allocation_preprocessed/",
                                  "stations_410/AIallocation410.csv"))
allocation_400 <- read.csv(paste0("data/AI_allocation_preprocessed/",
                                  "stations_400/AIallocation400.csv"))
allocation_390 <- read.csv(paste0("data/AI_allocation_preprocessed/",
                                  "stations_390/AIallocation390.csv"))
allocation_380 <- read.csv(paste0("data/AI_allocation_preprocessed/",
                                  "stations_380/AIallocation380.csv"))
allocation_370 <- read.csv(paste0("data/AI_allocation_preprocessed/",
                                  "stations_370/AIallocation370.csv"))

allocations <- cbind(stations_420 = table(allocation_420$STRATUM),
                     stations_410 = table(allocation_410$STRATUM),
                     stations_400 = table(allocation_400$STRATUM),
                     stations_390 = table(allocation_390$STRATUM),
                     stations_380 = table(allocation_380$STRATUM),
                     stations_370 = table(allocation_370$STRATUM))

station_drops <- t(apply(X = allocations, 
                         MARGIN = 1, 
                         FUN = function(x) -diff(x) ))
station_drops <- station_drops[apply(X = station_drops, 
                                     MARGIN = 1, 
                                     FUN = function(x) !all(x == 0) ), ]

strata_drops <-
  row.names(station_drops)[apply(X = station_drops,
                                 MARGIN = 2,
                                 FUN = function(x) which(x == 1))]
strata_drops <-
  matrix(data = as.integer(strata_drops),
         nrow = 10,
         dimnames = list(NULL,
                         paste0(seq(410, 370, -10), " stations")))


set.seed(2022)
full_allocation <- allocation_420[, c("VESSEL", "STRATUM", "STATIONID")]
removed_stations <- data.frame()

ivessel <- c('-1' = 143, '1' = 148)[paste(-1)]
for (ieffort in seq(from = 410, to = 370, by = -10)) {
  temp_drop <- table(strata_drops[, paste0(ieffort, " stations")])
  names_temp_drop <- as.integer(names(temp_drop))
  
  removed_idx <- c()
  for (istratum in names_temp_drop) {
    idrop_n <- temp_drop[paste(istratum)]
    
    for (isample in 1:idrop_n) {
      idx <- sample(x = which(full_allocation$STRATUM == istratum & 
                                full_allocation$VESSEL == ivessel), 
                    size = isample)
      ivessel <- 
        c('-1' = 143, 
          '1' = "148")[paste(-1 * as.integer(names(ivessel)))]
    }
    
    
    managmeent_area <- switch(substr(x = istratum, start = 1, stop = 1),
                              '2' = "West",
                              "3" = "Central", "4" = "Central",
                              "5" = "East", "6" = "East",
                              "7" = "SBS")
    
    removed_stations <- 
      rbind(removed_stations,
            data.frame(managmeent_area = managmeent_area,
                       STRATUM = istratum, 
                       stationid = full_allocation$STATIONID[idx],
                       vessel = ivessel,
                       total_n = ieffort))
    removed_idx <- c(removed_idx, idx)
  }
  
  full_allocation <- full_allocation[-removed_idx, ]
}

allocation_420[sample(x = with(allocation_420, 
                               which(STRATUM == 213 & VESSEL == 148)),
                      size = 1), ]
