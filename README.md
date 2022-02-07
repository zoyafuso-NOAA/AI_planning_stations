# Station allocations tasks for the Aleutian Island (AI) Bottom Trawl Survey:

Scripts in the [cohttps://github.com/zoyafuso-NOAA/AI_planning_stations/tree/main/code](url) directory complete these tasks:

## [code/get_allocation.R](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/code/get_allocation.R)

Use the [AIGOA_SurveyPlanning R package](https://github.com/afsc-gap-products/AIGOASurveyPlanning) (to be renamed in the future) to calculate the allocation of stations across strata, draw locations of stations, and assign stations to the two vessels. Allocations are provided for a range of total sample sizes (370 to 420 stations, intervals of 10 stations) and outputted in the [data/AI_allocation_preprocessed](https://github.com/zoyafuso-NOAA/AI_planning_stations/tree/main/data/AI_allocation_preprocessed) directory. This is the goa.planning() function written by Ned L. and Paul von S. contained when calling `load("G:/GOA/R/survey planning/.RData")` in R.

## [code/strata_choices.R](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/code/strata_choices.R)

To fulfill the priority of integrating new previously explored stations into the survey, we devise a system to integrate 10-12 new stations: 

1) Distinguish each AI stratum as either a “thin” stratum or a “large” stratum based on aspects of the stratum shape and size. The metric we use is the ratio of the total perimeter to total area (P:A): thin strata will have higher values and more compact strata will have lower values. Strata with P:A ratios less than the mean P:A across strata (mean(P:A)) are distinguished as large strata, and “thin” strata otherwise. Additionally we also take into account the total area in our definitions of large vs thin strata: 1000 km^2 is chosen as a cutoff between large and thin strata. Strata with total area > 1000 km^2 AND with a P:A ratio < mean(P:A) are considered large strata, and thin otherwise. For each fishery management area ([Western AI](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/output_plots/large_vs_thin_strata_Western.pdf), [Central AI](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/output_plots/large_vs_thin_strata_Central.pdf), [Eastern AI](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/output_plots/large_vs_thin_strata_Eastern.pdf), and Southern Bering Sea ([SBS](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/output_plots/large_vs_thin_strata_SBS.pdf))) an output plot is produced showing which strata were identified as either large or thin. The location of drawn stations are superimposed and labeled by vessel. 
2) Once strata are identified as either large or thin, one large and one thin stratum are randomly chosen in  for a total of eight strata. [chosen_strata.pdf](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/output_plots/chosen_strata.pdf) shows which strata were chosen in each fisheries management area.
3) Output a new [allocation spreadsheet](https://github.com/zoyafuso-NOAA/AI_planning_stations/blob/main/output_products/AIallocation420.xlsx) in the [output_products/](https://github.com/zoyafuso-NOAA/AI_planning_stations/tree/main/output_products) directory that removes the stationid, long, and lat info for
a) two randomly chosen stations for each selected large stratum: one station for vessel V1 and one station for vessel V2 for a total of 8 removed stations
b) one randomly chosen station for each selected thin stratum for a total of two removed stations for vessel V1 and two removed stations for vessel V2. Note, this results in 12 potential new stations. 

## code/trawlable_grids.R

