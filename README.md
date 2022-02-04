# AI_planning_stations
 This repo holds the code and instructions for completing station allocations tasks for the Aleutian Island (AI) Bottom Trawl Survey:
 
 * Allocation between boats and strata
 * Lat-longs to Mark Z & Paul
 * Update survey goa grid GIS or AI grid GIS
 * Station progress spreadsheet

Scripts in the code/ directory complete these tasks:

## code/get_allocation.R

Uses the AIGOA_SurveyPlanning R package (to be renamed in the future) to calculate the allocation of a target 420 stations across strata, draw locations of stations, and assign stations to the two vessels. This is the goa.planning() function written by Ned L. and Paul von S. contained when calling `load("G:/GOA/R/survey planning/.RData")` in R.

## code/strata_choices.R: 

To fulfill the priority of integrating new previously explored stations into the survey, we devised a system to integrate 10-12 new stations: 

1) Distinguish each AI stratum as either a “thin” stratum or a “large” stratum based on aspects of the stratum shape and size. The metric we use is the ratio of the total perimeter to total area (P:A): thin strata will have higher values and more compact strata will have lower values. Strata with P:A ratios less than the mean P:A across strata (mean(P:A)) are distinguished as large strata, and “thin” strata otherwise. Additionally we also take into account the total area in our definitions of large vs thin strata: 1000 km^2 is chosen as a cutoff between large and thin strata. Strata with total area > 1000 km^2 AND with a P:A ratio < mean(P:A) are considered large strata, and thin otherwise. For each fishery management area (Western AI, Central AI, Eastern AI, and SBS) an output plot is produced showing which strata were identified as either large or thin. The location of drawn stations are superimposed and labeled by vessel. 
2) Once strata are identified as either large or thin, one large and one thin stratum are randomly chosen in  for a total of eight strata. chosen_strata.pdf shows which strata were chosen in each fisheries management area.
3) Output a new allocation spreadsheet that removes the stationid, long, and lat info for
a) two randomly chosen stations for each selected large stratum: one station for vessel 176 and one station for vessel V2 for a total of 8 removed stations
b) one randomly chosen station for each selected thin stratum for a total of two removed stations for vessel 176 and two removed stations for vessel V2. Note, this results in 12 potential new stations. 

## code/trawlable_grids.R

