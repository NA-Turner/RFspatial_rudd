#SpatialRF

#datafile wtih balance presence absence
#normally distributed random PAs with unique locations
#buffer ranges by iso/thermo accounted for
#5 active tags on the array one any given time
#dataframe to load in called PA_data_randomized


################
##load in R packages
##########


#receiver data file with habitat data - not sure if required anymore?
#as all habitat data will be unique to the unique locations?
#load in receiver with habitat data 
hab_recs <- read.csv("./01_data/03_large_files_LFS/02_processed_files/HH_daily_receiver_presence__habitat_static_latlon.rds")
Rudd_recs<-read.csv('01_data/02_processed_files/Ham_recs_rudd.csv')
#from SL model - maybe different now need to check
### remove receiver 43 due to inaccurate substrate info and is a travel corridor, not habitat for fish.
#hab_daily <- hab_daily %>% filter(!station=="HAM-043")
#hab_recs <- hab_recs %>% filter(!station=="HAM-043")


#current predictor variables
#for SL all info would be based on the 350m buffer, will need to have it redone for our two buffer sizes
#for SL paper
#used the mean value of depth, secchi, hard substrate, SAV within the 350m range of rec was used
#fetch calculated at rec station only 
#50m grid of harbour developed (do not have this)


#assign each unique location to a depth/habitat/substrate etc. 
#will need raster layers and pull from there? GIS or in R
# could be the mean of the buffer zone as we dont actuall yknow where the fish was since they are randomly distributed?

#For layers
#SpatialRF does not support categorical or factor responses

#SAV layer needs work - missing in eastern section (walleye spawning area) could we use SAVM?
#would not use the hard substate layer as it currently is 
#recalcuate fetch using SAVM (would assign unique locations to unique fetch - seems like SL used line-of-sight scrubbing
# to get around this)
# #distance to wetland and river mouth has been calculated but will have to fill in missing recs that are now new
#depth
#  COULD ADD
#distance to shoreline

#would not use
#secchi layer - doesnt seem great 
#% hard unless it gets cleaned up 

#what is dist WL_RH??

habitat_more<-read.csv("./01_data/03_large_files_LFS/02_processed_files/SL layers/hh_hard_substrate_rcvrbuff_aug2023.csv")
habitat2_more<-habitat_more %>% dplyr::select(station, mean_prop_hard)

hab_recs<-left_join(hab_recs,habitat2_more, by="station")
hab_daily<-left_join(hab_daily, habitat2_more, by="station")

habitat_more2<-read.csv("./01_data/03_large_files_LFS/02_processed_files/SL layers/HH_Receivers_DistWetland_RiverMouth_EVPresence_wRedhIllMarsh_Aug2023.csv")
habitat2_more2<-habitat_more2 %>% dplyr::select(station, RM_DistFix,Emerg_Pres,WL_DistFix,ClosestWL)

hab_recs<-left_join(hab_recs,habitat2_more2, by="station")
hab_daily<-left_join(hab_daily, habitat2_more2, by="station")

 

##updated SAV from water level 75m
habitat_more4<-read.csv("./01_data/03_large_files_LFS/02_processed_files/SL layers/hh_rcvr_350buff_75SAV_june2024.csv")

hab_recs<-left_join(hab_recs,habitat_more4, by="station")
hab_daily<-left_join(hab_daily, habitat_more4, by="station")


## updated layers from SL
#need SAV layer updated to be mean of new buffer zones 
#thermocline and isocline 

#will run new fetch based on SAVM 

#receiver list
#for updated enviromental varaiables 
#difference in recs in my vs. SL sutdy

#differences in stations between SL study and my study 
# Assuming your dataframes are df1 and df2, each with a column called "station"
# Adjust the column name as needed

stations1 <- unique(SL_recs$station)
stations2 <- unique(Rudd_recs$station)

# Stations in df1 but NOT in df2
only_in_df1 <- setdiff(stations1, stations2)

# Stations in df2 but NOT in df1
only_in_df2 <- setdiff(stations2, stations1)

# Stations in BOTH
in_both <- intersect(stations1, stations2)

# Summary
cat("=== Station Comparison Summary ===\n")
cat("Stations in df1:          ", length(stations1), "\n")
cat("Stations in df2:          ", length(stations2), "\n")
cat("Stations in both:         ", length(in_both), "\n")
cat("Only in df1:              ", length(only_in_df1), "\n")
cat("Only in df2:              ", length(only_in_df2), "\n")

cat("\n--- Stations only in df1 ---\n")
print(sort(only_in_df1))

cat("\n--- Stations only in df2 ---\n")
print(sort(only_in_df2))

cat("\n--- Stations in both ---\n")
print(sort(in_both))

#stations that are new to the analysis since SL paper - 28 total 

#[1] "HAM-068" "HAM-070" "HAM-071" "HAM-072" "HAM-073" "HAM-074" "HAM-075" "HAM-076" "HAM-077" "HAM-078"
#[11] "HAM-079" "HAM-080" "HAM-081" "HAM-082" "HAM-083" "HAM-084" "HAM-085" "HAM-087" "HAM-088" "HAM-089"
#[21] "HAM-090" "HAM-091" "HAM-093" "HAM-094" "HAM-095" "HAM-096" "HAM-098" "HAM-099"