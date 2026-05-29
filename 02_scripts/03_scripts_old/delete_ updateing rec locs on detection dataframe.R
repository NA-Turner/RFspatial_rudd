###generate absence location for each presence location in the cleaned Rudd detection dataframe
##can be deleted 
#load in filtered rudd detection dataframe 
rudd_dets <- readRDS("01_data/03_large_files_LFS/02_processed_files/rudd_detections_QAQC.rds")


#make adjustments to receiver lcoations that were slighlty adjusted 
#to be inside of the HH polygon 

library(dplyr)

#recs file
recsham0<-read_csv("01_data/03_large_files_LFS/02_processed_files/Ham_recs_rudd.csv")


# --- Step 1: Select only the columns you need from receivers ---
# Adjust column names to match yours exactly
receiver_coords <- initial_deploy %>%
  select(station, deploy_lat, deploy_long)  # keep only what you need

# --- Step 3: Join updated coordinates onto detections ---
rudd_dets_updated <- rudd_dets %>%
  left_join(receiver_coords, by = "station")  # replace "receiver_id" with your actual key column

# --- Step 4: Check the result ---
# Confirm no NAs introduced (would mean a receiver ID didn't match)
rudd_dets_updated %>%
  filter(is.na(deploy_lat.y) | is.na(deploy_long.y)) %>%
  nrow()  # should be 0

#clean up the columns in the detection dataframe
rudd_dets_updated <- rudd_dets_updated[ , -c(15, 20, 21, 22)] # Removes the 1st and 3rd columns

rudd_dets_updated <- rudd_dets_updated %>% 
  rename(deploy_lat = deploy_lat.y)

rudd_dets_updated <- rudd_dets_updated %>% 
  rename(deploy_long = deploy_long.y)

# --- Step 5: Save ---
saveRDS(rudd_dets_updated, file = "Rudddets01062026.rds")

ham059<-rudd_dets_updated %>% filter(station=="HAM-059")
unique(ham059$deploy_lat)
unique(ham059$deploy_long)
