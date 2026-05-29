##
#building PA dataframe from cleaned and filtered detection dataframe.
#idea would be for every presence in the dataframe to add a corresponding absence location
#randomly assigned to a receiver an individual was not detected on at that time
#recs also have to have been deployed and avaliable for a possible detectin 
#so need to take deployments and winter removals into account when building the dataframe 


library(data.table)  
unique(rudd_dets$glatos_array)
#date range for the study when we have 4 or more fish avaliable at any given time period
saveRDS(rudd_dets_updated, file = "Rudddets01062026.rds")

rudd_dets_updated<- rudd_dets_updated %>% filter(detection_timestamp_EST>"2021-10-24 00:00:00")

rudd_dets_updated <- rudd_dets_updated %>%
 filter(
  !(as.Date(detection_timestamp_EST) >= as.Date("2023-04-15") & 
     as.Date(detection_timestamp_EST) <= as.Date("2023-04-21")) &
   !(as.Date(detection_timestamp_EST) >= as.Date("2023-12-24") & 
      as.Date(detection_timestamp_EST) <= as.Date("2024-04-19"))
 )

#filter out some detections on LKO rec
rudd_dets_updated<-rudd_dets_updated %>% filter(glatos_array=="HAM")

###code from management report
###split by individual and remove dets_rudd less than the min lag (i.e., a ping that was detected on more than one receiver)
#keeps the first detection from the ping tho
ind<-unique(rudd_dets_updated$animal_id)
singleping<-data.frame()

#loop to go through all individuals
for (t in 1:length(ind)){
  
  temp <- subset(rudd_dets_updated, rudd_dets_updated$animal_id == paste0(ind[t]))
  temp <- temp[order(temp$detection_timestamp_EST, decreasing=F),]
  lag<-min(temp$Min)
  
  ###### calculate and remove values with min nominal delay (~<120 s) time gap
  first_date <- temp$detection_timestamp_EST[1:(length(temp$detection_timestamp_EST)-1)]
  second_date <- temp$detection_timestamp_EST[2:length(temp$detection_timestamp_EST)]
  second_gap <- difftime(second_date, first_date, units="s")
  
  dup_index <- second_gap>lag
  dup_index <- c(TRUE, dup_index)
  temp<-temp[dup_index, ]
  
  ##################################################
  singleping<-bind_rows(singleping,temp)
}

filtered_detections1<-singleping

#saveRDS(single, paste0("./SimpleAnalyses/",spp[i],"/",spp[i],"_QAQC_dets_rudd_onercvrperping_2015-2020.rds"))
#add a date column to detections file 
filtered_detections1$date <- as.Date(filtered_detections1$detection_timestamp_EST, format = "%m/%d/%Y")

#keeps station with the most detections per fish per day 
# could change to 12 hour time bin later
#at one detection per day per individual =8001 (thats without absences)
daily <- filtered_detections1 %>% 
 group_by(transmitter_id, date, station) %>% 
 summarise(n_detections = n_distinct(detection_timestamp_utc), .groups = 'drop') %>%
 group_by(transmitter_id, date) %>%
 slice_max(n_detections, n = 1, with_ties = FALSE) %>%
 ungroup()

saveRDS(daily, "./01_data/HH_daily_presence_Rudd_Feb11.rds")

###if starting from here... 
Daily_singleping1 <- readRDS("~/For Github/Rudd-Ecology-HH1/01_data/HH_daily_presence_Rudd_Feb11.rds")
#Daily_al <- readRDS("~/For Github/Rudd-Ecology-HH1/01_data/HH_daily_presence_Rudd.rds")
#### get daily station presence #####

# extract unique bin_timestamp from the interpolated data
int <- unique(daily, by = "date")

##################################################
### load receivers #####
####load receivers from GLATOS file. This is provided with GLATOS query

#Select only station, lat, lon from big dataframe and join to small one

Recs_usedinSLpaper <- Recs_usedinSLpaper %>%
 left_join(Daily_singleping1 %>% dplyr::select(station, deploy_lat, deploy_long), by = "station")

Rudd_preppedforRFmodelPADF$station
Recs_usedinSLpaper$station

receivers<-read_glatos_receivers("./01_data/03_large_files_LFS/01_raw_files/GLATOS_receiverLocations_20260106_154310.csv")
#keep only ones that were used in SL paper 
recs_filtered <- receivers %>%
 semi_join(Recs_usedinSLpaper, by = "station")

