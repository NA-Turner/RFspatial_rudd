#ctlr shift p to get to command center (top bar)

#receiver dataframe set up
#filtering 
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(sf)
library(ggplot2)

#load in the full receiver file
#when adding in data positron will pick up on file names, just hit tab button and select pathway
recs<-read_csv("01_data/03_large_files_LFS/01_raw_files/GLATOS_receiverLocations_20260112_221824.csv")

#keep only hamilton recs
recs_ham<-recs %>% filter(glatos_array=="HAM")

#remove extra pier receivers 
##52, 55, 56, 54, 57, RETAIN 53
#remove 97 (upper spencer creek)
#92 (borers creek)
#86 (upper grindstone creek)
#remove reciver 8, deployed in 2021 and never recovered, never redeployed
#remove 14 only online in 2021 for a few months as it was then not recovered - lost

recs_ham1<-recs_ham %>% filter(!(station_no %in% c("52", "55", "56", "54", "57"
                , "97", "92", "86", "8", "14")))
unique(recs_ham1$station_no)

#want rec data from 2021 (in case any were deployed before the start date of our study)

recsham11 <- recs_ham1 %>%
  filter(deploy_date_time >= as.POSIXct("2021-01-01", tz = "UTC"))

#write csv
#write_csv(recsham11,"01_data/03_large_files_LFS/02_processed_files/Ham_recs_rudd.csv" )
recsham0<-read_csv("01_data/03_large_files_LFS/02_processed_files/Ham_recs_rudd.csv")

recsham0 <- recsham0 %>%
  mutate(recover_date_time = coalesce(recover_date_time, deploy_date_time))

#load in mapping data
HH_gcmap <- st_read("01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp")
HH_gcmap <- st_transform(HH_gcmap, crs = 4326)

ggplot(data = HH_gcmap) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_minimal()




#changed the receiver file but will have to change the detections file aswell for these
#so use the rec file to edit the detections file change these recs below to have deploy lat/lon
#as the same as the recs file. 
#change 90 2024 to be same location as 90 in 2025
#change 99 2024 to be same as 99 in 2025
#35 is redhill needs to be moved in slightly
# 59 and 67 chanve to be same as in 2025 

##make a timeline plot with time on x and rec on y for deployments and retrievals
#can go with this plot in the supplementary  
# maybe we can add colors to the recs based on orignal year deployed like in larocque paper?
#add in year initially deployed 
# Get the initial deployment year for each unique receiver
# Get the initial deployment year for each unique receiver
# Based on the earliest date for each receiver\
recsham0 <- recsham0 %>%
  mutate(
    deploy_year = as.integer(format(deploy_date_time, "%Y")),
    recovery_year = as.integer(format(
      coalesce(recover_date_time, deploy_date_time), "%Y"
    ))
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(deploy_year, recovery_year))) %>%
  unnest(year) %>%
  ungroup() %>%
  mutate(initial_deploy_year = as.character(year)) %>%
  select(-deploy_year, -recovery_year, -year)  # clean up helper columns
#plot yearly array with receiver the colour of when it was initially deployed 
#for supplemental
#keep only unique station per year


ggplot(data = HH_gcmap) +
  geom_sf(fill = "lightblue", color = "white") +
  geom_point(data = recsham0, 
             aes(x = deploy_long, y = deploy_lat, color = initial_deploy_year)) +
  facet_wrap(~initial_deploy_year) +
  labs(x = "Longitude", y = "Latitude", color = "Deployment Year") +  # legend title set here
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 12),
        axis.text.x   = element_text(size = 12),
        legend.title  = element_text(size = 14),  # controls font size of legend title
        legend.text   = element_text(size = 12))  # controls font size of legend items


#plot rec deployment retrival timeline
# Summarise deployment periods per receive 
deploy_timeline <- recsham0 %>%
  group_by(station) %>%
  summarise(start_date = min(deploy_date_time,   na.rm = TRUE),
            end_date   = max(recover_date_time,  na.rm = TRUE)) %>%
  ungroup()
# Plot timeline
# Order stations ascending
recsham0 <- recsham0 %>%
  mutate(station = factor(station, levels = sort(unique(station))))

ggplot(recsham0, aes(y = station)) +
  geom_segment(aes(x = deploy_date_time, xend = recover_date_time,
                   y = station, yend = station),
               color = "black", linewidth = 2) +
  geom_point(aes(x = deploy_date_time, y = station), 
             color = "darkgreen", size = 5, alpha=0.5) +
  geom_point(aes(x = recover_date_time, y = station),   
             color = "firebrick3", size = 3) +
  labs(x = "Date", y = "Station", title = "Receiver Deployment Timeline") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y  = element_text(size = 14),  # y axis station labels
        axis.text.x  = element_text(size = 14),  # x axis date labels
        axis.title   = element_text(size = 16),  # axis titles
        plot.title   = element_text(size = 18))  # plot title




