#####
## introducting randmoization into the presence/absence dataframe 
## 
#######

#need
#PA dataframe
#receiver locations 
#buffer zones - set based on thermocline/isocline and distance set using Wells et al. paper
#April 2023 to september 4th 2025
#need thermocline dates 



#########selecting approximate buffer ranges for fall and summer 
##offshore and nearshore
#all the west end recs (offshore and nearshore assigned to chain B)

wellsrecs<-read_excel("01_data/02_processed_files/Wellsetal_suppdata_detectionrange.xlsx")

group_A <- wellsrecs[wellsrecs$Assignment == "A", ]
group_B <- wellsrecs[wellsrecs$Assignment == "B", ]
group_C <- wellsrecs[wellsrecs$Assignment == "C", ]
group_D <- wellsrecs[wellsrecs$Assignment == "D", ]
group_E <- wellsrecs[wellsrecs$Assignment == "E", ]

#use group A for pelagic offshore + south shore walls
#use group B for west end and nearshore recs

detectionrange_means<-wellsrecs %>%
  group_by(Assignment, Season) %>%
  summarise(
    group_mean = mean(Mean),
    group_min = mean(Min),
    group_max = mean(Max),
    n_depths = n()
  )


#buffers are as follows, west end and nearshore following group B
#fall range = 413
#summer range = 292

#pelagic offshore and southshore 
#fall range = 429
#summer range = 275

########################################
## Water temperatures for 2023-2025?
##
######################
#larocque et al 2024 paper
#when missing temperature data used 
#mean julian day of seasonal deliniation based on some older center station temp data and based on thermcline/isocline also
#seasons were listed as follows:
#spring: April 25 to June 6
#summer: June 7 to October 3; 
# fall: October 4 to November 17,  
# winter: November 18 to April 24

## based on Wells et al. 2021 paper 
#isocline was October 6th 
#stratified from DOY 160 is around June 9th 

#can use these dates to assign isocline thermocline to PA dataframe, 
#from that decide nearshore vs. offshore recs (in column)
#then assign the mean buffer based on rec location and thermal bin 

##assign isocline/thermocline to PA dataframe 

#file from Building PA script
#saveRDS(pa_data, file = "01_data/02_processed_files/Rudd_5active_PAdata.rds")

pa_data$thermal <- ifelse(
  format(pa_data$date, "%m-%d") >= "10-06" | format(pa_data$date, "%m-%d") <= "06-06",
  "isocline",
  "thermocline"
)

unique(pa_data$station)
#assign nearshore or offshore for receiver location 
pa_data$stationloc <- ifelse(
  pa_data$station %in% c("HAM-004", "HAM-041", "HAM-021", "HAM-017", "HAM-039"),
  "offshore",
  ifelse(
    pa_data$station %in% c( "HAM-032", "HAM-079", "HAM-095", "HAM-099", "HAM-098", "HAM-030",
  "HAM-078", "HAM-044", "HAM-077", "HAM-094", "HAM-037", "HAM-063",
  "HAM-034", "HAM-043", "HAM-093", "HAM-003", "HAM-070", "HAM-062",
  "HAM-005", "HAM-042", "HAM-061", "HAM-089", "HAM-071", "HAM-047",
  "HAM-072", "HAM-027", "HAM-036", "HAM-090", "HAM-013", "HAM-051",
  "HAM-076", "HAM-046", "HAM-088", "HAM-059", "HAM-007", "HAM-002",
  "HAM-035", "HAM-080", "HAM-015", "HAM-028", "HAM-085", "HAM-068",
  "HAM-023", "HAM-083", "HAM-053", "HAM-087", "HAM-067", "HAM-073",
  "HAM-082", "HAM-058", "HAM-029", "HAM-025", "HAM-074", "HAM-081",
  "HAM-096", "HAM-084", "HAM-066", "HAM-091", "HAM-011", "HAM-033",
  "HAM-075", "HAM-045", "HAM-052", "HAM-057", "HAM-048", "HAM-060",
   "HAM-055", "HAM-012", "HAM-018", "HAM-009", "HAM-001"),
    "nearshore",
    NA
  )
)

unique(pa_data$station)
#will have to plot thermo/iso on map to validate that all points were assigned and were assigned correctly 


ggplot(data = HH_gcmap) +
  geom_sf(fill = "lightblue", color = "white") +
  geom_point(data = pa_data, 
             aes(x = deploy_long, y = deploy_lat, color = stationloc)) +
  facet_wrap(~year) +
  labs(x = "Longitude", y = "Latitude", color = "Deployment Year") +  # legend title set here
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 12),
        axis.text.x   = element_text(size = 12),
        legend.title  = element_text(size = 14),  # controls font size of legend title
        legend.text   = element_text(size = 12))  # controls font size of legend items

##then assign the buffer range based on nearshore/offshore and thermal layer

library(dplyr)
pa_data <- pa_data %>%
  mutate(buffer_m = case_when(
    stationloc == "offshore"  & thermal == "isocline"    ~ 429,
    stationloc == "offshore"  & thermal == "thermocline" ~ 275,
    stationloc == "nearshore" & thermal == "isocline"    ~ 413,
    stationloc == "nearshore" & thermal == "thermocline" ~ 292,
    TRUE ~ NA_real_
  ))

#then from here we can add the normally distrubuted randomization to points
#using the specific buffer zones around recs at different times of the year
#and the lake polygon to ensure there are not locations randomly placed on impassable features such as land
#look at brownscombe code for this aswell to help in coding 

#issues with NAs for deploy lat and deploy long investigate


#add seasonal time binning 

pa_data <- pa_data %>%
 mutate(
  month_num = as.numeric(format(date, "%m")),
  day_num = as.numeric(format(date, "%d")),
  season = case_when(
   (month_num == 4 & day_num >= 1) | month_num == 5 | (month_num == 6 & day_num <= 15) ~ "Spring",
   (month_num == 16 & day_num >= 7) | month_num %in% c(7, 8, 9) | (month_num == 10 & day_num <= 3) ~ "Summer",
   (month_num == 10 & day_num >= 4) | month_num == 11 & day_num <= 15 ~ "Fall",
   (month_num == 11 & day_num >= 16) | month_num %in% c(12, 1, 2) | (month_num == 3 & day_num <= 31) ~ "Winter",
   TRUE ~ NA_character_
  )
 )

#saveRDS(pa_data,file = "01_data/02_processed_files/PA RFspatial Rudd.rds")

