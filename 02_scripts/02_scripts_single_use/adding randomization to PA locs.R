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
##note** looked at assigning each receiver to a group based on methodology in the Wells paper
#however after reviewing the receiver file and depths associated with each, they did not always match up
#to Wells paper (across recs that were the same ID) so didnt want to mis assign recs or reassign recs
#from what they were previously assigned to in the wells paper
#for that reason, chose two chains to represent nearshore and offshore recs
#took the group mean of each of the two chains for summer and fall
#assigned to recs and will represent their buffer zone for the two thermal seasons
#these buffer zones will be used to create a normal random dist of detection points at recs
#for unique locations to be used in rfspatial model

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
#have data from center station for 
#2023 June 15th to November 24th 
#2024 July 30th to November 7th 
#this data doesnt show temp changes so cant calculate when thermocline/isocline establishes
#if we can get access to the center station data then we can calculate exactly the start/ends 
#for stratification in HH
# need to calculate the temp gradient between each pair of adjacent depth loggers
#apply the threshold >1'C summer or <1'C isothermal
#identify the first DOY when the gradient exceeds the 1'Cm 

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
pa_data$thermal <- ifelse(
  format(pa_data$date, "%m-%d") >= "10-06" | format(pa_data$date, "%m-%d") <= "06-06",
  "isocline",
  "thermocline"
)

unique(pa_data$station)
#station list 
[1] "HAM-032" "HAM-079" "HAM-095" "HAM-099" "HAM-041" "HAM-098" "HAM-030" "HAM-078" "HAM-044" "HAM-077" "HAM-094" "HAM-037" "HAM-063"
[14] "HAM-034" "HAM-043" "HAM-093" "HAM-003" "HAM-070" "HAM-062" "HAM-005" "HAM-042" "HAM-061" "HAM-089" "HAM-071" "HAM-047" "HAM-072"
[27] "HAM-027" "HAM-036" "HAM-090" "HAM-013" "HAM-051" "HAM-004" "HAM-076" "HAM-046" "HAM-088" "HAM-059" "HAM-007" "HAM-002" "HAM-035"
[40] "HAM-017" "HAM-080" "HAM-015" "HAM-028" "HAM-085" "HAM-068" "HAM-023" "HAM-083" "HAM-053" "HAM-087" "HAM-067" "HAM-001" "HAM-073"
[53] "HAM-082" "HAM-058" "HAM-029" "HAM-025" "HAM-074" "HAM-081" "HAM-096" "HAM-022" "HAM-084" "HAM-066" "HAM-021" "HAM-091" "HAM-011"
[66] "HAM-033" "HAM-075" "HAM-045" "HAM-052" "HAM-057" "HAM-048" "HAM-060" "HAM-039" "HAM-055" "HAM-012" "HAM-018" "HAM-009"
#assign nearshore or offshore for receiver location 
pa_data$stationloc <- ifelse(
  pa_data$station %in% c("HAM-001", "HAM-040"),
  "offshore",
  ifelse(
    df$station %in% c("HAM-045", "HAM-090"),
    "nearshore",
    NA
  )
)

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