###adjustd code from JDM
#Turner 2026
#gets hourly weighted fetch wind direction 

## needs to run in base R
library(pacman)
library(openair)
library(plyr)
library(lubridate)
library(tidyr)

#need to get a fetch calcualtion only based on thermal season
#need to get a SAV map based on fetch, depth, secchi for growing sesaon only. 
#during non growing sesaon will be 0 SAV - can we just link this to thermal season also???
#alternatively just set the growing season seperate from non growing season ?


library(purrr)
#base_dir <- "01_data/01_raw_files/monthly wind data 2024" #point to the folder all your csv files are located in 

#files <- list.files(
#  path = base_dir,
#  pattern = "\\.csv$",
#  recursive = TRUE,
#  full.names = TRUE
#)
#wind_data <- map_dfr(files, read.csv)
#save as .csv combined now 

#add to the wind data
wind_data$Date.Time..LST. <- as.POSIXct(
  wind_data$Date.Time..LST.,
  format = "%Y-%m-%d %H:%M",
  tz = "America/Toronto"
)

wind_data$thermal <- ifelse(
  format(wind_data$Date.Time..LST., "%m-%d") >= "10-06" | format(wind_data$Date.Time..LST., "%m-%d") <= "06-06",
  "isocline",
  "thermocline"
)
#write.csv(wind_data, "01_data/02_processed_files/wind data 2024 Burl pier.csv")

#now build wind weighted fetch for thermal seasons 

wind_data_isocline <- dplyr::filter(wind_data, thermal == "isocline")
#isocline
wind_data_isocline<-plyr::rename(wind_data_isocline,c("Wind.Dir..10s.deg."="wd","Wind.Spd..km.h."="ws"))


#need to get a fetch calcualtion only based on thermal season
#need to get a SAV map based on fetch, depth, secchi for growing sesaon only. 
#during non growing sesaon will be 0 SAV - can we just link this to thermal season also???
#alternatively just set the growing season seperate from non growing season ?

#compare a weighted wind fetch for growing season vs. if we just incoporate the whole yer

#wind weighted fetch based on thermal seaosn 

#also then get SAVM to get SAV est. using the thermocline wind weightd fetch to estimate. 
#orginal EC is in tens of degrees so multipy to convert to standard degrees (0-360)
wind_data_isocline$wd<-wind_data_isocline$wd*10
#speed is in km/h so multiply by 1000/3600 converst to m/s
wind_data_isocline$ws<-wind_data_isocline$ws*1000/3600
 
## Assess direction frequency (can adjust angles to match fetch calc)
#windrose from openair pacakge
#windrose bins wind observations into direction sectors and calculates frequency in each

test<-windRose(wind_data_isocline) #default = 12 sectors 30' each
nrow(test$data) 

wtest<-windRose(wind_data_isocline,angle=22.5) #16 
nrow(wtest$data) ## 16 angles

wtest2<-windRose(wind_data_isocline,angle=11.25) #gives 32 sectors at 11.25'C
nrow(wtest2$data) 
 
#going with 16

bearing<-data.frame(wtest$data)
bearing<-bearing[!(bearing$wd==-999),]
#makes sure 0 and 360 are not double counted
bearing$direction<-ifelse(bearing$wd<360,bearing$wd,0)
sum(bearing$freqs) ## total records
 
#now compute the proportion of time from each direction
b.2<-bearing
b.2$total.rec<-sum(b.2$freqs)
#calculate the proportion of time the wind blew from each direction bin 
b.2$prop.time<-b.2$freqs/b.2$total.rec

#write csv

#write.csv(b.2, "01_data/02_processed_files/for SAVM/isocline wind prop 2024.csv" )

#thermocline 

wind_data_thermocline <- dplyr::filter(wind_data, thermal == "thermocline")

#thermocline
wind_data_thermocline<-plyr::rename(wind_data_thermocline,c("Wind.Dir..10s.deg."="wd","Wind.Spd..km.h."="ws"))


#need to get a fetch calcualtion only based on thermal season
#need to get a SAV map based on fetch, depth, secchi for growing sesaon only. 
#during non growing sesaon will be 0 SAV - can we just link this to thermal season also???
#alternatively just set the growing season seperate from non growing season ?

#compare a weighted wind fetch for growing season vs. if we just incoporate the whole yer

#wind weighted fetch based on thermal seaosn 

#also then get SAVM to get SAV est. using the thermocline wind weightd fetch to estimate. 
#orginal EC is in tens of degrees so multipy to convert to standard degrees (0-360)
wind_data_thermocline$wd<-wind_data_thermocline$wd*10
#speed is in km/h so multiply by 1000/3600 converst to m/s
wind_data_thermocline$ws<-wind_data_thermocline$ws*1000/3600
 
## Assess direction frequency (can adjust angles to match fetch calc)
#windrose from openair pacakge
#windrose bins wind observations into direction sectors and calculates frequency in each

test1<-windRose(wind_data_thermocline) #default = 12 sectors 30' each
nrow(test$data) 

wtest_ther<-windRose(wind_data_thermocline,angle=22.5) #16 
nrow(wtest_ther$data) ## 16 angles

wtest2_ther<-windRose(wind_data_thermocline,angle=11.25) #gives 32 sectors at 11.25'C
nrow(wtest_ther$data) 
 
#save dataframes 
#going with 16 

bearing1<-data.frame(wtest_ther$data)
bearing1<-bearing1[!(bearing1$wd==-999),]
#makes sure 0 and 360 are not double counted
bearing1$direction<-ifelse(bearing1$wd<360,bearing1$wd,0)
sum(bearing1$freqs) ## total records
 
#now compute the proportion of time from each direction
b.22<-bearing1
b.22$total.rec<-sum(b.22$freqs)
#calculate the proportion of time the wind blew from each direction bin 
b.22$prop.time<-b.22$freqs/b.22$total.rec

#write csv
#write.csv(b.22, "01_data/02_processed_files/for SAVM/thermocline wind prop 2024.csv" )

