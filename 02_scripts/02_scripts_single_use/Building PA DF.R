##
#building PA dataframe from cleaned and filtered detection dataframe.
#spatial RF models cannot handle large dataframes so need to ensure we have 10k rows or less

library(data.table)  
unique(rudd_dets$glatos_array)
#date range for the study when we have 4 or more fish avaliable at any given time period
rudd_dets_updated<-#readRDS("c:/Users/TURNERN/Documents/For Github/RFspatial_rudd/01_data/03_large_files_LFS/Rudddets01062026.rds")

#this is for 4 fish active at one time
#daily presence= 8000 detections i.e too much for a RFspatial model to handle
#rudd_dets_updated<- rudd_dets_updated %>% filter(detection_timestamp_EST>"2021-10-24 00:00:00")
#rudd_dets_updated <- rudd_dets_updated %>%
 #filter(
 # !(as.Date(detection_timestamp_EST) >= as.Date("2023-04-15") & 
 #    as.Date(detection_timestamp_EST) <= as.Date("2023-04-21")) &
 #  !(as.Date(detection_timestamp_EST) >= as.Date("2023-12-24") & 
 #     as.Date(detection_timestamp_EST) <= as.Date("2024-04-19"))
 #)

rudd_dets_updated <- rudd_dets_updated %>%
  filter(
    (as.Date(detection_timestamp_EST) >= as.Date("2023-04-21") &
     as.Date(detection_timestamp_EST) <= as.Date("2023-12-09")) |
    (as.Date(detection_timestamp_EST) >= as.Date("2024-04-20") &
     as.Date(detection_timestamp_EST) <= as.Date("2025-09-04"))
  )

#filter out some detections on LKO rec
rudd_dets_updated<-rudd_dets_updated %>% filter(glatos_array=="HAM")

rudd_dets_updated1<-rudd_dets_updated %>% filter(!(station_no %in% c("52", "55", "56", "54", "57"
                , "97", "92", "86", "8", "14")))

###code from management report
###split by individual and remove dets_rudd less than the min lag (i.e., a ping that was detected on more than one receiver)
#keeps the first detection from the ping tho
ind<-unique(rudd_dets_updated1$animal_id)
singleping<-data.frame()

#loop to go through all individuals
for (t in 1:length(ind)){
  
  temp <- subset(rudd_dets_updated1, rudd_dets_updated1$animal_id == paste0(ind[t]))
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

filtered_detections1$date <- as.Date(filtered_detections1$detection_timestamp_EST, format = "%m/%d/%Y")

#keeps station with the most detections per fish per day 
#at one detection per day per individual =8001 (thats without absences)
daily <- filtered_detections1 %>% 
 group_by(transmitter_id, date, station) %>% 
 summarise(n_detections = n_distinct(detection_timestamp_utc), .groups = 'drop') %>%
 group_by(transmitter_id, date) %>%
 slice_max(n_detections, n = 1, with_ties = FALSE) %>%
 ungroup()

##################################################
### load receivers #####
####load receivers from GLATOS file. This is provided with GLATOS query

#Select only station, lat, lon from big dataframe and join to small one
#join receiver informtion (deploy lat and lon to detection file)
#recsham0<-read_csv("01_data/03_large_files_LFS/02_processed_files/Ham_recs_rudd.csv")

#recsham0 <- recsham0 %>%
#  mutate(recover_date_time = coalesce(recover_date_time, deploy_date_time))


daily$station<-as.factor(daily$station)
daily$year<-format(daily$date, "%Y")
daily$year<-as.factor(daily$year)
recsham0$initial_deploy_year<-as.factor(recsham0$initial_deploy_year)
#some duplicates due to battery changes in the same year so just keep one station per year 
recsham0_unique <- recsham0 %>%
  distinct(station, initial_deploy_year, .keep_all = TRUE)

########################################################################
#write.csv(recsham0_unique, "hamrecs_rudd2023 to 2025 unique.csv")

recsham0_unique <- recsham0_unique %>% 
  rename(year = initial_deploy_year)


daily1 <- daily %>%
  left_join(
    recsham0_unique %>%
      select(station, deploy_lat, deploy_long, year),
    by = c("station", "year")
  )



#this file now has one detection per individual per day
#linked to station and lat/lon location of the station 
#saveRDS(daily1, "01_data/02_processed_files/Rudd daily presence 5active.rds")

##need to assign random absences
#random absences need to be assigned per individual from the above dataframe
#and randomly assigned one location where a receiver was avaliable to hear a detection i.e. deployed
############
#assigning absences#####
################

# ── 1. Ensure date columns are the same type ─────────────────────────────────
daily1 <- daily1 |>
  mutate(date = as.Date(date))

recsham0_unique <- recsham0_unique |>
  mutate(
    deploy_date_time  = as.Date(deploy_date_time),
    recover_date_time = as.Date(recover_date_time)
  )

# ── 2. Helper: pick a random absence station for one detection ───────────────
sample_absence_receiver <- function(det_station, det_date, receivers_df) {
  
  eligible <- receivers_df |>
    filter(
      deploy_date_time  <= det_date,    # receiver already deployed
      recover_date_time >= det_date,    # receiver not yet recovered
      station           != det_station  # exclude the detected station
    )
  
  eligible |> slice_sample(n = 1)
}

# ── 3. Build pseudo-absence rows ─────────────────────────────────────────────
set.seed(42)
library(purrr)

absences <- daily1 |>
  mutate(
    absence_receiver = pmap(
      list(station, date),
      \(st, dt) sample_absence_receiver(st, dt, recsham0)
    )
  ) |>
  mutate(
    absence_station    = map_chr(absence_receiver, "station"),
    absence_deploy_lat = map_dbl(absence_receiver, "deploy_lat"),
    absence_deploy_long = map_dbl(absence_receiver, "deploy_long")
  ) |>
  select(-absence_receiver) |>
  transmute(
    transmitter_id = transmitter_id,
    date           = date,
    station        = absence_station,
    n_detections   = 0L,          # zero detections at absence location
    year           = year,
    deploy_lat     = absence_deploy_lat,
    deploy_long    = absence_deploy_long,
    presence       = 0L
  )

# ── 4. Label presences and bind ───────────────────────────────────────────────
presences <- daily1 |>
  mutate(presence = 1L)

pa_data <- bind_rows(presences, absences) |>
  arrange(transmitter_id, date)

# ── 5. Sanity checks ──────────────────────────────────────────────────────────
stopifnot(nrow(pa_data) == 2 * nrow(daily1))

pair_check <- pa_data |>
  group_by(transmitter_id, date) |>
  summarise(
    n_rows      = n(),
    n_stations  = n_distinct(station),
    .groups     = "drop"
  )

stopifnot(all(pair_check$n_rows == 2))
stopifnot(all(pair_check$n_stations == 2))

cat("Done.\n")
cat("Total rows:", nrow(pa_data),
    "| Presences:", sum(pa_data$presence),
    "| Absences:",  sum(pa_data$presence == 0L), "\n")


########QAQC checks of the above ######
##all pass
# ── 2. Check no absence station matches its paired presence station ───────────

paired_check <- pa_data |>
  group_by(transmitter_id, date) |>
  summarise(
    presence_station = station[presence == 1L],
    absence_station  = station[presence == 0L],
    .groups = "drop"
  ) |>
  mutate(same_station = presence_station == absence_station)

same_station_violations <- paired_check |> filter(same_station)

if (nrow(same_station_violations) == 0) {
  cat("PASS: No absence shares a station with its paired presence.\n")
} else {
  cat("FAIL:", nrow(same_station_violations), "pairs share the same station!\n")
  print(same_station_violations)
}

# ── 3. Check absence coverage across receivers ────────────────────────────────
# Useful to spot if any receiver is never/always selected as an absence
# (could indicate a bias in the random sampling)

absence_freq <- pa_data |>
  filter(presence == 0L) |>
  count(station, name = "n_times_as_absence") |>
  left_join(
    recsham0 |> select(station, deploy_date_time, recover_date_time),
    by = "station"
  ) |>
  mutate(
    days_deployed = as.numeric(recover_date_time - deploy_date_time)
  ) |>
  arrange(desc(n_times_as_absence))

cat("\nAbsence frequency per receiver:\n")
print(absence_freq |> select(station, n_times_as_absence, days_deployed))

# ── 4. Spot-check a specific receiver you know was offline ───────────────────
# Replace with a station name and date range you know it was out of the water

known_offline_station <- "HAM-045"        # <-- change to your station
known_offline_start   <- as.Date("2023-09-13")   # <-- change to offline period
known_offline_end     <- as.Date("2025-07-03")   # <-- change to offline period

spot_check <- pa_data |>
  filter(
    presence == 0L,
    station  == known_offline_station,
    date     >= known_offline_start,
    date     <= known_offline_end
  )

if (nrow(spot_check) == 0) {
  cat("\nPASS: Spot check — no absences at", known_offline_station,
      "during its known offline period.\n")
} else {
  cat("\nFAIL:", nrow(spot_check), "absences incorrectly placed at",
      known_offline_station, "while it was offline!\n")
  print(spot_check)
}

# ── 5. Summary table of presence/absence counts per transmitter ───────────────
# Quick sanity that every fish has equal presences and absences

balance_check <- pa_data |>
  group_by(transmitter_id, presence) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = presence, values_from = n,
              names_prefix = "presence_") |>
  rename(n_absences = presence_0, n_presences = presence_1) |>
  mutate(balanced = n_presences == n_absences)

cat("\nPer-fish balance check:\n")
print(balance_check)

if (all(balance_check$balanced)) {
  cat("PASS: Every fish has equal presences and absences.\n")
} else {
  cat("FAIL: Some fish have unequal presence/absence counts!\n")
}


#save the dataframe 
#saveRDS(pa_data, file = "Rudd_5active_PAdata.rds")
