#SpatialRF

#datafile wtih balance presence absence
#normally distributed random PAs with unique locations
#buffer ranges by iso/thermo accounted for
#5 active tags on the array one any given time
#dataframe to load in called PA_data_randomized


################
##load in R packages
##########
library(terra)
library(sf)
library(dplyr)



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

#apparently a slope layer? but dont ahve a tiff of that


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


new_stationlist <- Rudd_recs %>% select(station, deploy_lat, deploy_long, deploy_date_time, recover_date_time)
write.csv(new_stationlist, "station list for Rudd_2026.csv")

#we know the bathy layer is good, so can we use that to extract exact depth points of the P/A data

# ── 1. Load the depth raster ──────────────────────────────────────────────────
depth_raster <- rast("01_data/04_shapefiles/Enviro layers/WL.tif")

# Quick check
print(depth_raster)
plot(depth_raster)  # sanity check visual
# ── 2. Get unique lat/lon locations ──────────────────────────────────────────
# Assuming your data has columns: station, deploy_lat, deploy_long
# (adjust column names to match your actual dataframe)
#01_data\02_processed_files\PAdata_randomized.rds

unique_locs <- det_randomized |>
  distinct(transmitter_id, date, station, year, rand_long, rand_lat)

# ── 3. Convert to sf points — use WGS84 (EPSG:4326) since coords are lat/lon ─
locs_sf <- st_as_sf(unique_locs,
                    coords = c("rand_long", "rand_lat"),
                    crs    = 4326)

# ── 4. Reproject points to match the raster CRS ──────────────────────────────
# terra and sf need matching CRS for extraction to work correctly
raster_crs <- crs(depth_raster)
locs_projected <- st_transform(locs_sf, crs = raster_crs)

# Convert to SpatVector for terra::extract()
locs_vect <- vect(locs_projected)
# ── 5. Extract depth values ───────────────────────────────────────────────────
depth_vals <- terra::extract(depth_raster, locs_vect)
# Returns a dataframe with columns: ID (row index) + raster layer name(s)

# ── 6. Join by ID (row index) — safe regardless of ordering ──────────────────
# terra's ID column corresponds to the row number of locs_vect
unique_locs_depth <- unique_locs |>
  mutate(ID = row_number()) |>                        # create matching ID
  left_join(depth_vals, by = "ID") |>                 # join on position
  rename(depth_m = names(depth_raster)[1]) |>         # rename by layer name
  select(-ID)


# ── Split into complete and NA sets ──────────────────────────────────────────
locs_complete <- unique_locs_depth |> filter(!is.na(depth_m))
locs_na       <- unique_locs_depth |> filter(is.na(depth_m))

# ── Convert both to sf objects ────────────────────────────────────────────────
complete_sf <- st_as_sf(locs_complete,
                        coords = c("rand_long", "rand_lat"),
                        crs = 4326)

na_sf <- st_as_sf(locs_na,
                  coords = c("rand_long", "rand_lat"),
                  crs = 4326)

# ── For each NA point, find the nearest non-NA point ─────────────────────────
nearest_idx <- st_nearest_feature(na_sf, complete_sf)

# ── Borrow depth from the nearest complete point ──────────────────────────────
locs_na <- locs_na |>
  mutate(depth_m = locs_complete$depth_m[nearest_idx])

# ── Recombine ─────────────────────────────────────────────────────────────────
unique_locs_depth <- bind_rows(locs_complete, locs_na)

# ── Join back to pa_data ──────────────────────────────────────────────────────
det_randomized <- det_randomized |>
  select(-depth_m) |>                          # drop the old NA-containing column
  left_join(unique_locs_depth,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))

#save for now can delete later once we add in more variables 
saveRDS(det_randomized, "PA rand with depth.rds")

library(ggplot2)

ggplot(unique_locs_depth, aes(x = rand_long, y = rand_lat, colour = depth_m)) +
  geom_point(size = 2) +
  scale_colour_viridis_c(option = "mako", direction = -1, name = "Depth (m)") +
  theme_minimal() +
  labs(title = "Station depths", x = "Longitude", y = "Latitude")


#have to think about receiver line of sight for some areas
#grindstone and cootes paradise marsh will have to check
