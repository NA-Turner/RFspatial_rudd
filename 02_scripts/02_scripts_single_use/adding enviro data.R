#SpatialRF

#datafile wtih balance presence absence
#normally distributed random PAs with unique locations
#buffer ranges by iso/thermo accounted for
#5 active tags on the array one any given time
#dataframe to load in called PA_data_randomized
#current predictor variables
#Larocque et al. 2025 paper used a mean value for the set buffer size of 350m
#although we have different buffer sizes we are not using mean values but exact unique location 
#values for each presence absence detection based on the habitat layers. 



################
##load in R packages
##########
library(terra)
library(sf)
library(dplyr)


#lake ontario water levels for 2024 and 2025 were right in line with the historical average 
#~75msl for the summer period 

##REMEMBER##
#SpatialRF does not support categorical or factor responses


#####
## SL removed rec 43 from analysis do we also want to do the same ?!
### remove receiver 43 due to inaccurate substrate info and is a travel corridor, not habitat for fish.
#hab_daily <- hab_daily %>% filter(!station=="HAM-043")



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
#stations that are new to the analysis since SL paper - 28 total 

#[1] "HAM-068" "HAM-070" "HAM-071" "HAM-072" "HAM-073" "HAM-074" "HAM-075" "HAM-076" "HAM-077" "HAM-078"
#[11] "HAM-079" "HAM-080" "HAM-081" "HAM-082" "HAM-083" "HAM-084" "HAM-085" "HAM-087" "HAM-088" "HAM-089"
#[21] "HAM-090" "HAM-091" "HAM-093" "HAM-094" "HAM-095" "HAM-096" "HAM-098" "HAM-099"

#doesnt matter as we set new buffer zones and also are not taking the mean at each but using unique locations
#to assign enviro data to be unique to p/a locations 



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
  left_join(unique_locs_depth,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))

#save for now can delete later once we add in more variables 
#saveRDS(det_randomized, "c:/Users/TURNERN/Documents/For Github/RFspatial_rudd/01_data/02_processed_files/PA_rand_w_depth.rds")

library(ggplot2)

#plotting code to make sure depths look like they were generally correctly assigned 


locs_depth_sf <- st_as_sf(
  unique_locs_depth,
  coords = c("rand_long", "rand_lat"),
  crs = 4326
)

ggplot() +
  geom_sf(data = HH_plot,
          fill = "aliceblue",
          color = "steelblue") +
  geom_sf(data = locs_depth_sf,
          aes(colour = depth_m),
          size = 2) +
    geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.005 
          , linewidth = 0.3, linetype = "dashed") +
  scale_colour_viridis_c(
    option = "mako",
    direction = -1,
    name = "Depth (m)"
  ) +
  coord_sf() +
  theme_minimal()


ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.5 
          , linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = locs_nasf1,
          size = 1.8, alpha = 0.7, aes(color=depth_m)) +
   coord_sf(xlim = c(-79.94, -79.85),
           ylim = c(43.27, 43.30),
          expand = TRUE) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )


#SAV
# ── 1. Load the SAV raster ──────────────────────────────────────────────────
SAVnad83_raster <- rast("01_data/04_shapefiles/Enviro layers/SAV75_June2024_nad83.tif")

print(SAVnad83_raster)
plot(SAVnad83_raster)  # sanity check visual

fetch_aster <- rast("01_data/04_shapefiles/Enviro layers/fetchweightedm_2024.tiff")

print(SAVnad83_raster)
plot(SAVnad83_raster)  # sanity check visual


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
  left_join(unique_locs_depth,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))

#save for now can delete later once we add in more variables 
#saveRDS(det_randomized, "c:/Users/TURNERN/Documents/For Github/RFspatial_rudd/01_data/02_processed_files/PA_rand_w_depth.rds")

library(ggplot2)

#plotting code to make sure depths look like they were generally correctly assigned 


locs_depth_sf <- st_as_sf(
  unique_locs_depth,
  coords = c("rand_long", "rand_lat"),
  crs = 4326
)

ggplot() +
  geom_sf(data = HH_plot,
          fill = "aliceblue",
          color = "steelblue") +
  geom_sf(data = locs_depth_sf,
          aes(colour = depth_m),
          size = 2) +
    geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.005 
          , linewidth = 0.3, linetype = "dashed") +
  scale_colour_viridis_c(
    option = "mako",
    direction = -1,
    name = "Depth (m)"
  ) +
  coord_sf() +
  theme_minimal()


ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.5 
          , linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = locs_nasf1,
          size = 1.8, alpha = 0.7, aes(color=depth_m)) +
   coord_sf(xlim = c(-79.94, -79.85),
           ylim = c(43.27, 43.30),
          expand = TRUE) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )

