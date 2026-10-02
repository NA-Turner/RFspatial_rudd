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


#lake ontario water levels for 2024 and 2025 were right in line with the historical average 
#~75msl for the summer period 


#####
## SL removed rec 43 from analysis do we also want to do the same ?!
### remove receiver 43 due to inaccurate substrate info and is a travel corridor, not habitat for fish.
#hab_daily <- hab_daily %>% filter(!station=="HAM-043")
#keep


#extract exact depth points of the P/A data
# ── 1. Load the depth raster ──────────────────────────────────────────────────
depth_raster <- rast("01_data/04_shapefiles/Enviro layers/WL.tif")
print(depth_raster)
plot(depth_raster)  

#01_data\02_processed_files\PAdata_randomized.rds

unique_locs <- det_randomized |>
  distinct(transmitter_id, date, station, year, rand_long, rand_lat)

# ── 3. Convert to sf points — 
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

###MISSING AFTER COMP CRASH



# ── 1. Load the SAV raster ──────────────────────────────────────────────────
SAV2026_raster <- rast("01_data/04_shapefiles/Enviro layers/SAVM2026/SAVM_202608.tif")

print(SAV2026_raster)
plot(SAV2026_raster)  # sanity check visual

fetch_raster <- rast("01_data/04_shapefiles/Enviro layers/fetchweightedm_2024.tif")

print(SAVnad83_raster)
plot(SAVnad83_raster)  # sanity check visual


# ── 2. Get unique lat/lon locations ──────────────────────────────────────────
# Assuming your data has columns: station, deploy_lat, deploy_long
# (adjust column names to match your actual dataframe)
#01_data\02_processed_files\PAdata_randomized.rds
#going to add SAV to depth one
PA_rand_w_depth <- readRDS("01_data/02_processed_files/PA_rand_w_depth.rds")
unique_locs <- PA_rand_w_depth |>
  distinct(transmitter_id, date, station, year, rand_long, rand_lat)

# ── 3. Convert to sf points — use WGS84 (EPSG:4326) since coords are lat/lon ─
locs_sf <- st_as_sf(unique_locs,
                    coords = c("rand_long", "rand_lat"),
                    crs    = 4326)

# ── 4. Reproject points to match the raster CRS ──────────────────────────────
# terra and sf need matching CRS for extraction to work correctly
raster_crs <- crs(SAV2026_raster)
locs_projected <- st_transform(locs_sf, crs = raster_crs)

# Convert to SpatVector for terra::extract()
locs_vect <- vect(locs_projected)
# ── 5. Extract depth values ───────────────────────────────────────────────────
SAV_vals <- terra::extract(SAV2026_raster, locs_vect)
# Returns a dataframe with columns: ID (row index) + raster layer name(s)

# ── 6. Join by ID (row index) — safe regardless of ordering ──────────────────
# terra's ID column corresponds to the row number of locs_vect
unique_locs_sav <- unique_locs |>
  mutate(ID = row_number()) |>                        # create matching ID
  left_join(SAV_vals, by = "ID") |>                 # join on position
  rename(SAV = names(SAV2026_raster)[1]) |>         # rename by layer name
  select(-ID)


# ── Split into complete and NA sets ──────────────────────────────────────────
locs_complete <- unique_locs_sav |> filter(!is.na(SAV))
locs_na       <- unique_locs_sav |> filter(is.na(SAV))

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
  mutate(SAV = locs_complete$SAV[nearest_idx])

# ── Recombine ─────────────────────────────────────────────────────────────────
unique_locs_sav1 <- bind_rows(locs_complete, locs_na)

# ── Join back to pa_data ──────────────────────────────────────────────────────
det_randomized <- PA_rand_w_depth |>
  left_join(unique_locs_sav1,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))

#save for now can delete later once we add in more variables 
#saveRDS(det_randomized, "c:/Users/TURNERN/Documents/For Github/RFspatial_rudd/01_data/02_processed_files/PA_rand_w_depthSAV.rds")

library(ggplot2)

#plotting code to make sure depths look like they were generally correctly assigned 


locs_sav_sf <- st_as_sf(
  unique_locs_sav1,
  coords = c("rand_long", "rand_lat"),
  crs = 4326
)


###see script 
#add randomization to locs.R for HH plotting script


ggplot() +
  geom_sf(data = HH_plot,
          fill = "aliceblue",
          color = "steelblue") +
  geom_sf(data = locs_sav_sf,
          aes(colour = SAV),
          size = 2) +
#    geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.005 
#          , linewidth = 0.3, linetype = "dashed") +
  scale_colour_viridis_c(
    option = "mako",
    direction = -1,
    name = "SAV (%)"
  ) +
  coord_sf() +
  theme_minimal()


ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
 # geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.5 
 #         , linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = locs_nasf1,
          size = 1.8, alpha = 0.7, aes(color=depth_m)) +
   coord_sf(xlim = c(-79.94, -79.85),
           ylim = c(43.27, 43.30),
          expand = TRUE) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )

###addin fetch to the PA dataframe

PA_rand_w_depthSAV<-readRDS("01_data/02_processed_files/PA_rand_w_depthSAV.rds")

unique_locs <- PA_rand_w_depthSAV |>
  distinct(transmitter_id, date, station, year, rand_long, rand_lat)

# ── 3. Convert to sf points — use WGS84 (EPSG:4326) since coords are lat/lon ─
locs_sf <- st_as_sf(unique_locs,
                    coords = c("rand_long", "rand_lat"),
                    crs    = 4326)

# ── 4. Reproject points to match the raster CRS ──────────────────────────────
# terra and sf need matching CRS for extraction to work correctly
fetch2026_raster <- rast("01_data/04_shapefiles/Enviro layers/SAVM2026/fetch2026.tif")
plot(fetch2026_raster)
raster_crs <- crs(fetch2026_raster)
locs_projected <- st_transform(locs_sf, crs = raster_crs)

# Convert to SpatVector for terra::extract()
locs_vect <- vect(locs_projected)
# ── 5. Extract depth values ───────────────────────────────────────────────────
fetch_vals <- terra::extract(fetch2026_raster, locs_vect)
# Returns a dataframe with columns: ID (row index) + raster layer name(s)

# ── 6. Join by ID (row index) — safe regardless of ordering ──────────────────
# terra's ID column corresponds to the row number of locs_vect
unique_locs_fetch <- unique_locs |>
  mutate(ID = row_number()) |>
  left_join(fetch_vals, by = "ID") |>
  rename(fetch = names(fetch2026_raster)[1]) |>
  dplyr::select(-ID)

# ── Split into complete and NA sets ──────────────────────────────────────────
locs_complete <- unique_locs_fetch |> filter(!is.na(fetch))
locs_na       <- unique_locs_fetch |> filter(is.na(fetch))

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
  mutate(fetch = locs_complete$fetch[nearest_idx])

# ── Recombine ─────────────────────────────────────────────────────────────────
unique_locs_fetch1 <- bind_rows(locs_complete, locs_na)

# ── Join back to pa_data ──────────────────────────────────────────────────────
det_randomized <- PA_rand_w_depthSAV |>
  left_join(unique_locs_fetch1,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))

#save for now can delete later once we add in more variables 
#saveRDS(det_randomized, "c:/Users/TURNERN/Documents/For Github/RFspatial_rudd/01_data/02_processed_files/PA_rand_w_depthSAVfetch.rds")

library(ggplot2)

