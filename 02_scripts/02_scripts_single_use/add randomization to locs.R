#adding randomization to presence absence data
#Hamilton Harbour polygon
#Presence absence dataframe
# 
#  
# load in this dataframe
#PA balanced filtered Rudd detections dataframe 
pa_data<-readRDS("c:/Users/TURNERN/Documents/For Github/RFspatial_rudd/01_data/02_processed_files/PA RFspatial Rudd.rds")
colnames(pa_data)
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(parallel)
library(dplyr)

library(sf)
library(dplyr)
library(ggplot2)



# ── 1. Pre-compute clipped buffers per unique receiver ────────────────────────
# ── 1. Load and project shapefile ─────────────────────────────────────────────
# Load and project water polygon
HH_gcmap <- st_read(
  "01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp",
  quiet = TRUE
) %>%
  st_transform(crs = 32617)

water_union <- st_union(HH_gcmap)

# Create UTM coordinates for detections
inputs_all <- pa_data %>%
  filter(
    !is.na(deploy_lat),
    !is.na(deploy_long),
  ) %>%
  st_as_sf(
    coords = c("deploy_long", "deploy_lat"),
    crs = 4326
  ) %>%
  st_transform(crs = 32617) %>%
  mutate(
    recv_x_m = st_coordinates(.)[, 1],
    recv_y_m = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Create unique receiver-buffer combinations
receiver_lookup <- inputs_all %>%
  distinct(
    station,
    year,
    thermal,
    recv_x_m,
    recv_y_m,
  ) %>%
  mutate(
    buffer_id = paste(
      station,
      year,
      thermal,
      round(recv_x_m, 1),
      round(recv_y_m, 1),
      sep = "_"
    )
  )


library(sf)
library(dplyr)
library(purrr)
library(ggplot2)

# ── 1. Load and project water polygon ─────────────────────────────────────────
HH_gcmap <- st_read(
  "01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp",
  quiet = TRUE
) %>%
  st_transform(crs = 32617)

# ── 2. Read edited buffer shapefiles ─────────────────────────────────────────
buffers_isocline <- st_read(
  "01_data/04_shapefiles/buffer/Isocline/IsoclineBuff.shp"
) %>%
  st_transform(crs = 32617) %>%
  mutate(thermal = "isocline")

buffers_thermocline <- st_read(
  "01_data/04_shapefiles/buffer/Thermocline/ThermoBuff.shp"
) %>%
  st_transform(crs = 32617) %>%
  mutate(thermal = "thermocline")

# ── 3. Combine and fix column names ──────────────────────────────────────────
buffers_all <- bind_rows(buffers_isocline, buffers_thermocline) %>%
  rename(
    stationnull = station,
    station     = MERGE_SRC
  )

# Check

colnames(buffers_all)
buffers_all %>% st_drop_geometry() %>% count(thermal)
#
# 75 in isocline and 73 in thermocline 
##################

# ── 4. Build lookup list keyed by station_thermal ─────────────────────────────
# Uses your edited polygons — irregular/clipped shapes preserved exactly
water_buffer_list_edited <- setNames(
  st_geometry(buffers_all),
  paste(buffers_all$station, buffers_all$thermal, sep = "_")
)

# ── 5. Build inputs_all from pa_data ─────────────────────────────────────────
# Done ONCE — keeps all columns including buffer_m, thermal, stationloc
inputs_all <- pa_data %>%
  filter(!is.na(deploy_lat), !is.na(deploy_long)) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  st_transform(crs = 32617) %>%
  mutate(
    recv_x_m        = st_coordinates(.)[, 1],
    recv_y_m        = st_coordinates(.)[, 2],
    station_thermal  = paste(station, thermal, sep = "_")
  ) %>%
  st_drop_geometry()

# Check buffer_m is present
inputs_all %>% count(stationloc, thermal, buffer_m)

# ── 6. Confirm no missing keys ────────────────────────────────────────────────
missing_keys <- setdiff(
  unique(inputs_all$station_thermal),
  names(water_buffer_list_edited)
)
missing_keys  # should be character(0)

# ── 7. Randomization function ─────────────────────────────────────────────────
randomize_detection_irregular <- function(recv_x, recv_y, max_radius, 
                                          station_id, water_buffers, sigma = NULL) {
  if (is.null(sigma)) sigma <- max_radius / 3
  max_attempts <- 1000
  attempt <- 0
  
  water_geom <- water_buffers[[station_id]]
  
  repeat {
    attempt <- attempt + 1
    if (attempt > max_attempts) {
      warning("Max attempts reached for ", station_id, " — returning receiver location")
      return(c(recv_x, recv_y))
    }
    
    dx <- rnorm(1, mean = 0, sd = sigma)
    dy <- rnorm(1, mean = 0, sd = sigma)
    
    pt <- st_sfc(st_point(c(recv_x + dx, recv_y + dy)), crs = 32617)
    if (!st_intersects(pt, water_geom, sparse = FALSE)[1, 1]) next
    
    return(c(recv_x + dx, recv_y + dy))
  }
}

# ── 8. Run randomization ──────────────────────────────────────────────────────
set.seed(42)

det_utm_rand <- inputs_all %>%
  mutate(
    rand_coords = pmap(
      list(recv_x_m, recv_y_m, buffer_m, station_thermal),
      ~randomize_detection_irregular(..1, ..2,
                                     max_radius    = ..3,
                                     station_id    = ..4,
                                     water_buffers = water_buffer_list_edited),
      .progress = TRUE
    ),
    rand_x = map_dbl(rand_coords, 1),
    rand_y = map_dbl(rand_coords, 2)
  ) %>%
  select(-rand_coords)

# ── 9. Convert to WGS84 and extract lat/lon ───────────────────────────────────
det_randomized <- det_utm_rand %>%
  st_as_sf(coords = c("rand_x", "rand_y"), crs = 32617) %>%
  st_transform(crs = 4326) %>%
  mutate(
    rand_long = st_coordinates(.)[, 1],
    rand_lat  = st_coordinates(.)[, 2]
  )

# ── 10. Plot using your edited buffer polygons ────────────────────────────────
# CHANGE from before: geom_sf(buffers_all) replaces circular st_buffer() rings
HH_plot      <- st_transform(HH_gcmap, crs = 4326)
buffers_plot <- st_transform(buffers_all, crs = 4326)   # edited polygons for plot

sample_tags <- det_randomized %>%
  pull(transmitter_id) %>%
  unique() %>%
  sample(min(5, length(.)))

det_plot <- det_randomized %>%
  filter(transmitter_id %in% sample_tags)

ggplot() +
  # Harbour polygon
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  # Edited buffer polygons — irregular/clipped shapes from ArcGIS
  # CHANGED: was st_buffer() circles, now your actual edited polygons
  geom_sf(data = buffers_plot, fill = "steelblue", alpha = 0.08,
          color = "steelblue", linewidth = 0.3, linetype = "dashed") +
  # Randomized detection points
  geom_sf(data = det_plot, aes(color = factor(transmitter_id)),
          size = 1.8, alpha = 0.7) +
  # Facet by thermal to see isocline vs thermocline buffers separately
  facet_wrap(~thermal) +
  scale_color_brewer(palette = "Set2", name = "Transmitter ID") +
  theme_minimal() +
  labs(
    title    = "Randomized detection locations — Hamilton Harbour",
    subtitle = "Dashed polygons = edited detection buffers  |  Points = randomized locations",
    x = "Longitude", y = "Latitude"
  )
#save as an rds file 

saveRDS(det_randomized, "PAdata_randomized.rds")
#################################################################################


###########above map could go in supp matierals as an example with two fish plotted

sample_recs <- c("HAM-043", "HAM-062", "HAM-091", "HAM-066")

# Detections only at those stations
det_plot <- det_rand_sf %>%
  filter(station %in% sample_recs)

# Receiver centres at those stations
recv_sf <- det_utm_rand %>%
  filter(station %in% sample_recs) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("recv_x_m", "recv_y_m"), crs = 32617) %>%
  st_transform(crs = 4326)

# Buffer rings at those stations
recv_buffers <- det_utm_rand %>%
  st_drop_geometry() %>%
  filter(station %in% sample_recs) %>%
  group_by(station) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf(coords = c("recv_x_m", "recv_y_m"), crs = 32617) %>%
  st_buffer(dist = .$buffer_m) %>%
  st_transform(crs = 4326)

ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = recv_buffers, fill = "steelblue", alpha = 0.08,
          color = "steelblue", linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = recv_sf, shape = 3, size = 2.5, color = "grey30", stroke = 0.8) +
  geom_sf(data = det_plot, aes(color = factor(transmitter_id)),
          size = 1.8, alpha = 0.7) +
  scale_color_brewer(palette = "Set2", name = "Transmitter ID") +
  facet_wrap(~thermal) +
  coord_sf(xlim = c(-79.93, -79.88),
           ylim = c(43.27, 43.30),
           expand = TRUE) +
  theme_minimal() +
  labs(
    title    = "Randomized detection locations — Hamilton Harbour",
    subtitle = "Crosses = receiver centres  |  Dashed rings = buffer radius  |  Points = randomized locations",
    x = "Longitude", y = "Latitude"
  )
