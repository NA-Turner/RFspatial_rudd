#adding randomization to presence absence data
#Hamilton Harbour polygon
#Presence absence dataframe
# 
#  
# load in this dataframe
#PA balanced filtered Rudd detections dataframe 
pa_data<-readRDS("01_data/02_processed_files/PA RFspatial Rudd.rds")
colnames(pa_data)
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

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



# ── 2. Read edited buffer shapefiles ─────────────────────────────────────────
buffers_isocline <- st_read(
  "01_data/04_shapefiles/buffer/Isocline/Isobuff.shp"
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

#just a plot with iso and thermo and rec locations in the middle with buffer radius to show 
#coverage in the harbour 

# Build receiver points sf object for plotting
# How many unique stations in buffers vs recv_plot
length(unique(buffers_all$station))
nrow(recv_plot)

# Any NAs in coordinates
inputs_all %>% 
  filter(is.na(recv_x_m) | is.na(recv_y_m)) %>% 
  count(station)


# Pull receiver centres from buffer centroids — guarantees one point per buffer
recv_plot <- buffers_all %>%
  group_by(station, thermal) %>%
  slice(1) %>%
  ungroup() %>%
  st_centroid() %>%
  st_transform(crs = 4326)

# Check count matches
nrow(recv_plot)
length(unique(buffers_all$station))

ggplot() +
  # Harbour polygon
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  # Edited buffer polygons
  geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.5 
          , linewidth = 0.3, linetype = "dashed") +
  # Receiver locations as black X's
 # geom_sf(data = recv_plot, shape = 4, size = 2, color = "black", stroke = 0.8) +
  # Facet by thermal
 # facet_wrap(~thermal) +
  theme_minimal() +
    #coord_sf(xlim = c(-79.94, -79.85),
     #      ylim = c(43.27, 43.30),
      #     expand = TRUE) +
  labs(
    title    = "Buffer coverage",
    x = "Longitude", y = "Latitude"
  )


#save as an rds file 


#saveRDS(det_randomized, "c:/Users/TURNERN/Documents/For Github/RFspatial_rudd/01_data/02_processed_files/PAdata_randomized.rds")
pa_data<-readRDS("01_data/02_processed_files/PAdata_randomized.rds")

#################################################################################

# fish as an example
pa_data$transmitter_id
rudd512<-filter(pa_data, transmitter_id=="512")
rudd512 <- st_as_sf(rudd512)
rudd512$presence<-as.factor(rudd512$presence)
class(rudd512)

#HH_plot

ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = buffers_plot, aes (fill = thermal),alpha = 0.5 
          , linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = rudd512,
          size = 1.8, alpha = 0.7, aes(color=presence)) +
  scale_color_brewer(palette = "Set4") +
  facet_wrap(~year) +
   coord_sf(xlim = c(-79.94, -79.85),
           ylim = c(43.27, 43.30),
          expand = TRUE) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )


unique(det_randomized$station)
unique(buffers_plot$station)
