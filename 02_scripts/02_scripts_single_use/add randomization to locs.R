#adding randomization to presence absence data
#Hamilton Harbour polygon
#Presence absence dataframe 
#saveRDS(pa_data, "PA RFspatial Rudd.rds")
colnames(pa_data)
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)

# ── 1. Load and project shapefile ─────────────────────────────────────────────
HH_gcmap <- st_read("01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp")
HH_gcmap <- st_transform(HH_gcmap, crs = 32617)   # project to UTM for metre-based operations

#saveRDS(pa_data, file = "Rudd_5active_PAdata.rds")


ggplot(data = HH_gcmap) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_minimal()

# ── 2. Randomization function ─────────────────────────────────────────────────
# HH_gcmap here is your WATER polygon — st_intersects checks if point is IN water
# If your shapefile is a LAND polygon, flip the logic (remove the !)
randomize_detection <- function(recv_x, recv_y, buffer_radius, water_sf, sigma = NULL) {
  if (is.null(sigma)) sigma <- buffer_radius / 3
  max_attempts <- 1000
  attempt <- 0
  
  repeat {
    attempt <- attempt + 1
    if (attempt > max_attempts) {
      warning("Max attempts reached for receiver at (", recv_x, ",", recv_y, ") — returning receiver location")
      return(c(recv_x, recv_y))
    }
    
    dx <- rnorm(1, mean = 0, sd = sigma)
    dy <- rnorm(1, mean = 0, sd = sigma)
    
    # Check 1: within buffer radius
    if (sqrt(dx^2 + dy^2) > buffer_radius) next
    
    # Check 2: point must be within the water polygon
    pt <- st_point(c(recv_x + dx, recv_y + dy)) %>%
          st_sfc(crs = 32617)
    if (!st_intersects(pt, water_sf, sparse = FALSE)[1, 1]) next
    
    return(c(recv_x + dx, recv_y + dy))
  }
}

# ── 3. Project pa_data receiver coords to UTM ─────────────────────────────────
det_sf <- pa_data %>%
  filter(presence == 1) %>%                         # randomize presences only
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326)

det_utm <- st_transform(det_sf, crs = 32617)

coords_utm <- st_coordinates(det_utm)
det_utm$recv_x_m <- coords_utm[, 1]
det_utm$recv_y_m <- coords_utm[, 2]

# ── 4. Apply randomization ────────────────────────────────────────────────────
set.seed(42)

det_utm_rand <- det_utm %>%
  st_drop_geometry() %>%
  mutate(
    rand_coords = pmap(
      list(recv_x_m, recv_y_m, buffer_m),
      ~randomize_detection(..1, ..2, buffer_radius = ..3, water_sf = HH_gcmap)
    ),
    rand_x = map_dbl(rand_coords, 1),
    rand_y = map_dbl(rand_coords, 2)
  ) %>%
  select(-rand_coords)


# ── 5. Rebuild sf and reproject back to WGS84 ────────────────────────────────
det_rand_sf <- det_utm_rand %>%
  st_as_sf(coords = c("rand_x", "rand_y"), crs = 32617) %>%
  st_transform(crs = 4326)

#################################################################################
# ── Plot randomized detections on Hamilton Harbour map ────────────────────────

# Reproject HH_gcmap back to 4326 for plotting only
HH_plot <- st_transform(HH_gcmap, crs = 4326)

# Rebuild sf from randomized UTM coords and reproject to 4326
det_rand_sf <- det_utm_rand %>%
  st_as_sf(coords = c("rand_x", "rand_y"), crs = 32617) %>%
  st_transform(crs = 4326)

# Pull a sample of transmitters to keep the plot readable
sample_tags <- det_rand_sf %>%
  pull(transmitter_id) %>%
  unique() %>%
  sample(min(5, length(.)))   # plot up to 5 fish; adjust as needed

det_plot <- det_rand_sf %>%
  filter(transmitter_id %in% sample_tags)

# Original (non-randomized) receiver locations for the same rows
recv_sf <- det_utm %>%
  filter(transmitter_id %in% sample_tags) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("recv_x_m", "recv_y_m"), crs = 32617) %>%
  st_transform(crs = 4326)

# Buffer rings around each unique receiver (for visual reference)
recv_buffers <- det_utm %>%
  st_drop_geometry() %>%
  filter(transmitter_id %in% sample_tags) %>%
  group_by(station) %>%
  slice(1) %>%
  ungroup() %>%
  st_as_sf(coords = c("recv_x_m", "recv_y_m"), crs = 32617) %>%
  st_buffer(dist = .$buffer_m) %>%
  st_transform(crs = 4326)

ggplot() +
  # Harbour polygon
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  # Buffer rings
  geom_sf(data = recv_buffers, fill = "steelblue", alpha = 0.08,
          color = "steelblue", linewidth = 0.3, linetype = "dashed") +
  # Receiver centres
  geom_sf(data = recv_sf, shape = 3, size = 2.5, color = "grey30", stroke = 0.8) +
  # Randomized detections
  geom_sf(data = det_plot, aes(color = factor(transmitter_id)),
          size = 1.8, alpha = 0.7) +
  scale_color_brewer(palette = "Set2", name = "Transmitter ID") +
  theme_minimal() +
  labs(
    title    = "Randomized detection locations — Hamilton Harbour",
    subtitle = "Crosses = receiver centres  |  Dashed rings = buffer radius  |  Points = randomized locations",
    x = "Longitude", y = "Latitude"
  )