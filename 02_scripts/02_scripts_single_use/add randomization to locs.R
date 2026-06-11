#adding randomization to presence absence data
#Hamilton Harbour polygon
#Presence absence dataframe 
#(pa_data, "PA RFspatial Rudd.rds")
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



###########
library(purrr)
library(sf)
library(dplyr)






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
    !is.na(buffer_m)
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
    buffer_m
  ) %>%
  mutate(
    buffer_id = paste(
      station,
      year,
      thermal,
      round(recv_x_m, 1),
      round(recv_y_m, 1),
      buffer_m,
      sep = "_"
    )
  )

# Build clipped water buffers
receiver_water_buffers <- receiver_lookup %>%
  st_as_sf(
    coords = c("recv_x_m", "recv_y_m"),
    crs = 32617
  ) %>%
  mutate(
    buffer_geom = st_buffer(geometry, dist = buffer_m)
  )

receiver_water_buffers$water_buffer <- st_intersection(
  receiver_water_buffers$buffer_geom,
  water_union
)

# Create lookup list of water buffers
water_buffer_list <- setNames(
  receiver_water_buffers$water_buffer,
  receiver_water_buffers$buffer_id
)

# Add buffer IDs back to detections
inputs_all <- inputs_all %>%
  mutate(
    buffer_id = paste(
      station,
      year,
      thermal,
      round(recv_x_m, 1),
      round(recv_y_m, 1),
      buffer_m,
      sep = "_"
    )
  )

# Function to randomize detection locations
randomize_detection <- function(recv_x,
                                recv_y,
                                buffer_radius,
                                buffer_id,
                                water_buffers,
                                sigma = NULL,
                                max_attempts = 1000) {

  if (is.null(sigma)) {
    sigma <- buffer_radius / 3
  }

  water_geom <- water_buffers[[buffer_id]]

  if (is.null(water_geom) || st_is_empty(water_geom)) {
    warning("Missing or empty water buffer for: ", buffer_id)
    return(c(recv_x, recv_y))
  }

  for (attempt in seq_len(max_attempts)) {

    dx <- rnorm(1, mean = 0, sd = sigma)
    dy <- rnorm(1, mean = 0, sd = sigma)

    # Skip points outside detection radius
    if (sqrt(dx^2 + dy^2) > buffer_radius) {
      next
    }

    candidate <- st_sfc(
      st_point(c(recv_x + dx, recv_y + dy)),
      crs = 32617
    )

    # Return point if it falls within water
    if (st_intersects(candidate, water_geom, sparse = FALSE)[1, 1]) {
      return(c(recv_x + dx, recv_y + dy))
    }
  }

  warning(
    "Max attempts reached for ",
    buffer_id,
    ". Returning receiver location."
  )

  return(c(recv_x, recv_y))
}

# Randomize detections
set.seed(42)

det_utm_rand <- inputs_all %>%
  mutate(
    rand_coords = pmap(
      list(
        recv_x_m,
        recv_y_m,
        buffer_m,
        buffer_id
      ),
      ~ randomize_detection(
        recv_x = ..1,
        recv_y = ..2,
        buffer_radius = ..3,
        buffer_id = ..4,
        water_buffers = water_buffer_list
      )
    ),
    rand_x = map_dbl(rand_coords, 1),
    rand_y = map_dbl(rand_coords, 2)
  ) %>%
  select(-rand_coords)

# Convert randomized coordinates to sf object
det_utm_rand_sf <- det_utm_rand %>%
  st_as_sf(
    coords = c("rand_x", "rand_y"),
    crs = 32617
  )

# Convert back to latitude/longitude
det_randomized <- det_utm_rand_sf %>%
  st_transform(crs = 4326)

coords_ll <- st_coordinates(det_randomized)

det_randomized <- det_randomized %>%
  mutate(
    rand_long = coords_ll[, 1],
    rand_lat = coords_ll[, 2]
  )

#save as an rds file 

saveRDS(det_randomized, "PAdata_randomized.rds")
#################################################################################
# ── Plot randomized detections on Hamilton Harbour map ────────────────────────

# Reproject HH_gcmap back to 4326 for plotting only
HH_plot <- st_transform(HH_gcmap, crs = 4326)

# Rebuild sf from randomized UTM coords and reproject to 4326
det_rand_sf <- det_utm_rand %>%
  st_as_sf(coords = c("rand_x", "rand_y"), crs = 32617) %>%
  st_transform(crs = 4326)

# Pull a sample of transmitters to keep the plot readable
colnames(det_randomized)

sample_tags <- det_randomized %>%
  pull(transmitter_id) %>%
  unique() %>%
  sample(min(5, length(.)))   # plot up to 5 fish; adjust as needed

det_plot <- det_rand_sf %>%
  filter(transmitter_id %in% sample_tags)

# Original (non-randomized) receiver locations for the same rows
recv_sf <- det_utm_rand %>%
  filter(transmitter_id %in% sample_tags) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("recv_x_m", "recv_y_m"), crs = 32617) %>%
  st_transform(crs = 4326)

colnames(det_utm_rand)

# Buffer rings around each unique receiver (for visual reference)
recv_buffers <- det_utm_rand %>%
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
  facet_wrap(~thermal)+
  theme_minimal() +
  labs(
    title    = "Randomized detection locations — Hamilton Harbour",
    subtitle = "Crosses = receiver centres  |  Dashed rings = buffer radius  |  Points = randomized locations",
    x = "Longitude", y = "Latitude"
  )


###########above map could go in supp matierals as an example with two fish plotted
