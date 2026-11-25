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
HH_plot <- st_read(
  "01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp",
  quiet = TRUE
) %>%
  st_transform(crs = 32617)

water_union <- st_union(HH_plot)

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

# ── 3. add year to buffers from receiver look up file (for plotting later on)
buffers_all <- bind_rows(buffers_isocline, buffers_thermocline) %>%
  rename(
    stationnull = station,
    station     = MERGE_SRC
  )

buffers_all_year <- buffers_all %>%
  left_join(
    receiver_lookup %>% distinct(station, thermal, year),
    by = c("station", "thermal"),
    relationship = "many-to-many"
  )

##################

head(receiver_lookup)

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

##step 7 uniform random sampling within each of the buffers
set.seed(42)

det_utm_rand <- inputs_all %>%
  group_by(station_thermal) %>%
  group_modify(~{
    key  <- .y$station_thermal
    geom <- st_sfc(water_buffer_list_edited[[key]], crs = 32617)

    pts    <- st_sample(geom, size = nrow(.x), type = "random", exact = TRUE)
    coords <- st_coordinates(pts)

    .x %>% mutate(rand_x = coords[, 1], rand_y = coords[, 2])
  }) %>%
  ungroup()

# ── 8. Checks ─────────────────────────────────────────────────────────────────
# Same number of rows, no NAs
nrow(det_utm_rand) == nrow(inputs_all)
sum(is.na(det_utm_rand$rand_x))

# Every point sits inside its own buffer (should be TRUE)
det_check <- det_utm_rand %>%
  st_as_sf(coords = c("rand_x", "rand_y"), crs = 32617, remove = FALSE)

inside <- map2_lgl(
  seq_len(nrow(det_check)),
  det_check$station_thermal,
  ~ st_within(det_check[.x, ],
              st_sfc(water_buffer_list_edited[[.y]], crs = 32617),
              sparse = FALSE)[1, 1]
)


any(duplicated(paste(det_utm_rand$rand_x, det_utm_rand$rand_y)))
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
HH_plot      <- st_transform(HH_plot, crs = 4326)
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
#saveRDS(det_randomized, "01_data/02_processed_files/PAdata_randomized.rds")
#now go to adding predictors to DF script

#pa_data<-readRDS("01_data/02_processed_files/PAdata_randomized.rds")
##################################################
# fish as an example
unique(pa_data$transmitter_id)
pa_data$transmitter_id
rudd501<-filter(pa_data, transmitter_id=="501")
rudd501 <- st_as_sf(rudd501)
rudd501$presence<-as.factor(rudd501$presence)
class(rudd501)

#HH_plot
buffers_all_year_plot <- st_transform(buffers_all_year, 4326) %>%
  filter(!is.na(year))

years2425<-buffers_all_year_plot %>% filter(year %in% c(2024, 2025))

ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = years2425, aes(fill = thermal), alpha = 0.3,
          linewidth = 0.3, linetype = "dashed") +
  geom_sf(data = rudd501, aes(color = presence), size = 1.8, alpha = 0.8) +
  scale_color_manual(
    values = c("0" = "red", "1" = "darkgreen"),
    labels = c("0" = "Absence", "1" = "Presence"),
    name   = "Detection"
  ) +
  scale_fill_manual(
    values = c("isocline" = "#E69F00", "thermocline" = "#56B4E9"),
    name   = "Thermal period"
  ) +
  facet_wrap(~ year + thermal) +
  coord_sf(xlim = c(-79.95, -79.75),
           ylim = c(43.26, 43.35),
           expand = TRUE) +
  theme_minimal(base_size=20) +
  labs(x = "Longitude", y = "Latitude")

unique(det_randomized$station)
unique(buffers_plot$station)

  
#geom_sf change to receiver location 
#per year 
#add ham recs rudd
##needs year column added based on 

recs<-read.csv("01_data/02_processed_files/Hamrecs_rudd2023 to 2025_plottingonly.csv")
recs$deploy_date_time<-as.POSIXct(recs$deploy_date_time)
recs$year <- as.numeric(format(recs$deploy_date_time, "%Y"))
recs<-recs %>% filter(!(station_no %in% c("52", "55", "56", "54", "57"
                , "97", "92", "86", "8", "14", "22", "6", "19", "17")))

#################################
#added 6, 19, 17 need to check if htese are included in PA dataframe 


# 1. Station-year lookup: every year each receiver was in the water
recs$deploy_yr  <- as.numeric(format(as.Date(recs$deploy_date_time), "%Y"))
recs$recover_yr <- as.numeric(substr(recs$recover_date_time, 1, 4))
recs$recover_yr[is.na(recs$recover_yr)] <- recs$deploy_yr[is.na(recs$recover_yr)]  # no recovery date -> deploy year only

station_year <- unique(do.call(rbind, lapply(seq_len(nrow(recs)), function(i) {
  data.frame(station = recs$station[i],
             year    = seq(recs$deploy_yr[i], recs$recover_yr[i]))
})))

# 2. Buffers: keep only station-years where the receiver was deployed
buffers_year <- inner_join(buffers_plot, station_year, by = "station")

# 3. Receiver points with the same station-years
station_locs <- recs %>%
  distinct(station, .keep_all = TRUE) %>%
  select(station, deploy_lat, deploy_long)

recs_year <- inner_join(station_year, station_locs, by = "station")

# 4. Plot
HH_plot +
  geom_sf(data = buffers_year, fill = "steelblue", alpha = 0.08,
          color = 'thermal', linewidth = 0.3, linetype = "dashed",
          inherit.aes = FALSE) +
  geom_point(data = recs_year, aes(x = deploy_long, y = deploy_lat),
             size = 1, alpha = 0.7, color="darkred") +
  coord_sf(default_crs = 4326) +
  facet_wrap(~year) +
  theme_minimal()

HH_plot +
  geom_sf(data = buffers_year,
          aes(color = thermal),
          fill = "steelblue", alpha = 0.2,
          linewidth = 0.3, linetype = "dashed",
          inherit.aes = FALSE) +
  geom_point(data = recs_year, aes(x = deploy_long, y = deploy_lat),
             size = 1.5, alpha = 0.7, color = "black") +
  scale_color_manual(
    values = c(
               "isocline"    = "#E69F00",
               "thermocline"   = "#009E73"),
    name = "Thermal period"
  ) +
  coord_sf(default_crs = 4326) +
  facet_wrap(~ year) +
  theme_minimal()+
   theme(
    axis.title        = element_text(size = 16),
    axis.text         = element_text(size = 12),
    strip.text        = element_text(size = 14, face = "bold"),  # facet labels (years)
    legend.title      = element_text(size = 14),
    legend.text       = element_text(size = 12),
    legend.key.size   = unit(1, "cm")                            # bigger legend keys
  )
