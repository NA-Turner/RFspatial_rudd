#datafile wtih balance presence absence
#normally distributed random PAs with unique locations
#buffer ranges by iso/thermo accounted for
#5 active tags on the array one any given time





################
##load in R packages
##########
library(terra)
library(sf)
library(dplyr)

##########
#hamilton harbour polgyon file
shorelinemap <- read_sf("01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp")
shorelinemap <- st_transform(shorelinemap, crs = 4326)


HH_plot<-ggplot(shorelinemap) +
 geom_sf(fill = "lightblue", color = "black", size = 0.5) +
 theme_minimal() +
 theme(axis.text = element_text(size = 8))
#use glatos rpackage to summarize detections by num_fish and num_dets

HH_plot


########################################
######################
#larocque et al 2024 paper
#when missing temperature data used 
#mean julian day of seasonal deliniation based on some older center station temp data and based on thermcline/isocline also
#seasons were listed as follows:
#spring: April 25 to June 6
#summer: June 7 to October 3; 
# fall: October 4 to November 17,  
# winter: November 18 to April 24

## based on Wells et al. 2021 paper 
#isocline was October 6th 
#stratified from DOY 160 is around June 9th 

##assign isocline/thermocline to PA dataframe 

pa_data<-readRDS("01_data/02_processed_files/PAdata_randomized.rds")

pa_data$thermal <- ifelse(
  format(pa_data$date, "%m-%d") >= "10-06" | format(pa_data$date, "%m-%d") <= "06-06",
  "isocline",
  "thermocline"
)

unique(pa_data$station)
#assign nearshore or offshore for receiver location 
pa_data$stationloc <- ifelse(
  pa_data$station %in% c("HAM-004", "HAM-041", "HAM-021", "HAM-017", "HAM-039"),
  "offshore",
  ifelse(
    pa_data$station %in% c( "HAM-032", "HAM-079", "HAM-095", "HAM-099", "HAM-098", "HAM-030",
  "HAM-078", "HAM-044", "HAM-077", "HAM-094", "HAM-037", "HAM-063",
  "HAM-034", "HAM-043", "HAM-093", "HAM-003", "HAM-070", "HAM-062",
  "HAM-005", "HAM-042", "HAM-061", "HAM-089", "HAM-071", "HAM-047",
  "HAM-072", "HAM-027", "HAM-036", "HAM-090", "HAM-013", "HAM-051",
  "HAM-076", "HAM-046", "HAM-088", "HAM-059", "HAM-007", "HAM-002",
  "HAM-035", "HAM-080", "HAM-015", "HAM-028", "HAM-085", "HAM-068",
  "HAM-023", "HAM-083", "HAM-053", "HAM-087", "HAM-067", "HAM-073",
  "HAM-082", "HAM-058", "HAM-029", "HAM-025", "HAM-074", "HAM-081",
  "HAM-096", "HAM-084", "HAM-066", "HAM-091", "HAM-011", "HAM-033",
  "HAM-075", "HAM-045", "HAM-052", "HAM-057", "HAM-048", "HAM-060",
   "HAM-055", "HAM-012", "HAM-018", "HAM-009", "HAM-001"),
    "nearshore",
    NA
  )
)

unique(pa_data$station)
#will have to plot thermo/iso on map to validate that all points were assigned and were assigned correctly 


ggplot(data = HH_plot) +
  geom_sf(fill = "lightblue", color = "white") +
  geom_point(data = pa_data, 
             aes(x = deploy_long, y = deploy_lat, color = stationloc)) +
  facet_wrap(~year) +
  labs(x = "Longitude", y = "Latitude", color = "Deployment Year") +  # legend title set here
  theme_minimal() +
  theme(axis.text.y   = element_text(size = 12),
        axis.text.x   = element_text(size = 12),
        legend.title  = element_text(size = 14),  # controls font size of legend title
        legend.text   = element_text(size = 12))  # controls font size of legend items

##then assign the buffer range based on nearshore/offshore and thermal layer

library(dplyr)
pa_data <- pa_data %>%
  mutate(buffer_m = case_when(
    stationloc == "offshore"  & thermal == "isocline"    ~ 429,
    stationloc == "offshore"  & thermal == "thermocline" ~ 275, #good 
    stationloc == "nearshore" & thermal == "isocline"    ~ 413,
    stationloc == "nearshore" & thermal == "thermocline" ~ 300,
    TRUE ~ NA_real_
  ))

unique(pa_data$buffer_m)

#then from here we can add the normally distrubuted randomization to points
#using the specific buffer zones around recs at different times of the year
#and the lake polygon to ensure there are not locations randomly placed on impassable features such as land
#look at brownscombe code for this aswell to help in coding 

#add seasonal time binning 
pa_data<-readRDS("01_data/02_processed_files/PAdata_randomized.rds")


pa_data <- pa_data %>%
  mutate(
    month_num = as.numeric(format(date, "%m")),
    day_num = as.numeric(format(date, "%d")),
    season = case_when(
      (month_num == 4 & day_num >= 1) | month_num == 5 | (month_num == 6 & day_num <= 15) ~ "Spring",
      (month_num == 6 & day_num >= 16) | month_num %in% c(7, 8, 9) | (month_num == 10 & day_num <= 3) ~ "Summer",
      (month_num == 10 & day_num >= 4) | (month_num == 11 & day_num <= 17) ~ "Fall",
      (month_num == 11 & day_num >= 18) | month_num %in% c(12, 1, 2, 3) ~ "Winter",
      TRUE ~ NA_character_
    )
  )

#saveRDS(pa_data,file = "01_data/02_processed_files/PA RFspatial Rudd.rds")

# ── Unique receivers per station x thermal x stationloc for ArcGIS export ────

#colnames(pa_data)
#receivers_for_arcgis <- pa_data %>%
#  filter(!is.na(deploy_lat), !is.na(deploy_long)) %>%
#  group_by(station, stationloc) %>%
#  summarise(
#    deploy_lat  = mean(deploy_lat,  na.rm = TRUE),
#    deploy_long = mean(deploy_long, na.rm = TRUE),
#    .groups = "drop"
#  )
#receivers_for_arcgis_sf <- receivers_for_arcgis %>%
#  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
#  st_transform(crs = 32617)
#st_write(receivers_for_arcgis_sf,
#         "01_data/04_shapefiles/receivers_for_arcgis.shp",
#         delete_if_exists = TRUE)


#####
#load in raster data to extract unique values for each unqiue p/a location 

#extract depth points 
# Load the depth raster ──────────────────────────────────────────────────
depth_raster<-rast("01_data/02_processed_files/raster layers/depth/WL.tif")
print(depth_raster)
plot(depth_raster)  


unique_locs <- pa_data |>
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
  dplyr::select(-ID)


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
pa_data <- pa_data |>
  left_join(unique_locs_depth,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))

library(ggplot2)

HH_plot <- st_read(
  "01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp",
  quiet = TRUE
) %>% st_transform(4326)

locs_depth_sf <- st_as_sf(
  unique_locs_depth,
  coords = c("rand_long", "rand_lat"),
  crs = 4326
)

ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = locs_depth_sf,
          size = 1.8, alpha = 0.7, aes(color=depth_m)) +
   coord_sf(xlim = c(-79.94, -79.85),
           ylim = c(43.27, 43.30),
          expand = TRUE) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )


#SAV
#secchi included in SAV layer 
# ── 1. Load the SAV raster ──────────────────────────────────────────────────
#raster layer includes post-hoc secchi, masking polygon layer file but NO RBG edits to CPM and GC areas

SAV2026rbgedits_raster <- rast("01_data/02_processed_files/raster layers/sav_rbgedits1")

print(SAV2026rbgedits_raster)
plot(SAV2026rbgedits_raster)  

#load the fetch raster layer 
#fetch raster layer used in larocque et al. 
#fetch_raster <- rast("01_data/04_shapefiles/Enviro layers/fetchweightedm_2024.tif")



# ── 4. Reproject points to match the raster CRS ──────────────────────────────
# terra and sf need matching CRS for extraction to work correctly
raster_crs <- crs(SAV2026rbgedits_raster)
locs_projected <- st_transform(locs_sf, crs = raster_crs)

# Convert to SpatVector for terra::extract()
locs_vect <- vect(locs_projected)
# ── 5. Extract depth values ───────────────────────────────────────────────────
SAV_vals <- terra::extract(SAV2026rbgedits_raster, locs_vect)
# Returns a dataframe with columns: ID (row index) + raster layer name(s)

# ── 6. Join by ID (row index) — safe regardless of ordering ──────────────────
# terra's ID column corresponds to the row number of locs_vect
unique_locs_sav <- unique_locs |>
  mutate(ID = row_number()) |>                        # create matching ID
  left_join(SAV_vals, by = "ID") |>                 # join on position
  rename(SAV = names(SAV2026rbgedits_raster)[1]) |>         # rename by layer name
  dplyr::select(-ID)


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

# ── Borrow sav from the nearest complete point ──────────────────────────────
locs_na <- locs_na |>
  mutate(SAV = locs_complete$SAV[nearest_idx])

# ── Recombine ─────────────────────────────────────────────────────────────────
unique_locs_sav1 <- bind_rows(locs_complete, locs_na)

# ── Join back to pa_data ──────────────────────────────────────────────────────
pa_data <- pa_data |>
  left_join(unique_locs_sav1,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))

locs_sav_sf <- st_as_sf(
  unique_locs_sav1,
  coords = c("rand_long", "rand_lat"),
  crs = 4326
)

#check with a plot 
ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = locs_sav_sf,
          size = 1.8, alpha = 0.7, aes(color=SAV)) +
   coord_sf(xlim = c(-79.94, -79.85),
           ylim = c(43.27, 43.30),
          expand = TRUE) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )

###add in fetch to the PA dataframe
fetch2026_raster <- rast("01_data/02_processed_files/raster layers/fetch/fetchnew.tif")

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

# ── Borrow fetch from the nearest complete point ──────────────────────────────
locs_na <- locs_na |>
  mutate(fetch = locs_complete$fetch[nearest_idx])

# ── Recombine ─────────────────────────────────────────────────────────────────
unique_locs_fetch1 <- bind_rows(locs_complete, locs_na)

# ── Join back to pa_data ──────────────────────────────────────────────────────
pa_data <- pa_data |>
  left_join(unique_locs_fetch1,
            by = c("transmitter_id", "date","year", "station", "rand_lat", "rand_long"))


locs_fetch_sf <- st_as_sf(
  unique_locs_fetch1,
  coords = c("rand_long", "rand_lat"),
  crs = 4326
)

ggplot() +
  geom_sf(data = HH_plot, fill = "aliceblue", color = "steelblue", linewidth = 0.4) +
  geom_sf(data = locs_fetch_sf,
          size = 1.8, alpha = 0.7, aes(color=fetch)) +
   coord_sf(xlim = c(-79.94, -79.85),
           ylim = c(43.27, 43.30),
          expand = TRUE) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )

# ===============================
# 1. Distance to Shoreline
# ===============================
######################
library(sf)
library(dplyr)


# Transform both datasets to UTM Zone 17N (metres)
locations_utm <- st_transform(locs_sf, 32617)
shoreline_utm <- st_transform(shorelinemap, 32617)

# Dissolve polygons to remove boundaries between adjacent polygons
shoreline_union <- st_union(shoreline_utm)

# Extract the boundary of the combined polygon geometry
shoreline_boundary <- st_boundary(shoreline_union)

# Calculate distance to the nearest polygon boundary
pa_data$shoreline_distance_m <- as.numeric(
  st_distance(locations_utm, shoreline_boundary)
)

#check to make sure the distance to shoreline worked
ggplot() + HH_plot+
  geom_sf(data = pa_data,
          size = 1, alpha = 0.7, aes(color=shoreline_distance_m)) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude"
  )
##add zone variable 

#zones were added to a 50m grid in arcgis
#see supp figureXX.
#zones were based off of receiver zone assigned in Turner et al. 2026
#zones generally followed receiver location 
#and when possible did not cut off buffer zone areas i.e. along the north and east end areas
#some buffers overlap zones in the west end/carrolls bay/cootes paradise area (two fishway receivers)

#assign each PA randmoized location a unique zone value based on random location 

#PA_rand_w_depthSAVfetch<-readRDS("01_data/02_processed_files/PArand_complete.rds")

#dataframe now has:
#thermal, year, month, day, season, depth, SAV, fetch, distance to shoreline, zone area

#run a correlation matrix

library(dplyr)
library(corrplot)

# 1. Select only the numeric explanatory variables (drop the response, IDs, etc.)
predictors <- pa_data %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-c(presence))   # <- replace with your response / non-predictor columns

predictors <- pa_data %>%
  st_drop_geometry() %>%
  dplyr::select(depth_m, SAV, fetch, shoreline_distance_m)   # add month_num / day_num if they're candidate predictors


# 2. Correlation matrix (Spearman is safer if variables are skewed or non-linear)
cor_mat <- cor(predictors, use = "pairwise.complete.obs", method = "pearson")
round(cor_mat, 2)

# 3. Visualize
corrplot(cor_mat,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE)

# 4. Flag highly correlated pairs (|r| > 0.7 is a common threshold)
high_cor <- as.data.frame(as.table(cor_mat)) %>%
  filter(as.numeric(Var1) < as.numeric(Var2), abs(Freq) > 0.7) %>%
  arrange(desc(abs(Freq)))
high_cor



##### save for use in random forest analysis
#saveRDS(det_randomized, file = "01_data/02_processed_files/PArand_complete.rds")
#move to RF model code r. script

###
#to each location in the grided df add same as whats included in the RF model 
#distance to land
#zone?
#depth
#SAV
#fetch

grid <- read.csv("01_data/02_processed_files/for SAVM/HH_50mGrid_latlon.csv")

#assign each point a unique id
grid$cell_id <- seq_len(nrow(grid))

locs_sf   <- st_as_sf(grid, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
locs_proj <- st_transform(locs_sf, crs(depthrast))

# Extract depth. Rows come back in the same order as the points
ext <- terra::extract(depthrast, vect(locs_proj))
grid$depth_m <- ext[[2]]          # 2nd column is the raster values (1st is ID)

# Fill NAs from the nearest cell that has a value
has <- !is.na(grid$depth_m)
if (any(!has)) {
  nn   <- st_nearest_feature(locs_proj[!has, ], locs_proj[has, ])
  dist <- as.numeric(st_distance(locs_proj[!has, ], locs_proj[has, ][nn, ], by_element = TRUE))
  grid$depth_m[!has] <- grid$depth_m[has][nn]
  summary(dist)                   # check how far the borrowed values came from
}

#####plot up 
grid_sf <- st_as_sf(grid, coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)
HH_plot+
  geom_sf(data = grid_sf, aes(color = depth_m), size = 1.8, alpha = 0.7) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Depth (m)") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")


#SAV
locs_sf   <- st_as_sf(grid, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
locs_proj <- st_transform(locs_sf, crs(SAV2026rbgedits_raster))

# Extract sav Rows come back in the same order as the points
ext <- terra::extract(SAV2026rbgedits_raster, vect(locs_proj))
grid$sav <- ext[[2]]          # 2nd column is the raster values (1st is ID)

# Fill NAs from the nearest cell that has a value
has <- !is.na(grid$sav)
if (any(!has)) {
  nn   <- st_nearest_feature(locs_proj[!has, ], locs_proj[has, ])
  dist <- as.numeric(st_distance(locs_proj[!has, ], locs_proj[has, ][nn, ], by_element = TRUE))
  grid$sav[!has] <- grid$sav[has][nn]
  summary(dist)                   # check how far the borrowed values came from
}

grid_sf <- st_as_sf(grid, coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)
HH_plot+
  geom_sf(data = grid_sf, aes(color = sav), size = 1.8, alpha = 0.7) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "% SAV") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")



####fetch
locs_proj <- st_transform(locs_sf, crs(fetch2026_raster))

# Extract sav Rows come back in the same order as the points
ext <- terra::extract(fetch2026_raster, vect(locs_proj))
grid$fetch <- ext[[2]]          # 2nd column is the raster values (1st is ID)

# Fill NAs from the nearest cell that has a value
has <- !is.na(grid$fetch)
if (any(!has)) {
  nn   <- st_nearest_feature(locs_proj[!has, ], locs_proj[has, ])
  dist <- as.numeric(st_distance(locs_proj[!has, ], locs_proj[has, ][nn, ], by_element = TRUE))
  grid$fetch[!has] <- grid$fetch[has][nn]
  summary(dist)                   # check how far the borrowed values came from
}

#plot
grid_sf <- st_as_sf(grid, coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)
HH_plot+
  geom_sf(data = grid_sf, aes(color = fetch), size = 1.8, alpha = 0.7) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "fetch") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

#distance to shoreline 
grid_sf <- st_as_sf(grid, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Project to a metric CRS so distances are in metres (UTM 17N)
grid_utm <- st_transform(grid_sf, 32617)

# One boundary feature, so there is a single column of distances
shore_line <- shorelinemap |>
  st_transform(32617) |>
  st_union() |>
  st_boundary()

grid$shoreline_distance_m <- as.numeric(st_distance(grid_utm, shore_line)[, 1])

summary(grid$shoreline_distance_m)   # should be in metres and in the same range as the training data
summary(pa_data.train$shoreline_distance_m)

grid_sf <- st_as_sf(grid, coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)
HH_plot+
  geom_sf(data = grid_sf, aes(color = shoreline_distance_m), size = 1.8, alpha = 0.7) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "shoreline distance") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")


#######
#adding zone in arcgis 
#####
library(readxl)
gridnew<-read_excel("01_data/02_processed_files/grid50m_depthsavfetchzone.xls")

# Grid points with zone (plain data frame -> sf)
grid_sf <- st_as_sf(gridnew, coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE) |>
  dplyr::filter(!is.na(zone)) |>               # only grid points that have a zone
  st_transform(32617)

# PA points: use rand_long / rand_lat (the randomized locations)
pa_sf <- st_as_sf(pa_data |> st_drop_geometry(),
                  coords = c("rand_long", "rand_lat"),
                  crs = 4326, remove = FALSE) |>
  st_transform(32617)

# Nearest grid point for every PA point
nn <- st_nearest_feature(pa_sf, grid_sf)

d_m <- as.numeric(st_distance(pa_sf, grid_sf[nn, ], by_element = TRUE))
summary(d_m)                             # how far each PA point is from its grid point

# Assign zone
pa_data$zone <- trimws(as.character(grid_sf$zone[nn]))

#plot to ensure zones were assigned correctly 
HH_plot+
  geom_point(data = grid_sf, aes(color = zone, x=longitude, y=latitude), size = 1.8, alpha = 0.7) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

zone_levels <- c("Cootes Paradise", "Bayfront area", "Carrolls Bay",
                 "Grindstone", "West End", "Central", "North shore", "East End")


HH_plot+
  geom_point(data = pa_data, aes(rand_long, rand_lat, colour = zone),
             size = 0.6, alpha = 0.8) +
  scale_colour_brewer(palette = "Set1", name = "Zone", drop = FALSE, na.value = "black") +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  coord_sf(crs = 4326) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()

#save dataframe

#clean up any dates where teh active tags dropped to  <5
min_fish <- 4

# Unique fish detected as present on each date (geometry dropped only for the count)
fish_per_date <- pa_data |>
  st_drop_geometry() |>
  filter(presence == 1) |>
  group_by(date) |>
  summarise(n_fish = n_distinct(transmitter_id), .groups = "drop")


# Dates that pass
keep_dates <- fish_per_date |>
  filter(n_fish >= min_fish) |>
  pull(date)

# Filter presence AND absence rows by date
df_filtered <- pa_data |>
  filter(date %in% keep_dates)

# ---- Checks ----
# Rows and dates before/after
tibble(
  stage      = c("before", "after"),
  n_rows     = c(nrow(pa_data), nrow(df_filtered)),
  n_dates    = c(n_distinct(pa_data$date), n_distinct(df_filtered$date)),
  n_presence = c(sum(pa_data$presence == 1), sum(df_filtered$presence == 1)),
  n_absence  = c(sum(pa_data$presence == 0), sum(df_filtered$presence == 0))
)

# Confirm every remaining date has >= 4 unique fish present
df_filtered |>
  st_drop_geometry() |>
  filter(presence == 1) |>
  group_by(date) |>
  summarise(n_fish = n_distinct(transmitter_id), .groups = "drop") |>
  summarise(min_fish_on_any_date = min(n_fish))   # should be >= 4



##################
#this is the dataframe to be used in the RF code for analysis
#saveRDS(pa_filtered, "01_data/02_processed_files/PArand_complete_final.rds")
#############################






###########figures/tables
# Dates removed, including any date that had absences but no presence rows at all
removed_dates <- setdiff(unique(pa_data$date), keep_dates)
length(removed_dates)

threshold <- 4

# 1. Fish detected per day (start from the UNFILTERED data) ----
daily <- pa_data |>
  sf::st_drop_geometry() |>
  # filter(presence == 1) |>   # uncomment/rename if pa_data has absence rows
  mutate(day = as.Date(date)) |>
  group_by(day) |>
  summarise(n_fish = n_distinct(transmitter_id), .groups = "drop")

# 2. Full calendar, so days with no detections show up too ----
timeline <- tibble(day = seq(min(daily$day), max(daily$day), by = "day")) |>
  left_join(daily, by = "day") |>
  mutate(n_fish = replace_na(n_fish, 0L),
         status = if_else(n_fish >= threshold, "Included", "Excluded"))

# 3. Filter the data (keeps the sf geometry) ----
included_days <- timeline$day[timeline$status == "Included"]

pa_filtered <- pa_data |>
  filter(as.Date(date) %in% included_days)   # same presence filter here if needed

# 4. Consecutive included / excluded periods ----
periods <- timeline |>
  mutate(grp = consecutive_id(status)) |>
  group_by(grp, status) |>
  summarise(start    = min(day),
            end      = max(day),
            n_days   = n(),
            min_fish = min(n_fish),
            max_fish = max(n_fish),
            .groups  = "drop") |>
  dplyr::select(-grp)

gaps <- filter(periods, status == "Excluded")

print(periods, n = Inf)
print(arrange(gaps, desc(n_days)), n = Inf)   # longest gaps first

# 5. Overall and monthly summaries ----
timeline |>
  count(status) |>
  mutate(percent = round(100 * n / sum(n), 1))

monthly <- timeline |>
  group_by(month = lubridate::floor_date(day, "month")) |>
  summarise(days_total    = n(),
            days_included = sum(status == "Included"),
            days_excluded = sum(status == "Excluded"),
            pct_included  = round(100 * days_included / days_total, 1))

#write.csv(monthly, "03_outputs/02_files/detectionshist_monthly.csv")
print(monthly, n = Inf)
months_keep <- c("2023-12", "2024-04")

# Daily fish counts for those two months (includes excluded and zero-detection days)
daily_sub <- timeline |>
  filter(format(day, "%Y-%m") %in% months_keep) |>
  mutate(month = factor(format(day, "%b %Y"), levels = unique(format(day, "%b %Y"))))

print(daily_sub, n = Inf)

# Raw detection rows for those months (unfiltered by threshold)
pa_sub <- pa_data |>
  filter(format(as.Date(date), "%Y-%m") %in% months_keep)

# Daily plot
ggplot(daily_sub, aes(day, n_fish, fill = status)) +
  geom_col(width = 0.9) +
  geom_hline(yintercept = threshold - 0.5, linetype = "dashed") +
  facet_wrap(~ month, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c(Included = "#2b6f77", Excluded = "#e08b7d"),
                    name = NULL) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d") +
  labs(x = "Day of month", y = "Fish detected per day") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")
# 6. Timeline plot ----
ggplot() +
  geom_rect(data = gaps,
            aes(xmin = start - 0.5, xmax = end + 0.5, ymin = -Inf, ymax = Inf,
                fill = "Excluded"),
            alpha = 0.25) +
  geom_col(data = timeline, aes(day, n_fish, fill = status), width = 1) +
  geom_hline(yintercept = threshold - 0.5, linetype = "dashed") +
  scale_fill_manual(values = c(Included = "#2b6f77", Excluded = "#e08b7d"),
                    name = NULL) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y",
               expand = expansion(mult = 0.01)) +
  labs(x = "Date", y = "Fish detected per day") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "top")
###rudd that are included in this analsis. tag details. 


