#spatial random forest test run 


library(spatialRF)

# spatial RF essential just adds morans Eigenvectors that captuer spaital structure
#not explained by environmental predictors alone

library(dplyr)
library(tidyr)
library(parallel)
library(ggplot2)
library(patchwork)
library(sf)


#load in presence absence data with depth values assigned 
#01_data\02_processed_files\PA_rand_w_depth.rds
# --- 1. Prep data ---
# spatialRF wants a plain data frame (no sf geometry column) with explicit xy columns
pa_df <- PA_rand_w_depth %>%
  st_drop_geometry() %>%          # drop sf geometry if present
  filter(!is.na(depth_m), !is.na(presence)) %>%   # rf can't handle NAs
  as.data.frame()

# Use your randomized xy coords (or recv_x_m/recv_y_m, whichever represents
# the actual point location you want spatial structure computed from)
pa_df <- pa_df %>%
  mutate(
    xcoord = rand_long,
    ycoord = rand_lat
  )

# --- 2. Distance matrix ---
# For lat/long, this returns distances in meters if you use sf/geosphere;
# spatialRF's default expects a numeric matrix
coords_matrix <- as.matrix(pa_df[, c("xcoord", "ycoord")])

distance_matrix <- as.matrix(dist(coords_matrix))

# distance thresholds control which scales of spatial autocorrelation
# get modeled via the Moran's Eigenvector Maps (MEMs) - start coarse
distance_thresholds <- c(0, 1000, 5000)  # coarser, fewer thresholds

# --- 3. Define response/predictors ---
dependent.variable.name <- "presence"
predictor.variable.names <- c("depth_m")

# --- 4. Fit a non-spatial RF first, as spatialRF recommends,
#          to check for residual spatial autocorrelation ---
rf_model <- spatialRF::rf(
  data = pa_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance_thresholds,
  xy = pa_df[, c("xcoord", "ycoord")],
  seed = 42,
  verbose = TRUE
)

# check Moran's I of the residuals
spatialRF::plot_moran(rf_model, verbose = FALSE)


rf_spatial_model <- spatialRF::rf_spatial(
  model = rf_model,
  method = "mem.moran.sequential",  # default MEM selection method
  verbose = TRUE
)

spatialRF::plot_moran(rf_spatial_model, verbose = FALSE)
spatialRF::plot_importance(rf_spatial_model)