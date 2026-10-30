
#griffin et al. RSF resource random forest walk through 

library(here)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(raster)
library(tmap)
library(mlr)
library(ranger)
library(iml)
library(pdp)
library(sf)
library(imp)

#predictors October 2026

#others to consider
#photoperiod - vary temporaly 

#stack rasters???

#hamilton harbour polgyon file
shorelinemap <- read_sf("01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp")
shorelinemap <- st_transform(shorelinemap, crs = 4326)
###############################################

HH_plot<-ggplot(shorelinemap) +
 geom_sf(fill = "lightblue", color = "black", size = 0.5) +
 theme_minimal() +
 theme(axis.text = element_text(size = 8))
#use glatos rpackage to summarize detections by num_fish and num_dets

HH_plot

################################
#load in rasters and P/A data
#############################
#raster layers
#SAV
SAV2026rbgedits_raster <- rast("01_data/02_processed_files/raster layers/sav_rbgedits1")
plot(SAV2026rbgedits_raster)
#depth 
depthrast<-rast("01_data/02_processed_files/raster layers/depth/WL.tif")
plot(depthrast)
#fetch
fetch2026_raster <- rast("01_data/02_processed_files/raster layers/fetch/fetchnew.tif")
plot(fetch2026_raster)

#PA data frame with all variables added  (see adding predictors to DF.R code)

PA_rand_w_depthSAVfetch<-readRDS("01_data/02_processed_files/PArand_complete.rds")
# Row ID for indexing
PA_rand_w_depthSAVfetch$rowID <- seq_len(nrow(PA_rand_w_depthSAVfetch))

set.seed(1234)

is.na(PA_rand_w_depthSAVfetch$zone)

levels(base::as.factor(PA_rand_w_depthSAVfetch$presence))
PA_rand_w_depthSAVfetch$thermal<-base::as.factor(PA_rand_w_depthSAVfetch$thermal)
PA_rand_w_depthSAVfetch$season<-base::as.factor(PA_rand_w_depthSAVfetch$season)
PA_rand_w_depthSAVfetch$presence<-base::as.factor(PA_rand_w_depthSAVfetch$presence)

#################################

pa_data <- PA_rand_w_depthSAVfetch %>%
  sf::st_drop_geometry() %>%
  dplyr::select(presence, depth_m, SAV, season, zone, fetch, shoreline_distance_m, rand_long, rand_lat) %>%
  mutate(
    presence = factor(presence, levels = c(0, 1), labels = c("abs", "pres")),
    rowID    = seq_len(n())
  )
pa_data$zone<-base::as.factor(pa_data$zone)

#split into train and test data
#change % split right now at 60% train 
set.seed(1234)
pa_data.train <- pa_data[sample(1:nrow(pa_data), nrow(pa_data) * 0.7, replace = FALSE), ]
pa_data.test  <- pa_data[!(pa_data$rowID %in% pa_data.train$rowID), ]


coords.train <- pa_data.train[, c("rand_long", "rand_lat")]
coords.test  <- pa_data.test[, c("rand_long", "rand_lat")]

pa_data.train <- dplyr::select(pa_data.train, -rand_long, -rand_lat, -rowID)
pa_data.test  <- dplyr::select(pa_data.test, -rand_long, -rand_lat, -rowID)

task.train <- makeClassifTask(data = pa_data.train, target = "presence",
                               positive = "pres", coordinates = coords.train)
task.test  <- makeClassifTask(data = pa_data.test, target = "presence",
                               positive = "pres", coordinates = coords.test)



# defines model
#can adjust number of trees here
lrn.Bin <- makeLearner(cl = "classif.ranger", predict.type = "prob", num.trees=1000)

# Spatial CV + random hyperparameter search
#SpCV clusters training points by coordiantes (k-means) into 5 groups
#each round holds out one of the groups and trains on the other 4
#i.e. validation points are geographically separated from training points 
perf_level <- makeResampleDesc("SpCV", iters = 5)

#tries 50 random coos of 3 hyperparameters
ctrl <- makeTuneControlRandom(maxit = 50L)

#mtry = how many predictors each split considers
# sample.fraction = fraction of the data each tree sees
# min.node.size = min size of terminal node 
ps <- makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(pa_data.train) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)

#run tuning 
set.seed(1234)
tune_HH <- tuneParams(learner = lrn.Bin, task = task.train,
                       resampling = perf_level, par.set = ps,
                       control = ctrl, measures = mlr::auc)

##############################
##
##
##finding a sufficient number of trees for model
best <- tune_HH$x   # mtry, sample.fraction, min.node.size
tree_grid <- c(100, 250, 500, 750, 1000, 1500, 2000, 3000, 5000)

oob_curve <- lapply(tree_grid, function(nt) {
  # repeat a few seeds to see run-to-run noise
  sapply(1:5, function(s) {
    set.seed(s)
    rf <- ranger(presence ~ ., data = pa_data.train,
                 num.trees       = nt,
                 mtry            = best$mtry,
                 sample.fraction = best$sample.fraction,
                 min.node.size   = best$min.node.size,
                 probability     = TRUE)
    rf$prediction.error   # OOB Brier score for probability forests
  })
})

oob_df <- data.frame(
  num.trees = rep(tree_grid, each = 5),
  brier     = unlist(oob_curve)
)

library(ggplot2)
ggplot(oob_df, aes(num.trees, brier)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  scale_x_log10() +
  labs(y = "OOB Brier score", x = "Number of trees")


#where mean flattens out, pick a value at or a little past that point 
#here is would be aout 1000 trees




# Final learner + training
# new learner is built with hyperparameters that came out on top (from above code)
lrn_rf.Bin <- setHyperPars(makeLearner("classif.ranger", importance = "impurity",
                                        predict.type = "prob", num.trees=1000), par.vals = tune_HH$x)
set.seed(1234)
model_rf.Bin_HH <- mlr::train(lrn_rf.Bin, task.train)
results_HH <- mlr::getLearnerModel(model_rf.Bin_HH)


set.seed(1234)
pred_HH <- predict(model_rf.Bin_HH, task = task.test)
(performance_HH <- calculateROCMeasures(pred_HH))

#####################################################################
#this code creates a plot of the predictors and summarises on a scale of 1 to 0
##different way to plot predictors importance
# X_HH <- pa_data.train[which(names(pa_data.train) != "presence")]

 #predictor_HH <- Predictor$new(model_rf.Bin_HH, data = X_HH, y = pa_data.train$presence)
# imp_HH <- FeatureImp$new(predictor_HH, loss = "ce")

# imp_df_HH <- imp_HH$results

# # swap in your real predictor names — edit these labels to match what you kept in pa_data
# imp_df_HH$feature_label <- ifelse(imp_df_HH$feature == "depth_m", "Depth (m)",
#                             ifelse(imp_df_HH$feature == "SAV", "SAV",
#                             ifelse(imp_df_HH$feature == "season", "Season",
#                             ifelse(imp_df_HH$feature == "thermal", "Thermal period",
#                             ifelse(imp_df_HH$feature == "dist_to_shoreline_m", "Distance to shoreline",
#                             ifelse(imp_df_HH$feature == "fetch", "Fetch",
#                             ifelse(imp_df_HH$feature == "zone", "Zone",
#                                    imp_df_HH$feature)))))))


# ordered <- imp_df_HH %>% arrange(desc(importance)) %>% distinct(feature_label)
# ordered_fin <- ordered$feature_label
# imp_df_HH$feature_label <- factor(imp_df_HH$feature_label, levels = rev(ordered_fin))

# normalize <- function(x) (x - min(x)) / (max(x) - min(x))
# imp_df_HH$importance_norm <- normalize(imp_df_HH$importance)

# ggplot(imp_df_HH, aes(y = feature_label, x = importance_norm, fill = importance_norm)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_gradient(low = "darkorchid1", high = "darkorchid4") +
#   ylab("Feature") + xlab("Importance") + guides(fill = "none") +
#   theme_bw() +
#   theme(
#     axis.text.x  = element_text(size = 14),
#     axis.text.y  = element_text(size = 14),
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16)
#   )


##############other ways to plot out variable importance
#MSE and brier score, shows how much the model leans on each predictor variable to assist in accurate predictions 
#of presence absence data 
mse_imp <- function(rf, test, target = "presence", reps = 30, seed = 1234) {
  set.seed(seed)
  obs   <- as.numeric(test[[target]] == "pres")
  preds <- setdiff(names(test), target)

  mse   <- function(d) mean((obs - predict(rf, data = d)$predictions[, "pres"])^2)
  base  <- mse(test)                                  # MSE with nothing shuffled

  out <- do.call(rbind, lapply(preds, function(v) {
    inc <- replicate(reps, {
      d <- test
      d[[v]] <- sample(d[[v]])                        # break the predictor-response link
      mse(d) - base                                   # increase in MSE
    })
    data.frame(feature = v, mean_inc = mean(inc),
               lo = quantile(inc, 0.025), hi = quantile(inc, 0.975),
               pct_inc = 100 * mean(inc) / base)
  }))
  attr(out, "baseline_mse") <- base
  out
}

imp_mse <- mse_imp(results_HH, pa_data.test)
attr(imp_mse, "baseline_mse")      # baseline MSE (Brier) on this data
sqrt(attr(imp_mse, "baseline_mse"))# baseline RMSE

# Optional nicer labels
labs_map <- c(zone = "Zone", season = "Season",
              depth_m = "Depth (m)", fetch = "Fetch", SAV = "SAV %",
              shoreline_distance_m = "Distance to shoreline (m)")
imp_mse$label <- ifelse(imp_mse$feature %in% names(labs_map),
                        labs_map[imp_mse$feature], imp_mse$feature)

ggplot(imp_mse, aes(mean_inc, reorder(label, mean_inc))) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_col(fill = "purple", colour = "purple4", width = 0.7) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) +
  labs(x = "Increase in MSE (Brier score) when predictor is shuffled",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))


brier_test <- performance(pred_HH, measures = mlr::brier)   # MSE
rmse_test  <- sqrt(brier_test)                              # RMSE
c(MSE = unname(brier_test), RMSE = unname(rmse_test))

##########
#INDIVIDUAL PDP PLOTS
#with updated predictor importance MSE brier plotting code (above)
top_vars <- imp_mse$feature[order(-imp_mse$mean_inc)]
top_vars   # season, zone, fetch, depth_m, SAV, shoreline_distance_m


pdp_list <- lapply(setNames(top_vars, top_vars), function(v)
  pdp::partial(results_HH, pred.var = v, prob = TRUE, which.class = 2,
               train = pa_data.train[, names(pa_data.train) != "presence"],
               progress = "none"))


# 1. One shared y-range across every predictor's PDP
y_rng <- range(unlist(purrr::map(pdp_list, ~ range(.x$yhat, na.rm = TRUE))))
y_rng <- c(floor(y_rng[1] * 20) / 20, ceiling(y_rng[2] * 20) / 20)   # round out to nearest 0.05
y_rng

labs_map <- c(season = "Season", zone = "Zone", fetch = "Fetch", depth_m = "Depth (m)",
              SAV = "SAV %", shoreline_distance_m = "Distance to shoreline (m)")


plot_one <- function(df, v, ylim = y_rng) {
  base <- theme_bw() + theme(text = element_text(size = 16))
  ylab <- bquote("Marginal effect" ~ (hat(y)))
  xl   <- if (v %in% names(labs_map)) labs_map[[v]] else v

  if (is.numeric(df[[v]])) {
    ggplot(df, aes(.data[[v]], yhat)) +
      geom_smooth(colour = "purple", linewidth = 1.1) +
      coord_cartesian(ylim = ylim) +
      labs(x = xl, y = ylab) + base
  } else {
    ggplot(df, aes(.data[[v]], yhat)) +
      geom_col(fill = "purple", colour = "purple4") +
      coord_cartesian(ylim = c(0, ylim[2])) +       # bars must start at 0 to be read honestly
      labs(x = xl, y = ylab) + base +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
  }
}

plots <- purrr::imap(pdp_list, ~ plot_one(.x, .y))
patchwork::wrap_plots(plots, ncol = 2)


# 2-way interactions with the strongest-interacting feature
conflicted::conflict_prefer("map", "purrr")
conflicted::conflict_prefer("imap", "purrr")
conflicted::conflict_prefer("set_names", "purrr")


plot_one <- function(df, v) {
  base <- theme_bw() + theme(text = element_text(size = 16))
  ylab <- bquote("Marginal effect" ~ (hat(y)))
  xl   <- if (v %in% names(labs_map)) labs_map[[v]] else v

  if (is.numeric(df[[v]])) {
    ggplot(df, aes(.data[[v]], yhat)) +
      geom_smooth(colour = "purple", linewidth = 1.1) +
      labs(x = xl, y = ylab) + base
  } else {
    ggplot(df, aes(.data[[v]], yhat)) +
      geom_col(fill = "purple", colour = "purple4") +
      labs(x = xl, y = ylab) + base +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
  }
}

plots <- purrr::imap(pdp_list, ~ plot_one(.x, .y))
patchwork::wrap_plots(plots, ncol = 2)


















####H statistic interation step 
set.seed(1234)
pa_data.train_sub_f <- pa_data.train[sample(nrow(pa_data.train),
                                            round(nrow(pa_data.train) * 0.25)), ]
X_HH_sub <- dplyr::select(pa_data.train_sub_f, -presence)

predictor_HH_sub <- Predictor$new(model_rf.Bin_HH, data = X_HH_sub,
                                  y = pa_data.train_sub_f$presence)

vars <- names(X_HH_sub)
pairs <- combn(vars, 2, simplify = FALSE)

# 1. Overall H per feature (how much each variable interacts with anything)
interact_all <- Interaction$new(predictor_HH_sub)
plot(interact_all)

# 2. Pairwise H for the top one or two features only
interact_top <- Interaction$new(predictor_HH_sub, feature = "season")
interact_top$results %>% dplyr::arrange(dplyr::desc(.interaction))
###interactions 
#plus a plot

labs_map <- c(zone = "Zone", season = "Season",
              depth_m = "Depth (m)", fetch = "Fetch", SAV = "SAV %",
              shoreline_distance_m = "Distance to shoreline")
lab <- function(v) ifelse(v %in% names(labs_map), labs_map[v], v)

# If interact_top is the iml Interaction object, use $results.
# If it's already a data frame, replace interact_top$results with interact_top.
top4 <- interact_top$results %>%
  group_by(.feature) %>%
  summarise(H = mean(.interaction), .groups = "drop") %>%
  arrange(desc(H)) %>%
  slice_head(n = 4) %>%
  separate(.feature, into = c("a", "b"), sep = ":") %>%
  mutate(pair = paste(lab(a), "×", lab(b)))
top4

ggplot(top4, aes(H, reorder(pair, H))) +
  geom_col(fill = "purple", colour = "purple4", width = 0.7) +
  labs(x = "Interaction strength (H statistic)", y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))

# ---- 1. Interactions with SEASON (the variable used for colour) ----
interact_season <- Interaction$new(predictor_HH_sub, feature = "season")

interact_season_df <- interact_season$results %>%
  group_by(.feature) %>%
  summarise(H = mean(.interaction), .groups = "drop") %>%
  arrange(desc(H)) %>%
  separate(.feature, into = c("a", "b"), sep = ":") %>%
  mutate(partner = ifelse(a == "season", b, a)) %>%
  as.data.frame()
interact_season_df

# ---- 2. Settings ----
season_lv   <- c("Spring", "Summer", "Fall", "Winter")
season_cols <- c(Spring = "#ffb3ba", Summer = "#a8e6cf",
                 Fall   = "#f9c8a0", Winter = "#85c1e9")

xlabs <- c(fetch = "Fetch", zone = "Zone", month = "Month", depth_m = "Depth (m)",
           shoreline_distance_m = "Distance to shoreline (m)", SAV = "SAV %")
lab <- function(v) if (v %in% names(xlabs)) xlabs[[v]] else v

# Partners of season, strongest interaction first, restricted to variables in the model
X_cols <- names(X_HH_sub)
vars <- interact_season_df$partner
vars <- vars[vars %in% X_cols & vars != "season"]
vars

# ---- 3. Two-way PDPs: each partner x season ----
pdp_list <- map(set_names(vars), function(v) {
  pdp::partial(results_HH,
               pred.var        = c(v, "season"),
               prob            = TRUE,
               which.class     = 2,
               train           = X_HH_sub,       # predictors only, same columns as the model
               grid.resolution = 30,
               progress        = "none")
})

# ---- 4. One shared y-range, so every panel is comparable ----
y_rng <- range(unlist(map(pdp_list, ~ range(.x$yhat, na.rm = TRUE))))
y_rng <- c(floor(y_rng[1] * 20) / 20, ceiling(y_rng[2] * 20) / 20)

# ---- 5. Plot function (defined BEFORE it is used) ----
plot_pdp <- function(df, v) {
  df$season <- factor(df$season, levels = season_lv)

  base <- theme_bw() +
    theme(axis.text    = element_text(size = 12),
          axis.title   = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.title = element_text(size = 14))

  if (is.numeric(df[[v]])) {
    ggplot(df, aes(.data[[v]], yhat, colour = season, group = season)) +
      geom_smooth(linewidth = 1.1) +
      scale_colour_manual(values = season_cols, name = "Season", drop = FALSE) +
      labs(x = lab(v), y = bquote("Marginal effect" ~ (hat(y)))) +base
  } else {
    ggplot(df, aes(.data[[v]], yhat, fill = season)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = season_cols, name = "Season", drop = FALSE) +
      labs(x = lab(v), y = bquote("Marginal effect" ~ (hat(y)))) +
       base +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
  }
}

plots <- imap(pdp_list, ~ plot_pdp(.x, .y))

# ---- 6. Combine with one shared legend and save ----
fig <- wrap_plots(plots, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
fig

ggsave("pdp_season_interactions.png", plot = fig, width = 11, height = 12, dpi = 300)



#top 3 two-way interaction plots as grids 
#could you add season as a colour?
#grids for depth and zone
#SAV season allows us to narrow in on smaller bins for SAV
#dsitance to land and season 
#distance to land and depth (should make sense just like depth and SAV)

library(patchwork)

season_lv <- c("Spring", "Summer", "Fall", "Winter")
zone_levels <- c("Cootes Paradise", "Grindstone", "Carrolls Bay", "Bayfront area",
                 "West End", "North shore", "Central", "East End")

xlabs <- c(fetch = "Fetch", zone = "Zone", depth_m = "Depth (m)",
           shoreline_distance_m = "Distance to shoreline (m)"
           , SAV = "SAV %")

# bin the predictor (continuous only) and average yhat per bin x season
make_grid_df <- function(df, v, n_bins = 8) {
  df$season <- factor(df$season, levels = season_lv)

  if (is.numeric(df[[v]])) {
    brks <- seq(min(df[[v]]), max(df[[v]]), length.out = n_bins + 1)
    df$bin <- cut(df[[v]], breaks = brks, include.lowest = TRUE, dig.lab = 4)
  } else {
    df$bin <- if (v == "zone") factor(df[[v]], levels = zone_levels) else factor(df[[v]])
  }

  df %>%
    data.frame() %>%
    dplyr::group_by(bin, season) %>%
    dplyr::summarise(mean = mean(yhat), .groups = "drop")
}

grid_list <- lapply(names(pdp_list), function(v) make_grid_df(pdp_list[[v]], v))
names(grid_list) <- names(pdp_list)

# common colour scale across panels so they are comparable
fill_rng <- range(unlist(lapply(grid_list, function(d) range(d$mean))))

plot_grid <- function(d, v) {
  ggplot(d, aes(x = season, y = bin, fill = mean)) +
    geom_tile(width = 1, height = 0.9) +
    scale_fill_gradient(low = "white", high = "steelblue", limits = fill_rng) +
    labs(x = "Season", y = xlabs[[v]],
         fill = bquote("Mean marginal effect" ~ (hat(y)))) +
    theme_classic() +
    theme(axis.text.y = element_text(colour = "black", size = 11),
          axis.text.x = element_text(angle = 35, hjust = 1, colour = "black", size = 12),
          axis.title  = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))
}

grid_plots <- Map(plot_grid, grid_list, names(grid_list))

wrap_plots(grid_plots, ncol = 2, guides = "collect") &
  theme(legend.position = "right")

ggsave("pdp_season_grids.png", width = 11, height = 12, dpi = 300)

#depth and zone
# 1. Two-way PDP: zone x depth
pdp_zd <- pdp::partial(results_HH,
                       pred.var        = c("zone", "depth_m"),
                       prob            = TRUE,
                       which.class     = 2,
                       train           = X_HH_sub,
                       grid.resolution = 30,
                       progress        = "none")

pdp_zd$zone <- factor(trimws(as.character(pdp_zd$zone)), levels = zone_levels)

# 2. Bin depth and average yhat within each zone x depth bin
n_bins <- 8
brks <- seq(min(pdp_zd$depth_m), max(pdp_zd$depth_m), length.out = n_bins + 1)
pdp_zd$depth_bin <- cut(pdp_zd$depth_m, breaks = brks, include.lowest = TRUE, dig.lab = 3)

grid_zd <- pdp_zd %>%
  data.frame() %>%
  group_by(zone, depth_bin) %>%
  summarise(mean = mean(yhat), .groups = "drop")

# 3. Plot
ggplot(grid_zd, aes(zone, depth_bin, fill = mean)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue",
                      name = expression("Marginal effect" ~ (hat(y)))) +
  labs(x = "Zone", y = "Depth (m)") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle = 35, hjust = 1, size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title   = element_text(size = 16),
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 14))

#SAV and zone 

pdp_zd <- pdp::partial(results_HH,
                       pred.var        = c("zone", "SAV"),
                       prob            = TRUE,
                       which.class     = 2,
                       train           = X_HH_sub,
                       grid.resolution = 30,
                       progress        = "none")

pdp_zd$zone <- factor(trimws(as.character(pdp_zd$zone)), levels = zone_levels)

# 2. Bin depth and average yhat within each zone x depth bin
n_bins <- 8
brks <- seq(min(pdp_zd$SAV), max(pdp_zd$SAV), length.out = n_bins + 1)
pdp_zd$sav_bin <- cut(pdp_zd$SAV, breaks = brks, include.lowest = TRUE, dig.lab = 3)

grid_zd <- pdp_zd %>%
  data.frame() %>%
  group_by(zone, sav_bin) %>%
  summarise(mean = mean(yhat), .groups = "drop")

# 3. Plot
ggplot(grid_zd, aes(zone, sav_bin, fill = mean)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue",
                      name = expression("Marginal effect" ~ (hat(y)))) +
  labs(x = "Zone", y = "SAV (%)") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle = 35, hjust = 1, size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title   = element_text(size = 16),
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 14))


##############
library(patchwork)

season_lv <- c("Spring", "Summer", "Fall", "Winter")

# 1. Finer PDPs for just these three predictors
focus <- c(dist_to_shoreline_m = "Distance to shoreline (m)",
           depth_m             = "Depth (m)",
           SAV                 = "SAV %")

pdp_focus <- lapply(setNames(names(focus), names(focus)), function(v) {
  pdp::partial(results_HH,
               pred.var        = c(v, "season"),
               prob            = TRUE,
               which.class     = 2,
               train           = pa_data.train,
               grid.resolution = 60,
               progress        = "none")
})

# 2. Grid plot function: own fill scale, optional custom breaks
plot_season_grid <- function(df, v, label, breaks = NULL, n_bins = 10,
                             show_values = FALSE) {
  df$season <- factor(df$season, levels = season_lv)

  if (is.null(breaks)) {
    breaks <- seq(min(df[[v]]), max(df[[v]]), length.out = n_bins + 1)
  }
  df$bin <- cut(df[[v]], breaks = breaks, include.lowest = TRUE, dig.lab = 4)

  if (anyNA(df$bin)) warning(v, ": some values fall outside your breaks")

  d <- df %>%
    data.frame() %>%
    dplyr::filter(!is.na(bin)) %>%
    dplyr::group_by(bin, season) %>%
    dplyr::summarise(mean = mean(yhat), .groups = "drop")

  p <- ggplot(d, aes(x = season, y = bin, fill = mean)) +
    geom_tile(width = 1, height = 0.95) +
    scale_fill_gradient(low = "white", high = "steelblue") +   # free scale, per plot
    labs(x = "Season", y = label,
         fill = bquote("Mean marginal effect" ~ (hat(y)))) +
    theme_classic() +
    theme(axis.text.y  = element_text(colour = "black", size = 12),
          axis.text.x  = element_text(angle = 35, hjust = 1, colour = "black", size = 12),
          axis.title   = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.title = element_text(size = 12))

  if (show_values) {
    p <- p + geom_text(aes(label = sprintf("%.2f", mean)), size = 3)
  }
  p
}

# 3. Plot each one separately
p_dist  <- plot_season_grid(pdp_focus$dist_to_shoreline_m, "dist_to_shoreline_m",
                            focus[["dist_to_shoreline_m"]], n_bins = 12)
p_depth <- plot_season_grid(pdp_focus$depth_m, "depth_m",
                            focus[["depth_m"]], n_bins = 10)
p_sav   <- plot_season_grid(pdp_focus$SAV, "SAV",
                            focus[["SAV"]], n_bins = 10)

p_dist
p_depth
p_sav

ggsave("pdp_grid_dist_shoreline.png", p_dist,  width = 6, height = 6, dpi = 300)
ggsave("pdp_grid_depth.png",          p_depth, width = 6, height = 6, dpi = 300)
ggsave("pdp_grid_SAV.png",            p_sav,   width = 6, height = 6, dpi = 300)

##############################
## ---- 1. AUC: test set vs spatial CV ----
auc_test <- mlr::performance(pred_HH, measures = mlr::auc)
auc_test

tune_HH$y          

# spatial CV AUC for the best hyperparameters (auc.test.mean)

# If the test AUC is clearly higher than the spatial CV AUC, the random
# test split is probably flattering the model (spatial autocorrelation).


## ---- 2. ROC curve (test set) ----
roc_df <- mlr::generateThreshVsPerfData(
  pred_HH, measures = list(mlr::fpr, mlr::tpr, mlr::tnr)
)$data

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_line(colour = "purple", linewidth = 1.2) +
  coord_equal() +
  labs(x = "False positive rate (1 - specificity)",
       y = "True positive rate (sensitivity)",
       title = sprintf("Test-set ROC (AUC = %.3f)", auc_test)) +
  theme_bw() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16))


## ---- 3. Choose a threshold from spatial CV predictions (not the test set) ----
set.seed(1234)
rs <- mlr::resample(lrn_rf.Bin, task.train, resampling = perf_level,
                    measures = mlr::auc, show.info = FALSE)
rs$aggr   # pooled spatial CV AUC

cv_thr <- mlr::generateThreshVsPerfData(
  rs$pred, measures = list(mlr::tpr, mlr::tnr)
)$data

# Youden's J / TSS = sensitivity + specificity - 1
cv_thr$TSS <- cv_thr$tpr + cv_thr$tnr - 1
best_thr   <- cv_thr$threshold[which.max(cv_thr$TSS)]
best_thr

ggplot(cv_thr, aes(x = threshold, y = TSS)) +
  geom_line(linewidth = 1.1, colour = "purple") +
  geom_vline(xintercept = best_thr, linetype = "dashed") +
  theme_bw() +
  labs(x = "Probability threshold", y = "TSS (sens + spec - 1)")


## ---- 4. Confusion matrix on the test set at that threshold ----
pred_HH_thr <- mlr::setThreshold(pred_HH, best_thr)
(performance_HH_thr <- mlr::calculateROCMeasures(pred_HH_thr))



#########################################################
##
## prediction probability mapping from RF modelling
##
###############################################################
#build the grid dataframe up 
#50m grid dataframe - see builing PA dataframe code
#see adding predictors to DF (PA dataframe)
grid <- read.csv("01_data/02_processed_files/for SAVM/HH_50mGrid_latlon.csv")

#########
#Ranger built in function attempt

pred_rf<-predict(model_rf.Bin_HH, task.test, type="response")
pred_rf <- pred_rf$predictions [, 2]

library(viridis)

needed <- results_HH$forest$independent.variable.names
needed

# Grid x seasons (no years)
# 1. Load your 50 m prediction grid (use your own file path)
grid <- read.csv("01_data/02_processed_files/for SAVM/HH_50mGrid_latlon.csv")

# 2. Confirm it's a data frame, not a function
class(grid)      # should be "data.frame"
names(grid)      # shows the column names you have
head(grid)

seasons <- data.frame(season = levels(factor(pa_data.train$season)))
gridseason <- merge(seasons, grid)
gridseason$season <- factor(gridseason$season, levels = levels(factor(pa_data.train$season)))
# if zone is in the model:
# gridseason$zone <- factor(gridseason$zone, levels = levels(pa_data.train$zone))

gridseason <- gridseason[complete.cases(gridseason[, needed]), ]

gridseason$pred <- predict(results_HH, data = gridseason[, needed])$predictions[, "pres"]

# Shoreline layer must be in EPSG:32617 to match UTM.X / UTM.Y
shore <- st_transform(shorelinemap, 32617)

ggplot() +
  geom_tile(data = gridseason, aes(longitude,latitude , fill = pred)) +
  geom_sf(data = shore, fill = NA, colour = "darkgrey", inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Presence probability", limits = c(0, 1)) +
  facet_wrap(~ season) +
  coord_sf(crs = 32617, datum = 32617) +
  labs(x = "Easting", y = "Northing") +
  theme_bw() + theme(text = element_text(size = 18))















###second example pick one to keep delete other 

library(dplyr); library(tidyr); library(sf); library(terra)
library(ggplot2); library(viridis)

needed <- results_HH$forest$independent.variable.names
needed    # every one of these must be a column in the surface, with matching names and levels

# 1. Grid points (rename columns to match your model; edit the right-hand names to your grid)
surf <- grid %>%
  transmute(ID, x = longitude    , y = latitude ,
            depth_m = Depth, SAV = SAV, fetch = Fetch)

# 2. Corrected distance to shoreline, computed the same way as for the detections
pts <- st_as_sf(surf, coords = c("x", "y"), crs = 32617, remove = FALSE)
shore_line <- st_boundary(st_transform(shorelinemap, 32617))
surf$dist_to_shoreline_m <- as.numeric(apply(st_distance(pts, shore_line), 1, min))

# 3. Zone, only if it is still in the model (zones_sf = your zone polygons with a 'zone' column)
# surf$zone <- st_join(pts, st_transform(zones_sf, 32617))$zone
# surf$zone <- factor(trimws(as.character(surf$zone)), levels = levels(pa_data.train$zone))

# 4. One row per cell per season
season_lv <- levels(factor(pa_data.train$season))
surf <- surf %>%
  tidyr::crossing(season = season_lv) %>%
  mutate(season = factor(season, levels = season_lv))

# 5. Predict (ranger errors on NAs, so drop incomplete rows first)
surf <- surf[complete.cases(surf[, needed]), ]
surf$pred <- predict(results_HH, data = surf[, needed])$predictions[, "pres"]

# 6. Optional mask: cells whose covariates fall outside the sampled range
covs <- c("depth_m", "SAV", "fetch", "dist_to_shoreline_m")
in_range <- Reduce(`&`, lapply(covs, function(v)
  surf[[v]] >= min(pa_data.train[[v]]) & surf[[v]] <= max(pa_data.train[[v]])))
surf$pred_masked <- ifelse(in_range, surf$pred, NA)
mean(in_range)   # share of cells inside the sampled range

# 7. Map, faceted by season
shore <- st_transform(shorelinemap, 32617)

ggplot() +
  geom_raster(data = surf, aes(x, y, fill = pred_masked)) +
  geom_sf(data = shore, fill = NA, colour = "darkgrey", inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Relative habitat\nsuitability", limits = c(0, 1),
                       na.value = "grey85") +
  facet_wrap(~ season) +
  coord_sf(crs = 32617, datum = 32617) +
  labs(x = "Easting", y = "Northing") +
  theme_bw() + theme(text = element_text(size = 16))

# 8. GeoTIFFs for GIS (one layer per season)
r <- terra::rast(lapply(season_lv, function(s)
  terra::rast(surf[surf$season == s, c("x", "y", "pred_masked")],
              type = "xyz", crs = "EPSG:32617")))
names(r) <- season_lv
terra::writeRaster(r, "HH_suitability_by_season.tif", overwrite = TRUE)