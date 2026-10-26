
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

levels(as.factor(PA_rand_w_depthSAVfetch$presence))
PA_rand_w_depthSAVfetch$thermal<-as.factor(PA_rand_w_depthSAVfetch$thermal)
PA_rand_w_depthSAVfetch$season<-as.factor(PA_rand_w_depthSAVfetch$season)
PA_rand_w_depthSAVfetch$presence<-as.factor(PA_rand_w_depthSAVfetch$presence)

#################################

pa_data <- PA_rand_w_depthSAVfetch %>%
  sf::st_drop_geometry() %>%
  dplyr::select(presence, depth_m, SAV, season, thermal, zone, fetch, dist_to_shoreline_m, rand_long, rand_lat) %>%
  mutate(
    presence = factor(presence, levels = c(0, 1), labels = c("abs", "pres")),
    rowID    = seq_len(n())
  )
pa_data$zone<-as.factor(pa_data$zone)

#split into train and test data
#change % split right now at 60% train 
pa_data.train <- pa_data[sample(1:nrow(pa_data), nrow(pa_data) * 0.6, replace = FALSE), ]
pa_data.test  <- pa_data[!(pa_data$rowID %in% pa_data.train$rowID), ]


coords.train <- pa_data.train[, c("rand_long", "rand_lat")]
coords.test  <- pa_data.test[, c("rand_long", "rand_lat")]

pa_data.train <- dplyr::select(pa_data.train, -rand_long, -rand_lat, -rowID)
pa_data.test  <- dplyr::select(pa_data.test, -rand_long, -rand_lat, -rowID)

task.train <- makeClassifTask(data = pa_data.train, target = "presence",
                               positive = "pres", coordinates = coords.train)
task.test  <- makeClassifTask(data = pa_data.test, target = "presence",
                               positive = "pres", coordinates = coords.test)



# Learner
lrn.Bin <- makeLearner(cl = "classif.ranger", predict.type = "prob")

# Spatial CV + random hyperparameter search
perf_level <- makeResampleDesc("SpCV", iters = 5)
ctrl <- makeTuneControlRandom(maxit = 50L)

ps <- makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(pa_data.train) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)

set.seed(1234)
tune_HH <- tuneParams(learner = lrn.Bin, task = task.train,
                       resampling = perf_level, par.set = ps,
                       control = ctrl, measures = mlr::auc)

# Final learner + training
lrn_rf.Bin <- setHyperPars(makeLearner("classif.ranger", importance = "impurity",
                                        predict.type = "prob"), par.vals = tune_HH$x)
set.seed(1234)
model_rf.Bin_HH <- train(lrn_rf.Bin, task.train)
results_HH <- mlr::getLearnerModel(model_rf.Bin_HH)

set.seed(1234)
pred_HH <- predict(model_rf.Bin_HH, task = task.test)
(performance_HH <- calculateROCMeasures(pred_HH))

X_HH <- pa_data.train[which(names(pa_data.train) != "presence")]

predictor_HH <- Predictor$new(model_rf.Bin_HH, data = X_HH, y = pa_data.train$presence)
imp_HH <- FeatureImp$new(predictor_HH, loss = "ce")

imp_df_HH <- imp_HH$results

# swap in your real predictor names — edit these labels to match what you kept in pa_data
imp_df_HH$feature_label <- ifelse(imp_df_HH$feature == "depth_m", "Depth (m)",
                            ifelse(imp_df_HH$feature == "SAV", "SAV",
                            ifelse(imp_df_HH$feature == "season", "Season",
                            ifelse(imp_df_HH$feature == "thermal", "Thermal period",
                            ifelse(imp_df_HH$feature == "dist_to_shoreline_m", "Distance to shoreline",
                            ifelse(imp_df_HH$feature == "fetch", "Fetch",
                            ifelse(imp_df_HH$feature == "zone", "Zone",
                                   imp_df_HH$feature)))))))


ordered <- imp_df_HH %>% arrange(desc(importance)) %>% distinct(feature_label)
ordered_fin <- ordered$feature_label
imp_df_HH$feature_label <- factor(imp_df_HH$feature_label, levels = rev(ordered_fin))

normalize <- function(x) (x - min(x)) / (max(x) - min(x))
imp_df_HH$importance_norm <- normalize(imp_df_HH$importance)

ggplot(imp_df_HH, aes(y = feature_label, x = importance_norm, fill = importance_norm)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "darkorchid1", high = "darkorchid4") +
  ylab("Feature") + xlab("Importance") + guides(fill = "none") +
  theme_bw() +
  theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

top_1_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[1, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_2_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[2, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_3_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[3, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_4_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[4, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_5_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[5, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_6_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[6, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_7_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[7, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)


ggplot(top_1_pdp_HH, aes(x = dist_to_shoreline_m, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("distance to shoreline (m)") + ylab(bquote("Marginal effect" ~ (hat(y))))+
    theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

ggplot(top_2_pdp_HH, aes(x = factor(season, levels = c("Spring", "Summer", "Winter", "Fall")), y = yhat)) +
  geom_col(fill = "purple", col = "purple4") +
  theme_bw() + xlab("Season") + ylab(bquote("Marginal effect" ~ (hat(y)))) +
  theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )


#NAS in the zone so need to go back to where this was assigned to PA dataframe to check whats missing

zone_levels <- c("Cootes Paradise", "Grindstone", "Carrolls Bay", "Bayfront area",
                 "West End", "North shore", "Central", "East End")

top_3_pdp_HH$zone <- as.character(top_3_pdp_HH$zone)
top_3_pdp_HH$zone[is.na(top_3_pdp_HH$zone)] <- "North shore"
top_3_pdp_HH$zone <- factor(top_3_pdp_HH$zone, levels = zone_levels)

ggplot(top_3_pdp_HH, aes(x = zone, y = yhat)) +
  geom_col(fill = "purple", col = "purple4") +
  theme_bw() + xlab("Zone") + ylab(bquote("Marginal effect" ~ (hat(y)))) +
  theme(
    axis.text.x  = element_text(size = 14, angle = 35, hjust = 1),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

ggplot(top_4_pdp_HH, aes(x = fetch, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("Fetch") + ylab(bquote("Marginal effect" ~ (hat(y))))+
      theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )


ggplot(top_5_pdp_HH, aes(x = depth_m, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("depth (m)") + ylab(bquote("Marginal effect" ~ (hat(y))))+
        theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )



ggplot(top_6_pdp_HH, aes(x = SAV, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("SAV %") + ylab(bquote("Marginal effect" ~ (hat(y))))+
          theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

###################################
## ---- Interaction strengths (sub-sampled) ----
set.seed(1234)
pa_data.train_sub_f <- pa_data.train[sample(nrow(pa_data.train),
                                            round(nrow(pa_data.train) * 0.25)), ]
X_HH_sub <- dplyr::select(pa_data.train_sub_f, -presence)

predictor_HH_sub <- Predictor$new(model_rf.Bin_HH, data = X_HH_sub,
                                  y = pa_data.train_sub_f$presence)

# keep the Interaction object separate from its plot
interact_all <- Interaction$new(predictor_HH_sub)
plot(interact_all) + ggtitle("Interaction strengths")

interact_results_HH <- interact_all$results %>%
  group_by(.feature) %>%
  summarise(H = mean(.interaction)) %>%
  rename(feature = .feature) %>%
  arrange(desc(H)) %>%
  as.data.frame()

# 2-way interactions with the strongest-interacting feature
interact_top_1_HH <- Interaction$new(predictor_HH_sub,
                                     feature = interact_results_HH$feature[1])

interact_top_1_HH_df <- interact_top_1_HH$results %>%
  group_by(.feature) %>%
  summarise(H = mean(.interaction)) %>%
  rename(feature = .feature) %>%
  arrange(desc(H)) %>%
  separate(feature, into = c("top1", "top2"), sep = ":") %>%
  as.data.frame()

interact_top_1_HH_df   # check this: top row is your strongest pair

library(purrr)
library(patchwork)   # install.packages("patchwork") if needed

season_lv   <- c("Spring", "Summer", "Fall", "Winter")
season_cols <- c(Spring = "#ffb3ba", Summer = "#a8e6cf",
                 Fall   = "#f9c8a0", Winter = "#85c1e9")

# the six predictors paired with season, in order of interaction strength
vars <- interact_top_1_HH_df$top1

# nicer axis labels
xlabs <- c(fetch = "Fetch", zone = "Zone", depth_m = "Depth (m)",
           dist_to_shoreline_m = "Distance to shoreline (m)",
           thermal = "Thermal period", SAV = "SAV %")

# compute all six 2-way PDPs
vars <- interact_top_1_HH_df$top1   # must exist before the loop

pdp_list <- purrr::map(purrr::set_names(vars), function(v) {
  pdp::partial(results_HH,
               pred.var        = c(v, "season"),
               prob            = TRUE,
               which.class     = 2,
               train           = pa_data.train,
               grid.resolution = 30,
               progress        = "none")
})


conflicted::conflict_prefer("map", "purrr")
conflicted::conflict_prefer("imap", "purrr")
conflicted::conflict_prefer("set_names", "purrr")
y_rng <- range(unlist(purrr::map(pdp_list, ~ range(.x$yhat))))

# shared y-range so panels are comparable (drop this to let each panel scale freely)
plots <- purrr::imap(pdp_list, ~ plot_pdp(.x, .y))

plot_pdp <- function(df, v) {
  df$season <- factor(df$season, levels = season_lv)

  base <- theme_bw() +
    theme(axis.text  = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.title = element_text(size = 14))

  if (is.numeric(df[[v]])) {
    # continuous predictor: one line per season
    ggplot(df, aes(x = .data[[v]], y = yhat, colour = season, group = season)) +
      geom_smooth(method="gam", alpha=0.2, linewidth=1.1) +
      scale_colour_manual(values = season_cols) +
      scale_fill_manual(values=season_cols)+
      labs(x = xlabs[[v]], y = bquote("Marginal effect" ~ (hat(y))),
           colour = "Season") +
      coord_cartesian(ylim = y_rng) +
      base
  } else {
    # categorical predictor: grouped bars, season as fill
    ggplot(df, aes(x = .data[[v]], y = yhat, fill = season)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = season_cols) +
      labs(x = xlabs[[v]], y = bquote("Marginal effect" ~ (hat(y))),
           fill = "Season") +
      coord_cartesian(ylim = c(0, y_rng[2])) +
      base +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
  }
}

plots <- imap(pdp_list, ~ plot_pdp(.x, .y))

# combined figure with one shared legend
wrap_plots(plots, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("pdp_season_interactions.png", width = 11, height = 12, dpi = 300)

#top 3 two-way interaction plots as grids 
#could you add season as a colour?


#next will be building the predictive raster layer 
#first need to figure out whats going on with zone OR drop it 
#starting at page 17 of Griffin et al. appendix data sheet 1

library(patchwork)

season_lv <- c("Spring", "Summer", "Fall", "Winter")
zone_levels <- c("Cootes Paradise", "Grindstone", "Carrolls Bay", "Bayfront area",
                 "West End", "North shore", "Central", "East End")

xlabs <- c(fetch = "Fetch", zone = "Zone", depth_m = "Depth (m)",
           dist_to_shoreline_m = "Distance to shoreline (m)",
           thermal = "Thermal period", SAV = "SAV %")

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

#temporal blocking ?? i.e. spring/summer/winter/fall or thermocline isoline?



##############################
## ---- 1. AUC: test set vs spatial CV ----
auc_test <- mlr::performance(pred_HH, measures = mlr::auc)
auc_test

tune_HH$y          # spatial CV AUC for the best hyperparameters (auc.test.mean)

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
