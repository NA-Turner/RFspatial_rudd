####
#second Rudd RF model attempt
#adding month as a predictor variable 

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
PA_rand_w_depthSAVfetch$thermal<-base::as.factor(PA_rand_w_depthSAVfetch$thermal)
PA_rand_w_depthSAVfetch$season<-base::as.factor(PA_rand_w_depthSAVfetch$season)
PA_rand_w_depthSAVfetch$presence<-base::as.factor(PA_rand_w_depthSAVfetch$presence)

#change month from a numbers to words 
PA_rand_w_depthSAVfetch <- PA_rand_w_depthSAVfetch %>% 
  mutate(month = month.name[month_num])

#################################
#subset the dataframe, keep only predictors you want

pa_data <- PA_rand_w_depthSAVfetch %>%
  sf::st_drop_geometry() %>%
  dplyr::select(presence, depth_m, SAV, zone, season, fetch, shoreline_distance_m, rand_long, rand_lat) %>%
  mutate(
    presence = factor(presence, levels = c(0, 1), labels = c("abs", "pres")),
    rowID    = seq_len(n())
  )


pa_data$zone<-base::as.factor(pa_data$zone)

#split into train and test data
#change % split right now at 70% train 
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




##older code from original run through 

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
model_rf.Bin_HH <- mlr::train(lrn_rf.Bin, task.train)
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
                            ifelse(imp_df_HH$feature == "shoreline_distance_m", "Distance to shoreline",
                            ifelse(imp_df_HH$feature == "fetch", "Fetch",
                            ifelse(imp_df_HH$feature == "zone", "Zone",
                                   imp_df_HH$feature))))))


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

ggplot(top_3_pdp_HH, aes(x = fetch, y = yhat)) +
  geom_smooth(fill = "purple", method= "purple4") +
  theme_bw() + xlab("Fetch") + ylab(bquote("Marginal effect" ~ (hat(y)))) +
  theme(
    axis.text.x  = element_text(size = 14, angle = 35, hjust = 1),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )


top_4_pdp_HH$month <- factor(top_4_pdp_HH$month)

ggplot(top_3_pdp_HH, aes(x = fetch, y = yhat)) +
 geom_smooth(col = "purple", method= "gam") +
  theme_bw() + xlab("Fetch") + ylab(bquote("Marginal effect" ~ (hat(y))))+
      theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

ggplot(top_4_pdp_HH, aes(x = factor(month, levels = c("January", "February", "March", "April", "May", "June", 
"July", "August", "September", "October", "November", "December")), y = yhat)) +
  geom_col(fill = "purple", col = "purple4") +
  theme_bw() + xlab("Month") + ylab(bquote("Marginal effect" ~ (hat(y)))) +
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


zone_levels <- c("Cootes Paradise", "Grindstone", "Carrolls Bay", "Bayfront area",
                 "West End", "North shore", "Central", "East End")

top_7_pdp_HH$zone <- factor(top_7_pdp_HH$zone)

ggplot(top_7_pdp_HH, aes(x = factor(zone, levels = c("Cootes Paradise", "Bayfront area", "Carrolls Bay", 
"Grindstone ", "West End", "Central", "North shore", "East End")), y = yhat)) +
  geom_col(fill = "purple", col = "purple4") +
  theme_bw() + xlab("Zone") + ylab(bquote("Marginal effect" ~ (hat(y)))) +
  theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )






####testing model fits 
#with thermal and zone dropped season inlcuded


library(pROC)

p_c <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)

logloss_val <- -mean(obs[ok] * base::log(p_c) + (1 - obs[ok]) * base::log(1 - p_c))

rf <- results_HH                          # underlying ranger object for this model
y  <- pa_data.train$presence              # must match the rows the model was trained on
obs <- as.numeric(y == "pres")
p   <- rf$predictions[, "pres"]           # OOB probabilities
ok  <- !is.na(p)

roc_obj <- pROC::roc(obs[ok], p[ok], quiet = TRUE, direction = "<")
ci      <- pROC::ci.auc(roc_obj, method = "delong")

ths <- seq(0.05, 0.95, by = 0.01)
tss <- sapply(ths, function(th) {
  pred <- p[ok] >= th
  mean(pred[obs[ok] == 1]) + mean(!pred[obs[ok] == 0]) - 1
})

p_c <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
oob_one <- data.frame(
  model     = "no_zone",                  # change the label for each model
  n_pred    = rf$num.independent.variables,
  mtry      = rf$mtry,
  min_node  = rf$min.node.size,
  n_trees   = rf$num.trees,
  auc_oob   = as.numeric(pROC::auc(roc_obj)),
  auc_lo    = ci[1],
  auc_hi    = ci[3],
  brier_oob = mean((obs[ok] - p[ok])^2),
  rmse_oob  = sqrt(mean((obs[ok] - p[ok])^2)),
  logloss   = logloss_val,
  tss_max   = max(tss),
  best_thr  = ths[which.max(tss)]
)
oob_one





















                               ##############################
#model tuning 
tune_rf <- function(train_df, coords_train, folds = NULL,
                    target = "presence", positive = "pres",
                    iters = 5, reps = 3, maxit = 100L, num.trees = 1000,
                    sf_range = c(0.2, 0.9), nodesize_range = c(1, 10),
                    unordered = "ignore",     # or "order"
                    measure = mlr::auc,       # or mlr::brier
                    seed = 1234) {

  task <- makeClassifTask(data = train_df, target = target,
                          positive = positive, coordinates = coords_train)

  set.seed(seed)
  if (is.null(folds)) {
    rin <- makeResampleInstance(
      makeResampleDesc("SpRepCV", folds = iters, reps = reps), task)
  } else {
    # custom folds: list of list(train_idx, test_idx), indices into train_df
    rin <- makeResampleInstance(
      makeResampleDesc("CV", iters = length(folds)), task)
    rin$train.inds <- lapply(folds, `[[`, 1)
    rin$test.inds  <- lapply(folds, `[[`, 2)
  }

  fold_counts <- t(sapply(rin$test.inds,
                          function(i) table(getTaskTargets(task)[i])))
  print(fold_counts)
  if (any(fold_counts == 0)) warning("A fold has no presences or no absences.")

  lrn <- makeLearner("classif.ranger", predict.type = "prob",
                     num.trees = num.trees, respect.unordered.factors = unordered)
  ps <- makeParamSet(
    makeIntegerParam("mtry", lower = 1, upper = ncol(train_df) - 1),
    makeNumericParam("sample.fraction", lower = sf_range[1], upper = sf_range[2]),
    makeIntegerParam("min.node.size", lower = nodesize_range[1], upper = nodesize_range[2])
  )

  set.seed(seed)
  tuned <- tuneParams(lrn, task, resampling = rin, par.set = ps,
                      control = makeTuneControlRandom(maxit = maxit),
                      measures = measure, show.info = FALSE)

  final_lrn <- setHyperPars(
    makeLearner("classif.ranger", importance = "impurity",
                predict.type = "prob", num.trees = num.trees,
                respect.unordered.factors = unordered),
    par.vals = tuned$x)
  set.seed(seed)
  model <- train(final_lrn, task)

  list(task = task, tuned = tuned, model = model, fold_counts = fold_counts,
       opt_path = as.data.frame(tuned$opt.path))
}




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



##checking model for spatial autocorrelation 
library(spdep)
library(gstat)


obs   <- as.numeric(pa_data.train$presence == "pres")
p_oob <- results_HH$predictions[, "pres"]

resid_sf <- st_as_sf(
  data.frame(resid = obs - p_oob,
             x = coords.train$rand_long, y = coords.train$rand_lat),
  coords = c("x", "y"), crs = 4326) |>
  st_transform(32617)          # UTM 17N, so distances are in metres
xy <- st_coordinates(resid_sf)

# Duplicate coordinates can break the neighbour search
sum(duplicated(xy))

# a) Global Moran's I (permutation test)
lw <- nb2listw(knn2nb(knearneigh(xy, k = 8)), style = "W")
set.seed(1234)
moran.mc(resid_sf$resid, lw, nsim = 999)

# b) Correlogram: Moran's I by distance band (edit breaks to your extent)
breaks <- c(0, 100, 250, 500, 1000, 2000, 3000)
cor_df <- do.call(rbind, lapply(seq_along(breaks)[-1], function(i) {
  lw_d <- nb2listw(dnearneigh(xy, breaks[i - 1], breaks[i]),
                   style = "W", zero.policy = TRUE)
  mt <- moran.test(resid_sf$resid, lw_d, zero.policy = TRUE)
  data.frame(upper = breaks[i], I = unname(mt$estimate[1]), p = mt$p.value)
}))
cor_df
ggplot(cor_df, aes(upper, I)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_line() +
  geom_point(aes(colour = p < 0.05)) +
  labs(x = "Distance (m)", y = "Moran's I of residuals")

# c) Variogram
vg <- variogram(resid ~ 1, resid_sf, cutoff = 3000, width = 100)
vg_fit <- fit.variogram(vg, vgm(0.7 * max(vg$gamma), "Sph", 1000, min(vg$gamma)))
plot(vg, vg_fit); vg_fit

# d) Map the residuals
ggplot(cbind(resid_sf, xy), aes(X, Y, colour = resid)) +
  geom_point(size = 0.8) +
  scale_colour_gradient2(low = "blue", mid = "grey90", high = "red") +
  coord_equal()


library(blockCV)
library(pROC)

best <- tune_HH$x
dat  <- cbind(pa_data.train, coords.train)
sf_d <- st_as_sf(dat, coords = c("rand_long", "rand_lat"), crs = 4326, remove = FALSE) |>
  st_transform(32617)

block_auc <- function(size_m, k = 5, seed = 1234) {
  set.seed(seed)
  cb <- cv_spatial(x = sf_d, column = "presence", k = k, size = size_m,
                   selection = "random", iteration = 50,
                   plot = FALSE, report = FALSE, progress = FALSE)
  aucs <- sapply(cb$folds_list, function(f) {
    tr <- pa_data.train[f[[1]], ]; te <- pa_data.train[f[[2]], ]
    rf <- ranger(presence ~ ., data = tr, num.trees = 1000, probability = TRUE,
                 mtry = best$mtry, sample.fraction = best$sample.fraction,
                 min.node.size = best$min.node.size)
    as.numeric(pROC::auc(te$presence,
                         predict(rf, te)$predictions[, "pres"], quiet = TRUE))
  })
  c(size_m = size_m, mean_auc = mean(aucs), sd_auc = sd(aucs), min_auc = min(aucs))
}

do.call(rbind, lapply(c(250, 500, 1000, 2000, 5000), block_auc))

###next
sizes <- c(250, 500, 750, 1000, 1500, 2000, 3000)
out <- do.call(rbind, lapply(sizes, function(s)
  do.call(rbind, lapply(1:5, function(sd)
    block_auc(s, seed = sd)))))
agg <- aggregate(cbind(mean_auc, min_auc) ~ size_m, as.data.frame(out), mean)
agg

km_folds <- makeResampleInstance(makeResampleDesc("SpCV", iters = 5), res$task)$test.inds
sapply(km_folds, function(i) table(pa_data.train$zone[i]))

d <- data.frame(pa_data.train, obs = obs, p = p_oob)
sapply(split(d, d$zone), function(x)
  tryCatch(as.numeric(pROC::auc(x$obs, x$p, quiet = TRUE)), error = function(e) NA))
sapply(split(d, d$season), function(x) as.numeric(pROC::auc(x$obs, x$p, quiet = TRUE)))

#leave one zone out one zone in 
best <- tune_HH$x
zones <- levels(pa_data.train$zone)

loz <- function(drop_zone_var = FALSE) {
  dat <- if (drop_zone_var) pa_data.train[, names(pa_data.train) != "zone"] else pa_data.train
  do.call(rbind, lapply(zones, function(z) {
    te_i <- which(pa_data.train$zone == z)
    tr <- dat[-te_i, ]; te <- dat[te_i, ]
    if (length(unique(te$presence)) < 2) return(NULL)
    rf <- ranger(presence ~ ., data = tr, num.trees = 1000, probability = TRUE,
                 mtry = min(best$mtry, ncol(tr) - 1),
                 sample.fraction = best$sample.fraction,
                 min.node.size = best$min.node.size)
    data.frame(zone = z, n = nrow(te), n_pres = sum(te$presence == "pres"),
               auc = as.numeric(pROC::auc(te$presence,
                       predict(rf, te)$predictions[, "pres"], quiet = TRUE)))
  }))
}

with_zone    <- loz(FALSE)
without_zone <- loz(TRUE)
merge(with_zone, without_zone, by = c("zone", "n", "n_pres"),
      suffixes = c("_with_zone", "_no_zone"))

##################
##run next for zone issue
best <- tune_HH$x
no_zone <- pa_data.train[, names(pa_data.train) != "zone"]

cv_auc <- function(data, folds, ...) {
  sapply(folds, function(f) {
    tr <- data[f[[1]], ]; te <- data[f[[2]], ]
    rf <- ranger(presence ~ ., data = tr, num.trees = 1000, probability = TRUE,
                 mtry = min(best$mtry, ncol(tr) - 1),
                 sample.fraction = best$sample.fraction,
                 min.node.size = best$min.node.size, ...)
    as.numeric(pROC::auc(te$presence,
                         predict(rf, te)$predictions[, "pres"], quiet = TRUE))
  })
}

out <- do.call(rbind, lapply(1:5, function(sd) {
  set.seed(sd)
  cb <- cv_spatial(x = sf_d, column = "presence", k = 5, size = 1000,
                   selection = "random", iteration = 100,
                   plot = FALSE, report = FALSE, progress = FALSE)
  data.frame(seed = sd,
    zone_default = mean(cv_auc(pa_data.train, cb$folds_list)),
    zone_ordered = mean(cv_auc(pa_data.train, cb$folds_list,
                               respect.unordered.factors = "order")),
    no_zone      = mean(cv_auc(no_zone, cb$folds_list)))
}))
colMeans(out[-1])



# ---- Evaluation and importance (unchanged) ----
set.seed(1234)
pred_HH <- predict(model_rf.Bin_HH, task = task.test)
(performance_HH <- calculateROCMeasures(pred_HH))

X_HH <- pa_data.train[which(names(pa_data.train) != "presence")]
predictor_HH <- Predictor$new(model_rf.Bin_HH, data = X_HH, y = pa_data.train$presence)
imp_HH <- FeatureImp$new(predictor_HH, loss = "ce")
imp_df_HH <- imp_HH$results

performance(pred_HH, measures = list(mlr::auc, mlr::brier, mlr::logloss))

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



ggplot(top_3_pdp_HH, aes(x = fetch, y = yhat)) +
  geom_smooth(method="gam", col = "purple4") +
  theme_bw() + xlab("fetch") + ylab(bquote("Marginal effect" ~ (hat(y)))) +
  theme(
    axis.text.x  = element_text(size = 14, angle = 35, hjust = 1),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

ggplot(top_4_pdp_HH, aes(x = month, y = yhat)) +
  geom_col(col = "purple") +
  theme_bw() + xlab("Month") + ylab(bquote("Marginal effect" ~ (hat(y))))+
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
  theme_bw() + xlab("sav (%)") + ylab(bquote("Marginal effect" ~ (hat(y))))+
          theme(
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

library(dplyr); library(tidyr); library(ggplot2)
library(iml); library(pdp); library(purrr); library(patchwork)

# ---- Subsample of the data the model was trained on ----
set.seed(1234)
sub_f <- pa_data.train[sample(nrow(pa_data.train),
                              round(nrow(pa_data.train) * 0.25)), ]
X_sub <- dplyr::select(sub_f, -presence)

predictor_sub <- Predictor$new(model_rf.Bin_HH, data = X_sub, y = sub_f$presence)

# ---- Overall interaction strengths ----
interact_all <- Interaction$new(predictor_sub)
plot(interact_all) + ggtitle("Interaction strengths")

# ---- 2-way interactions with season ----
interact_season <- Interaction$new(predictor_sub, feature = "season")

interact_season_df <- interact_season$results %>%
  group_by(.feature) %>%
  summarise(H = mean(.interaction), .groups = "drop") %>%
  arrange(desc(H)) %>%
  separate(.feature, into = c("a", "b"), sep = ":") %>%
  mutate(partner = ifelse(a == "season", b, a)) %>%
  as.data.frame()
interact_season_df

# ---- Plot settings ----
print(levels(factor(pa_data.train$season)))   # check these match season_lv below

season_lv   <- c("Spring", "Summer", "Fall", "Winter")
season_cols <- c(Spring = "#ffb3ba", Summer = "#a8e6cf",
                 Fall   = "#f9c8a0", Winter = "#85c1e9")

xlabs <- c(fetch = "Fetch", zone = "Zone", month = "Month",
           depth_m = "Depth (m)", dist_to_shoreline_m = "Distance to shoreline (m)",
           SAV = "SAV %")
lab <- function(v) if (v %in% names(xlabs)) xlabs[[v]] else v

vars <- interact_season_df$partner
vars <- vars[vars %in% names(X_sub) & vars != "season"]

# ---- 2-way PDPs ----
pdp_list <- purrr::map(purrr::set_names(vars), function(v) {
  pdp::partial(results_HH,
               pred.var        = c(v, "season"),
               prob            = TRUE,
               which.class     = 2,
               train           = X_sub,
               grid.resolution = 30,
               progress        = "none")
})

y_rng <- range(unlist(purrr::map(pdp_list, ~ range(.x$yhat))))

plot_pdp <- function(df, v) {
  df$season <- factor(df$season, levels = season_lv)
  base <- theme_bw() +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.text = element_text(size = 12), legend.title = element_text(size = 14))

  if (is.numeric(df[[v]])) {
    ggplot(df, aes(x = .data[[v]], y = yhat, colour = season, group = season)) +
      geom_line(linewidth = 1.1) +
      scale_colour_manual(values = season_cols, name = "Season") +
      labs(x = lab(v), y = bquote("Marginal effect" ~ (hat(y)))) +
      coord_cartesian(ylim = y_rng) + base
  } else {
    ggplot(df, aes(x = .data[[v]], y = yhat, fill = season)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = season_cols, name = "Season") +
      labs(x = lab(v), y = bquote("Marginal effect" ~ (hat(y)))) +
      coord_cartesian(ylim = c(0, y_rng[2])) + base +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
  }
}

plots <- purrr::imap(pdp_list, ~ plot_pdp(.x, .y))

wrap_plots(plots, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
