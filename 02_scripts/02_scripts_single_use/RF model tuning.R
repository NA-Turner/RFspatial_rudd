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
PA_rand_w_depthSAVfetch$thermal<-as.factor(PA_rand_w_depthSAVfetch$thermal)
PA_rand_w_depthSAVfetch$season<-as.factor(PA_rand_w_depthSAVfetch$season)
PA_rand_w_depthSAVfetch$presence<-as.factor(PA_rand_w_depthSAVfetch$presence)

#change month from a numbers to words 
PA_rand_w_depthSAVfetch <- PA_rand_w_depthSAVfetch %>% 
  mutate(month_name = month.name[month_num])

#################################
#subset the dataframe, keep only predictors you want

pa_data <- PA_rand_w_depthSAVfetch %>%
  sf::st_drop_geometry() %>%
  dplyr::select(presence, depth_m, SAV,month_name, season, thermal, fetch, dist_to_shoreline_m, rand_long, rand_lat) %>%
  mutate(
    presence = factor(presence, levels = c(0, 1), labels = c("abs", "pres")),
    rowID    = seq_len(n())
  )
pa_data$zone<-as.factor(pa_data$zone)
pa_data$month_name<-as.factor(pa_data$month_name)

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


library(mlr)
library(ranger)

tune_rf <- function(train_df, coords_train,
                    target = "presence", positive = "pres",
                    iters = 5, maxit = 100L, num.trees = 1000,
                    sf_range = c(0.2, 0.9), nodesize_range = c(1, 10),
                    seed = 1234) {

  task <- makeClassifTask(data = train_df, target = target,
                          positive = positive, coordinates = coords_train)

  # 1. Check class counts in each spatial fold before spending time tuning
  set.seed(seed)
  rdesc <- makeResampleDesc("SpCV", iters = iters)
  rin   <- makeResampleInstance(rdesc, task)
  fold_counts <- t(sapply(rin$test.inds,
                          function(i) table(getTaskTargets(task)[i])))
  print(fold_counts)
  if (any(fold_counts == 0))
    warning("A fold has no presences or no absences; reduce iters.")

  # 2. Learner and search space scaled to this dataset
  lrn <- makeLearner("classif.ranger", predict.type = "prob",
                     num.trees = num.trees)
  ps <- makeParamSet(
    makeIntegerParam("mtry", lower = 1, upper = ncol(train_df) - 1),
    makeNumericParam("sample.fraction", lower = sf_range[1], upper = sf_range[2]),
    makeIntegerParam("min.node.size", lower = nodesize_range[1],
                     upper = nodesize_range[2])
  )

  # 3. Tune with random search under spatial CV
  set.seed(seed)
tuned <- tuneParams(lrn, task, resampling = rin, par.set = ps,
                    control = makeTuneControlRandom(maxit = maxit),
                    measures = mlr::auc, show.info = FALSE)

  # 4. Final model on the full training set
  final_lrn <- setHyperPars(
    makeLearner("classif.ranger", importance = "impurity",
                predict.type = "prob", num.trees = num.trees),
    par.vals = tuned$x)
  set.seed(seed)
  model <- train(final_lrn, task)

  list(task = task, tuned = tuned, model = model, fold_counts = fold_counts,
       opt_path = as.data.frame(tuned$opt.path))
}

# Usage (same prep as before: factor target, coords split off, columns dropped)
res <- tune_rf(pa_data.train, coords.train)
res$tuned$x       # best hyperparameters
res$tuned$y       # best spatial-CV AUC

res <- tune_rf(pa_data.train, coords.train)

tune_HH         <- res$tuned
model_rf.Bin_HH <- res$model
results_HH      <- mlr::getLearnerModel(model_rf.Bin_HH)



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
