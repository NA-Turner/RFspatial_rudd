
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

#hamilton harbour polgyon file


shorelinemap <- read_sf("01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp")
shorelinemap <- st_transform(shorelinemap, crs = 4326)


HH_plot<-ggplot(shorelinemap) +
 geom_sf(fill = "lightblue", color = "black", size = 0.5) +
 theme_minimal() +
 theme(axis.text = element_text(size = 8))
#use glatos rpackage to summarize detections by num_fish and num_dets

HH_plot

#presence absence locations
# PA_rand_w_depthSAV

#raster layers
#SAV
SAV2026_raster <- rast("01_data/04_shapefiles/Enviro layers/SAVM2026/SAVM_202608.tif")
plot(SAV2026_raster)
#depth 
depthrast<-rast("01_data/04_shapefiles/Enviro layers/WL.tif")
plot(depthrast)
#fetch
fetct2026_raster <- rast("01_data/04_shapefiles/Enviro layers/SAVM2026/fetch2026.tif")
plot(fetct2026_raster)

#PA data frame 
PA_rand_w_depthSAVfetch<-readRDS("01_data/02_processed_files/PA_rand_w_depthSAVfetch.rds")
# Row ID for indexing
PA_rand_w_depthSAVfetch$rowID <- seq_len(nrow(PA_rand_w_depthSAVfetch))

set.seed(1234)

levels(as.factor(PA_rand_w_depthSAVfetch$presence))
PA_rand_w_depthSAVfetch$thermal<-as.factor(PA_rand_w_depthSAVfetch$thermal)
PA_rand_w_depthSAVfetch$season<-as.factor(PA_rand_w_depthSAVfetch$season)
PA_rand_w_depthSAVfetch$presence<-as.factor(PA_rand_w_depthSAVfetch$presence)

#add 'zones' which will be the same rec groupings we did for goldfish 
PA_rand_w_depthSAVfetch$zone

PA_rand_w_depthSAVfetch$zone <- case_when(
  PA_rand_w_depthSAVfetch$station %in% c("HAM-053", "HAM-051","HAM-036","HAM-082", "HAM-081") ~ "Bayfront area",
  PA_rand_w_depthSAVfetch$station %in% c("HAM-030", "HAM-042", "HAM-044", "HAM-096", "HAM-098", "HAM-095", "HAM-094", "HAM-093", 
  "HAM-088", "HAM-090","HAM-091", "HAM-089", "HAM-030", "HAM-042") ~ "Cootes Paradise",
  PA_rand_w_depthSAVfetch$station %in% c( "HAM-033", "HAM-059", "HAM-060", "HAM-061", "HAM-062","HAM-066", "HAM-067", "HAM-068", "HAM-087") ~ "Grindstone",
  PA_rand_w_depthSAVfetch$station %in% c("HAM-048", "HAM-018", "HAM-028", "HAM-037", "HAM-072", "HAM-073", "HAM-074", "HAM-075", "HAM-076", "HAM-079", "HAM-078", 
                               "HAM-077", "HAM-085", "HAM-084", "HAM-083", "HAM-005", "HAM-011", "HAM-027", "HAM-023", "HAM-080") ~ "West End",
  PA_rand_w_depthSAVfetch$station %in% c("HAM-035") ~ "East End",
  PA_rand_w_depthSAVfetch$station %in% c("HAM-013", "HAM-003", "HAM-002", "HAM-012", "HAM-045", "HAM-025", "HAM-058", "HAM-009", "HAM-008", "HAM-034", "HAM-035") ~ "East End",
  PA_rand_w_depthSAVfetch$station %in% c("HAM-046", "HAM-047", "HAM-007", "HAM-015", "HAM-001") ~ "North shore",
  PA_rand_w_depthSAVfetch$station %in% c("HAM-021", "HAM-004", "HAM-017", "HAM-039", "HAM-041") ~ "Central",
  PA_rand_w_depthSAVfetch$station %in% c("HAM-070", "HAM-099" ,"HAM-071", "HAM-063", "HAM-032", "HAM-043", "HAM-029") ~ "Carrolls Bay",
  TRUE ~ NA_character_ # Default to NA if no match
)

#distance to shoreline code

# ===============================
# 1. Distance to Shoreline
# ===============================

# Convert shoreline polygon to boundary
shoreline_boundary <- st_boundary(shorelinemap)
# Calculate distance from points to shoreline boundary
PA_rand_sf <- st_as_sf(
  PA_rand_w_depthSAVfetch,
  coords = c("rand_long", "rand_lat"),
  crs = 4326,        # <- set this to your actual CRS (e.g. 4326 if lat/long WGS84)
  remove = FALSE      # keep rand_long/rand_lat as regular columns too
)
PA_rand_sf <- st_transform(PA_rand_sf, st_crs(shoreline_boundary))

dist_to_shore <- st_distance(PA_rand_sf, shoreline_boundary)
# Extract distances
PA_rand_w_depthSAVfetch$dist_to_shoreline_m <- as.numeric(dist_to_shore[,1])
colnames(PA_rand_w_depthSAVfetch)



#####distance to shroeline and zone area added
saveRDS(PA_rand_w_depthSAVfetch, file = "01_data/02_processed_files/PA_rand_w_depthSAVfetchDISTtoshoreZone.rds")



colnames(PA_rand_w_depthSAVfetch)

pa_data <- PA_rand_w_depthSAVfetch %>%
  sf::st_drop_geometry() %>%
  dplyr::select(presence, depth_m, SAV, season, thermal, zone, depth_m, fetch, dist_to_shoreline_m, rand_long, rand_lat) %>%
  mutate(
    presence = factor(presence, levels = c(0, 1), labels = c("abs", "pres")),
    rowID    = seq_len(n())
  )
pa_data$zone<-as.factor(pa_data$zone)


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
  ylab("Feature") + xlab("Importance") + guides(fill = "none") + theme_bw()

top_1_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[1, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_2_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[2, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_3_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[3, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_4_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[4, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_6_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[6, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)
top_7_pdp_HH <- pdp::partial(results_HH, pred.var = imp_df_HH[7, 1], prob = TRUE,
                              train = pa_data.train, progress = "text", which.class = 2)


ggplot(top_1_pdp_HH, aes(x = dist_to_shoreline_m, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("distance to shoreline (m)") + ylab(bquote("Marginal effect" ~ (hat(y))))

ggplot(top_3_pdp_HH, aes(x = fetch, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("fetch") + ylab(bquote("Marginal effect" ~ (hat(y))))

ggplot(top_4_pdp_HH, aes(x = depth_m, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("depth (m)") + ylab(bquote("Marginal effect" ~ (hat(y))))

ggplot(top_4_pdp_HH, aes(x = depth_m, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("depth (m)") + ylab(bquote("Marginal effect" ~ (hat(y))))

ggplot(top_6_pdp_HH, aes(x = SAV, y = yhat)) +
  geom_smooth(col = "purple", method = "gam") +
  theme_bw() + xlab("SAV %") + ylab(bquote("Marginal effect" ~ (hat(y))))

#someth9ing going on with zone its all .5 across the board
ggplot(top_7_pdp_HH, aes(x = zone, y = yhat)) +
stat_summary(fun = mean, geom = "bar", fill = "purple", col = "black", alpha = 0.7) +
stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
theme_bw() +
xlab("Zone") +
ylab(bquote("Marginal effect" ~ (hat(y)))) +
theme(
axis.text.x = element_text(color = "black", angle = 35, vjust = 1, hjust = 1),
axis.text.y = element_text(color = "black")) 


#calculate interaction strenghts across all predictors and plot marginal effects of the top two-way interaction 
#Sub-sample and re-run model to reduce computational times when
# determining the top two-way interaction variables.
set.seed(1234)
# Sub-sample and re-run model to reduce computational times when
# determining the top two-way interaction variables.
set.seed(1234)
pa_data.train_sub <- pa_data.train %>% mutate(rowID = seq(1:nrow(pa_data.train)))
pa_data.train_sub_f <- pa_data.train_sub[sample(1:nrow(pa_data.train_sub),
                                                  nrow(pa_data.train_sub) * 0.25, replace = FALSE), ]
X_HH_sub <- pa_data.train_sub_f %>% dplyr::select(-c(presence, rowID))

# Create 'Predictor' object to interpret findings via the iml package.
predictor_HH_sub <- Predictor$new(model_rf.Bin_HH, data = X_HH_sub,
                                   y = pa_data.train_sub_f$presence)
# Create 'Predictor' object to interpret findings via the iml
# package.
predictor_rudd_sub <- Predictor$new(model_rf.Bin_HH, data = X_HH_sub,
y = pa_data.train_sub_f$presf)

interact_HH_sub <- Interaction$new(predictor_HH_sub) %>% plot() +
  ggtitle("Interaction strengths")

interact_HH_sub

(interact_results_HH <- interact_HH_sub$data %>%
   group_by(.feature) %>%
   summarise(H = mean(.interaction)) %>%
   rename(feature = .feature) %>%
   as.data.frame() %>%
   arrange(desc(H)))

  (interact_top_1_HH <- iml::Interaction$new(predictor_HH_sub,
                                            feature = interact_results_HH$feature[1]))

(interact_top_1_HH_df <- interact_top_1_HH$results %>%
   group_by(.feature) %>%
   summarise(H = mean(.interaction)) %>%
   rename(feature = .feature) %>%
   as.data.frame() %>%
   arrange(desc(H)) %>%
   separate(col = feature, into = c("top1", "top2"), sep = "\\:"))


#season dist to shoreline
summary(pa_data.train$dist_to_shoreline_m)

top_int_1_pdp_df_HH <- results_HH %>%
  pdp::partial(pred.var = c(interact_top_1_HH_df$top1[1], interact_top_1_HH_df$top2[1]),
               prob = TRUE, which.class = 2,
               progress = "text", train = pa_data.train)

top_int_1_pdp_df_HH$Variable <- paste(top_int_1_pdp_df_HH$season, top_int_1_pdp_df_HH$dist_to_shoreline_m)
top_int_1_pdp_df_HH$Inter_Variable <- "Season_DistShoreline"


top_int_1_pdp_df_HH_bins_Dist <- cut(
  top_int_1_pdp_df_HH$dist_to_shoreline_m,
  breaks = c(0, 500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000),
  labels = c("0-500", "500-1000", "1-2km", "2-3km", "3-4km", "4-5km",
             "5-6km", "6-7km", "7-8km", "8-9km", "9-10km", "10-11km", "11-12km", "12-13km")
)
top_int_1_pdp_df_HH$bins_Dist <- top_int_1_pdp_df_HH_bins_Dist

breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000, 13000)
labels = c("0-2km", "2-4km", "4-6km", "6-8km", "8-10km", "10-12km", "12-13km")
top_int_1_pdp_df_HH$bins_Dist <- top_int_1_pdp_df_HH_bins_Dist

{
  top_int_1_pdp_df_HH_mean <-
    top_int_1_pdp_df_HH %>%
    data.frame() %>%
    group_by(bins_Dist, season) %>%
    summarise(ci = list(mean_cl_normal(yhat) %>%
                           rename(mean = y, lwr = ymin, upr = ymax))) %>%
    unnest() %>%
    mutate(variance = (upr - lwr))

  ggplot(top_int_1_pdp_df_HH_mean, aes(season, bins_Dist)) +
    geom_tile(aes(fill = mean), width = 1, height = .9) +
    scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 1, by = 1)) +
    theme_classic() +
    xlab("Season") +
    ylab("Distance to shoreline (m)") +
    theme(
      axis.text.y = element_text(colour = "black"),
      axis.text.x = element_text(angle = 35, hjust = 1, colour = "black"),
      strip.text = element_text(face = "bold", size = 8, lineheight = 5.0),
      legend.position = "right"
    ) +
    labs(fill = "Mean Marginal effect" ~ (hat(y))) 
}


#next will be building the predictive raster layer 
#first need to figure out whats going on with zone OR drop it 
#starting at page 17 of Griffin et al. appendix data sheet 1