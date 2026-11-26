#comparing two random forest models 

###model 1
#all predictor varibales included, including correlated ones


##model 2:
#depth removed a highly correlated predictor variable

library(mlr)

drop_var <- "depth_m"   # CHANGE to the correlated predictor you removed

# ---- same train/test rows for both models ----
set.seed(1234)
train_id <- sample(seq_len(nrow(pa_data)), floor(nrow(pa_data) * 0.7))

full_vars <- c("depth_m", "SAV", "season", "zone", "fetch", "shoreline_distance_m")
red_vars  <- setdiff(full_vars, drop_var)

make_task <- function(rows, vars) {
  d <- as.data.frame(pa_data[rows, ])
  makeClassifTask(data = d[, c("presence", vars)], target = "presence",
                  positive = "pres",
                  coordinates = d[, c("rand_long", "rand_lat")])
}

test_id <- setdiff(seq_len(nrow(pa_data)), train_id)

# ---- same spatial folds for both models ----
base_task <- make_task(train_id, full_vars)

set.seed(1234)
tune_folds <- makeResampleInstance(makeResampleDesc("SpCV", iters = 5),
                                   task = base_task)
set.seed(42)
comp_folds <- makeResampleInstance(
  makeResampleDesc("SpRepCV", folds = 5, reps = 20), task = base_task)

m <- list(mlr::auc, mlr::logloss, mlr::brier)

# ---- identical pipeline for each predictor set ----
run_model <- function(vars) {
  task.train <- make_task(train_id, vars)
  task.test  <- make_task(test_id, vars)

  lrn <- makeLearner("classif.ranger", predict.type = "prob",
                     num.trees = 2000, importance = "impurity")

  ps <- makeParamSet(
    makeIntegerParam("mtry", lower = 1, upper = length(vars)),
    makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
    makeIntegerParam("min.node.size", lower = 1, upper = 10)
  )

  set.seed(1234)
  tuned <- tuneParams(lrn, task = task.train, resampling = tune_folds,
                      par.set = ps, measures = mlr::auc,
                      control = makeTuneControlRandom(maxit = 50L))
  lrn <- setHyperPars(lrn, par.vals = tuned$x)

  set.seed(1234)
  cv <- mlr::resample(lrn, task.train, comp_folds, measures = m)

  set.seed(1234)
  model <- mlr::train(lrn, task.train)
  pred  <- predict(model, task = task.test)

  list(tuned = tuned, cv = cv, model = model, pred = pred,
       test = mlr::performance(pred, measures = m))
}

full <- run_model(full_vars)
red  <- run_model(red_vars)

# ---- compare ----
rbind(full = full$cv$aggr, reduced = red$cv$aggr)    # spatial CV
rbind(full = full$test, reduced = red$test)          # test set

for (met in c("auc", "logloss", "brier")) {
  d <- full$cv$measures.test[[met]] - red$cv$measures.test[[met]]
  cat("\n", met, "(full - reduced)\n")
  print(summary(d))
}

rbind(full = unlist(full$tuned$x), reduced = unlist(red$tuned$x))

