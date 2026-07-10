library(mlr3proba)
library(mlr3extralearners)
library(mlr3pipelines)

task = tsk("lung")

lrn_xgb <- po("encode") %>>%
  lrn("surv.xgboost.cox", nrounds = 500, max_depth = 3) |>
  as_learner()

lrn_xgb$train(tsk("lung"))
xgbmod <- lrn_xgb$model$surv.xgboost.cox$model$model

penc = po("encode")
X <- penc$train(list(task))[[1]]$data(cols = task$feature_names)

xg_glex <- glex::glex(xgbmod, x = X)
