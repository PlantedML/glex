library(glex)
library(xgboost)
library(mlr3verse)
set.seed(21)

data("kc_housing", package = "mlr3data")
train.inds = !grepl(pattern = "2015", kc_housing$date)
test.inds = grepl(pattern = "2015", kc_housing$date)
kc_housing$yr_renovated = kc_housing$sqft_basement = kc_housing$date = NULL

task = as_task_regr(x = kc_housing, target = "price")

learners = list(
  lrn("regr.ranger"),
  lrn("regr.lm"),
  lrn("regr.featureless"),
  lrn(
    "regr.xgboost",
    max_depth = 4,
    eta = .2,
    min_child_weight = 1.9,
    subsample = 0.7,
    colsample_bytree = 0.9,
    colsample_bylevel = 0.63,
    nrounds = 30
  )
)

design = benchmark_grid(task, learners, rsmp("cv", folds = 3))
bench = benchmark(design)
bench$aggregate(msr("regr.rmse"))

# xgb hyperpars taken from https://mlr-org.com/gallery/basic/2020-01-30-house-prices-in-king-county/index.html
xgb = learners[[4]]
xgb$train(task, row_ids = which(train.inds))
pred = xgb$predict(task, row_ids = which(test.inds))
pred$score(msr("regr.rmse"))

# Decompose
glex_xgb_train = glex(
  xgb$model,
  x = as.matrix(task$data(rows = which(train.inds)))
)
glex_xgb_test = glex(
  xgb$model,
  x = as.matrix(task$data(rows = which(test.inds)))
)

# Create new data using found components
train2 = cbind(glex_xgb_train$m, glex_xgb_train$intercept)
test2 = cbind(glex_xgb_test$m, glex_xgb_test$intercept)
ytrain = kc_housing$price[train.inds]
ytest = kc_housing$price[test.inds]

# Train a LASSO to obtain a sparse linear combination of the found components
library(glmnet)
train2mat = as.matrix(as.data.frame(train2))
test2mat = as.matrix(as.data.frame(test2))
lasso = cv.glmnet(train2mat, ytrain)
#plot(lasso)
#coef(lasso, s = "lambda.min")

# Train a LASSO with post-hoc feature removal of zipcode for fairness reasons
# (zipcode is sometimes associated with rassism)
train2fair = train2mat[, !grepl("zipcode", colnames(train2mat))]
test2fair = test2mat[, !grepl("zipcode", colnames(test2mat))]
lasso_fair = cv.glmnet(train2fair, ytrain)

# Compare models: original xgb model vs. LASSO on found components vs. LASSO without zipocde
sqrt(mean((pred$response - ytest)^2))
sqrt(mean((predict(lasso, newx = test2mat, s = "lambda.min") - ytest)^2))
sqrt(mean((predict(lasso_fair, newx = test2fair, s = "lambda.min") - ytest)^2))

# component indices containing "zipcode", i.e. main or interaction effects of zipcode
idx_unfair = glex:::find_term_matches("zipcode", names(glex_xgb_test$m))
components_fair = glex_xgb_test$m[, -idx_unfair, with = FALSE]
# Prediction = sum of components (w/o zipcode) + intercept (average prediction)
components_fair_sum = rowSums(components_fair) + glex_xgb_test$intercept

# "Raw" prediction of XGb with zipcode removed
sqrt(mean((components_fair_sum - ytest)^2))

c(
  "Original XGboost prediction" = sqrt(mean((pred$response - ytest)^2)),
  "Prediction without zipcode" = sqrt(mean((components_fair_sum - ytest)^2)),
  "LASSO on components" = sqrt(mean(
    (predict(lasso, newx = test2mat, s = "lambda.min") - ytest)^2
  )),
  "LASSO on fair components" = sqrt(mean(
    (predict(lasso_fair, newx = test2fair, s = "lambda.min") - ytest)^2
  ))
) |>
  sort()

# Variable importance of components containing zipcode
vi = glex_vi(glex_xgb_test)

# VIs of everything containing zipcode: mostly 2nd order effects
vi[glex:::find_term_matches("zipcode", vi$term), ] |>
  autoplot(by_degree = TRUE)

# VIs of everything else: mostly main effects
vi[-glex:::find_term_matches("zipcode", vi$term), ] |>
  autoplot(by_degree = TRUE)

# Mostly flat with one huge spike -> specific neighborhood?
plot_main_effect(glex_xgb_test, "zipcode")

# Greatest inetraction effects: with lat/longitude, house/apartment sizes: plausible
vi[glex:::find_term_matches("zipcode", vi$term), ] |>
  dplyr::slice_max(m, n = 10)

plot_main_effect(glex_xgb_test, "zipcode")
