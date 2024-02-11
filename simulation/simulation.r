old_glex <- glex::glex

devtools::load_all()
setwd('simulation')
source("simulate_functions.r")
source("plot_functions.r")
source("cv.r")

mu <- c(0,0)
sigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
dataset <- simulate_dat(mu, sigma, n = 1e4)

dtrain <- xgb.DMatrix(data = dataset$x, label = dataset$y)

# XGBoost parameters
params <- list(
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.043
)

# Number of boosting rounds
optimal_nrounds <- 29
xg <- xgboost(
  params = params,
  data = dtrain,
  nrounds = optimal_nrounds,
  verbose = 1
)

library(future)
plan(multicore)
cv_res <- cv_and_obtain_learner(dataset)
xg <- cv_res$learner$model

object1 <- glex::glex(xg, dataset$x, probFunction = probFunction)
object2 <- glex::glex(xg, dataset$x, probFunction = probFunctionEmp)
object3 <- old_glex(xg, dataset$x)


# plots
plot_shap(object1, object2, 1)
plot_shap(object1, object2, 2)

plot_shap_resid(object1, object2, 1, emp_only = T)
plot_shap_resid(object1, object2, 2, emp_only = T)

plot_components(object1, object2, coords = "x1")
plot_components(object1, object2, coords = "x2")

plot_components(object3, object2, coords = "x1")
plot_components(object3, object2, coords = "x2")

plot_components(object1, object2, coords = c("x1", "x2"))
plot_components(object3, object2, coords = c("x1", "x2"))
