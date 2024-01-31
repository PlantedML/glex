devtools::load_all()
source("simulate_functions.r")
source("cv.r")


dataset <- simulate_dat(mu, sigma, n = 1e4)
dataset$x_og <- dataset$x
dataset$x <- dataset$x[, c(2, 1)] # switch columns

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

object1 <- glex::glex(xg, dataset$x, probFunction = probFunction)
object2swapped <- glex::glex(xg, dataset$x, probFunction = probFunctionEmp)

dataset <- simulate_dat(mu, sigma, n = 1e3)

res <- cv_and_obtain_learner(dataset)
learner <- res[[1]]
instance <- res[[2]]

autoplot(instance, type = "pairs")
autoplot(task, type = "pairs")

xg <- learner$model
object1 <- glex(xg, dataset$x, probFunction = probFunction)
object2 <- glex(xg, dataset$x, probFunction = probFunctionEmp)

# plots
plot_shap(object1, object2, 1)
plot_shap(object1, object2, 2)

plot_shap_resid(object1, object2, 1, emp_only = T)
plot_shap_resid(object1, object2, 2, emp_only = T)

plot_components(object1, object2, coords = "x1")
plot_components(object1, object2, coords = "x2")
plot_components(object1, object2, coords = c("x1", "x2"))
