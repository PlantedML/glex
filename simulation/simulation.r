old_glex <- glex::glex

devtools::load_all()
setwd("simulation")
source("simulate_functions.r")
source("plot_functions.r")
source("cv.r")
library(future)
plan(multicore)

mu <- c(0, 0)
sigma <- matrix(c(1, 0.9, 0.9, 1), ncol = 2)
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

cv_res <- cv_and_obtain_learner(dataset)
xg <- cv_res$learner$model

object1 <- glex::glex(xg, dataset$x, probFunction = probFunction)
object2 <- glex::glex(xg, dataset$x, probFunction = probFunctionEmp)
object3 <- old_glex(xg, dataset$x)

plot_components_and_true_m <- function(o1, o2, dataset, fun = function(x) -x - 2*0.3) {
  ggplot(data = as.data.frame(dataset$x)) +
    geom_line(aes(x = x1, y = o1$m$x1), color = "blue") + # Line for y1 from df1
    geom_line(aes(x = x1, y = o2$m$x1), color = "red") + # Line for y2 from df2
    stat_function(
      fun = fun, # Replace 'your_function'
      aes(x = x1), color = "black", size = 1
    ) + # with your actual function
    labs(x = "x", y = "y", title = "Plot of y1 vs y2 with Function")
}
# plots
plot_shap(object1, object2, 1)
plot_shap(object1, object2, 2)

plot_shap_resid(object1, object2, 1, emp_only = T)
plot_shap_resid(object1, object2, 2, emp_only = T)

plot_components(object1, object2, coords = "x1")
plot_components(object1, object2, coords = "x2")

plot_components(object3, object2, coords = "x1")
plot_components(object3, object2, coords = "x2")
plot_components_and_true_m(object3, object2, dataset, function(x) x - 2 * 0.3)

plot_components(object1, object2, coords = c("x1", "x2"))
plot_components(object3, object2, coords = c("x1", "x2"))
