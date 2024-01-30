devtools::load_all()
source("simulate_functions.r")


dataset <- simulate_dat(mu, sigma, n = 1e4)
dataset$x_og <- dataset$x
# dataset$x <- dataset$x[, c(2, 1)] # switch columns

dtrain <- xgb.DMatrix(data = dataset$x, label = dataset$y)

# XGBoost parameters
params <- list(
  objective = "reg:squarederror",
  max_depth = 3,
  eta = 0.1
)

# Number of boosting rounds
optimal_nrounds <- 123
xg <- xgboost(
  params = params,
  data = dtrain,
  nrounds = optimal_nrounds,
  verbose = 1
)

object1 <- glex::glex(xg, dataset$x, probFunction = probFunction)
object2 <- glex::glex(xg, dataset$x, probFunction = probFunctionEmp)
