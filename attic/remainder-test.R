library(glex)
library(xgboost)
library(randomPlantedForest)
set.seed(234)
options(max.print = 10)

# this is completely arbitrary nonsense
xdat <- data.frame(
  x1 = rnorm(100),
  x2 = rpois(100, 2),
  x3 = runif(100)
)
xdat <- within(xdat, y <- 3 * x1 + 0.5 * (x2 + x3) + 3 * abs(x1 * x3))

# rpf has remainder term
rpf_fit <- rpf(y ~ ., data = xdat, num.trees = 50, max_interaction = 3)
rpf_glex <- glex(rpf_fit, xdat, max_interaction = 2)
rpf_glex$remainder

# also, intercept is stored as scalar
rpf_glex$intercept


# xgb not yet
xgb_fit <- xgboost(
  data = as.matrix(xdat[, 1:3]),
  label = xdat$y,
  max_depth = 3,
  early_stopping_rounds = 50,
  nrounds = 1000,
  verbose = FALSE
)
xgb_glex <- glex(xgb_fit, as.matrix(xdat[, 1:3]), max_interaction = 2)
xgb_glex$remainder

xgb_glex$intercept

# Also, shap is stored but known to be wrong due to max_interaction limit
xgb_glex$shap
