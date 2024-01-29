devtools::load_all()
library(mvtnorm)
library(xgboost)
library(tidyverse)
library(patchwork)
theme_set(theme_bw())


# Mean vector
p <- 2
mu <- rep(0, p)

# Covariance matrix
cov_base <- 0.3
sigma <- toeplitz(cov_base^(0:(p - 1)))
set.seed(123)

simulate_dat <- function(mu, sigma, n = 1e5, p = length(mu), beta = rep(1, length(mu)), beta0 = 0) {
  # Simulate data
  x <- matrix(rmvnorm(n = n, mean = mu, sigma = sigma),
    ncol = p,
    dimnames = list(NULL, paste0("x", seq_len(p)))
  )
  lp <- x %*% beta + beta0 + 2 * x[, 1] * x[, 2]
  y <- lp + rnorm(n)

  # Return the dataset
  list(x = x, y = y)
}

true_shap <- function(x1, x2, coord = 1) {
  if (coord == 1) x1 + x1 * x2 - cov_base
  else x2 + x1 * x2 - cov_base
}

dataset <- simulate_dat(mu, sigma, n = 1e4)
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

# SHAP decomposition
cov_args <- list(mu, sigma)

probFunction <- function(coords, lb, ub) {
  pmvnorm(lower = lb, upper = ub, mean = cov_args[[1]][coords], sigma = cov_args[[2]][coords, coords])
}

probFunctionEmp <- function(coords, lb, ub) {
  mean(apply(t(dataset$x[, coords]) > lb & t(dataset$x[, coords]) < ub, 2, all))
}

object1 <- glex(xg, dataset$x, probFunction = probFunction)
object2 <- glex(xg, dataset$x, probFunction = probFunctionEmp)

plot_shap <- function(object1, object2, coord = 2) {
  x <- dataset$x
  plotdata <- data.frame(
    x1 = x[, 1], x2 = x[, 2],
    shapA1 = true_shap(x[, 1], x[, 2], 1),
    shapA2 = true_shap(x[, 1], x[, 2], 2),
    shapB1 = object1$shap$x1, shapB2 = object1$shap$x2,
    shapEmp1 = object2$shap$x1, shapEmp2 = object2$shap$x2
  )
  plotdata.long <- reshape2::melt(plotdata, id = c("x1", "x2"))


  theme_update(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.6),
    text = element_text(family = "Helvetica Neue")
  )
  p <- ggplot(data = plotdata.long[grepl(paste0(coord, "$"), plotdata.long$variable), ],
              aes_string(x = paste0("x", coord), y = "value", color = "variable")) +
    geom_point() +
    labs(y = "SHAP")
  ggsave(paste0("shap", coord, ".png"), plot = p, width = 10, height = 8, dpi = 300)
  p
}
plot_shap(object1, object2, 1)
plot_shap(object1, object2, 2)

plot_components <- function(object1, object2, coords = "x1") {
  p1 <- autoplot(object1, coords) + geom_rug(sides = "b")
  p2 <- autoplot(object2, coords) + geom_rug(sides = "b")

  p <- p1 + p2
  ggsave(paste0("components", paste(coords, collapse = ""), ".png"), plot = p, width = 15, height = 8, dpi = 300)
  p
}

plot_components(object1, object2, coords = "x1")
plot_components(object1, object2, coords = "x2")
plot_components(object1, object2, coords = c("x1", "x2"))


x <- dataset$x
residData1 <- data.frame(
  x1 = x[, 1],
  resA1 = true_shap(x[, 1], x[, 2], 1) - object2$shap$x1,
  resB1 = object1$shap$x1 - object2$shap$x1
)
residData1.long <- reshape2::melt(residData1, id = c("x1"))


ggplot(data = residData1.long, aes(x = x1, y = value, color = variable)) +
  geom_point()
