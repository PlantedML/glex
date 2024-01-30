library(mvtnorm)
library(xgboost)


# Mean vector
p <- 2
mu <- rep(0, p)

# Covariance matrix
cov_base <- 0.0
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

cov_args <- list(mu, sigma)

probFunction <- function(coords, lb, ub) {
  pmvnorm(lower = lb, upper = ub, mean = cov_args[[1]][coords], sigma = cov_args[[2]][coords, coords])
}

probFunctionEmp_ <- function(coords, lb, ub, x) {
  mean(apply(t(x[, coords]) > lb & t(x[, coords]) < ub, 2, all))
}

probFunctionEmp <- function(...) probFunctionEmp_(..., dataset$x)
probFunctionEmp2 <- function(...) probFunctionEmp_(..., dataset$x_og)
