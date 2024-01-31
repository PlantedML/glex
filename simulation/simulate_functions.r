library(mvtnorm)
library(xgboost)


simulate_dat <- function(mu, sigma, n = 1e5) {
  p <- length(mu)
  # Simulate data
  x <- matrix(rmvnorm(n = n, mean = mu, sigma = sigma),
    ncol = p,
    dimnames = list(NULL, paste0("x", seq_len(p)))
  )
  lp <- x[, 1] + x[, 2] + 2 * x[, 1] * x[, 2]
  y <- lp + rnorm(n)

  # Return the dataset
  list(x = x, y = y)
}

simulate_dat_wrapped <- function(n, c, s = FALSE) {
  if (s) {
    mu <- rep(0, 100)
    sigma <- diag(nrow = 100)
    sigma[1, 2] <- c
    sigma[2, 1] <- c
  } else {
    mu <- rep(0, 2)
    sigma <- toeplitz(c^(0:1))
  }

  list(dat = simulate_dat(mu, sigma, n), cov_args = list(mu, sigma))
}

true_shap <- function(x1, x2, coord = 1, cov_base = 0.3) {
  if (coord == 1) x1 + x1 * x2 - cov_base
  else x2 + x1 * x2 - cov_base
}

true_shap2 <- function(x, coord = 1, cov_base = 0.3) {
  x[, ..coord] + x[, 1] * x[, 2] - cov_base
}

true_components_m <- function(x, cov_base = 0.3) {
  data.table(
    x1 = x[, 1] - 2 * cov_base,
    x2 = x[, 2] - 2 * cov_base,
    "x1:x2" = 2 * x[, 1] * x[, 2] + 2 * cov_base
  )
}

probFunction_ <- function(coords, lb, ub, cov_args) {
  pmvnorm(lower = lb, upper = ub, mean = cov_args[[1]][coords], sigma = cov_args[[2]][coords, coords])
}

probFunctionEmp_ <- function(coords, lb, ub, x) {
  mean(apply(t(x[, coords]) > lb & t(x[, coords]) < ub, 2, all))
}

cov_args <- list(c(0, 0), toeplitz(0.3^(0:1)))
probFunction <- function(...) probFunction_(..., cov_args)

probFunctionEmp <- function(...) probFunctionEmp_(..., dataset$x)
probFunctionEmp2 <- function(...) probFunctionEmp_(..., dataset$x_og)
