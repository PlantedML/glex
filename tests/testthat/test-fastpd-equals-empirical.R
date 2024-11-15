test_that("FastPD equals empirical leaf weighting", {
  set.seed(1)
  n <- 5e2
  p <- 2
  x <- matrix(rnorm(n * p), ncol = p)
  colnames(x) <- paste0("x", 1:p)
  x[, 2] <- 0.3 * x[, 1] + sqrt(1 - 0.3^2) * x[, 2]  # Add covariance
  y <- x[, 1] + x[, 2] + 2 * x[, 1] * x[, 2] + rnorm(n)

  dtrain <- xgb.DMatrix(data = x, label = y)
  xg <- xgboost(data = dtrain, max_depth = 4, eta = 1, nrounds = 15, objective = "reg:squarederror")

  fastpd <- glex(xg, x)
  empirical_leaf_weighting <- glex(xg, x, probFunction = "empirical")

  expect_equal(fastpd$m, empirical_leaf_weighting$m)
  expect_equal(fastpd$intercept, empirical_leaf_weighting$intercept)  # Check intercept
})
