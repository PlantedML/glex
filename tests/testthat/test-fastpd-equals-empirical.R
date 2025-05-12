library(ranger)

test_that("FastPD equals empirical leaf weighting", {
  set.seed(1)
  n <- 5e2
  p <- 2
  x <- matrix(rnorm(n * p), ncol = p)
  colnames(x) <- paste0("x", 1:p)
  x[, 2] <- 0.3 * x[, 1] + sqrt(1 - 0.3^2) * x[, 2] # Add covariance
  y <- x[, 1] + x[, 2] + 2 * x[, 1] * x[, 2] + rnorm(n)

  rf <- ranger(x = x, y = y, num.trees = 5, max.depth = 4, node.stats = TRUE)

  fastpd <- glex(rf, x)
  empirical_leaf_weighting <- glex(rf, x, probFunction = "empirical")

  expect_equal(fastpd$m, empirical_leaf_weighting$m)
  expect_equal(fastpd$intercept, empirical_leaf_weighting$intercept) # Check intercept
})

test_that("FastPD equals empirical leaf weighting for lower interactions", {
  set.seed(1)
  x <- as.matrix(mtcars[, -1])
  rf <- ranger(
    x = x, y = mtcars$mpg,
    node.stats = TRUE,
    num.trees = 5, max.depth = 4
  )

  fastpd <- glex(rf, x)
  empirical_leaf_weighting <- glex(rf, x, probFunction = "empirical")

  sorted_cols_fastpd <- sapply(colnames(fastpd$m), function(col) paste(sort(strsplit(col, ":")[[1]]), collapse = ":"))
  colnames(fastpd$m) <- sorted_cols_fastpd

  sorted_cols_empirical <- sapply(colnames(empirical_leaf_weighting$m), function(col) paste(sort(strsplit(col, ":")[[1]]), collapse = ":"))
  colnames(empirical_leaf_weighting$m) <- sorted_cols_empirical

  expect_equal(
    fastpd$m,
    empirical_leaf_weighting$m,
    ignore_attr = TRUE
  )
})
