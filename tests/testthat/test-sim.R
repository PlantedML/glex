# Simulate data
n <- 100
p <- 2
beta <- c(1, 1)
beta0 <- 0
x <- matrix(rnorm(n = n * p), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0 + 2*x[, 1] * x[, 2]
y <- lp + rnorm(n)

x_train <- x[1:50, ]
x_test <- x[51:100, ]
y_train <- y[1:50]
y_test <- y[51:100]

ybin <- ifelse(1 / (1 + exp(-lp)) > .5, 1, 0)
ybin_train <- ybin[1:50]
ybin_test <- ybin[51:100]

# xgboost: regression
xg <- xgboost(data = x_train, label = y_train, params = list(max_depth = 4, eta = .1), nrounds = 10, verbose = 0)
pred_train <- predict(xg, x_train)
pred_test <- predict(xg, x_test)

# Decompositions
res_train <- glex(xg, x_train)
res_test <- glex(xg, x_test)

# xgboost: binary classif
xgbin <- xgboost(data = x_train, label = ybin_train, params = list(max_depth = 4, eta = .1), nrounds = 10, verbose = 0)
predbin_train <- predict(xgbin, x_train)
predbin_test <- predict(xgbin, x_test)

# Decompositions
resbin_train <- glex(xgbin, x_train)
resbin_test <- glex(xgbin, x_test)

test_that("regr: Prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$shap),
               pred_train,
               tolerance = 1e-5)
})

test_that("binary: Prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(resbin_train$intercept + rowSums(resbin_train$shap),
               predbin_train,
               tolerance = 1e-5)
})

test_that("regr: Prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$shap),
               pred_test,
               tolerance = 1e-5)
})

test_that("binary: Prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(resbin_test$intercept + rowSums(resbin_test$shap),
               predbin_test,
               tolerance = 1e-5)
})

test_that("regr: Prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$m),
               pred_train,
               tolerance = 1e-5)
})

test_that("classif: Prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(resbin_train$intercept + rowSums(resbin_train$m),
               predbin_train,
               tolerance = 1e-5)
})

test_that("regr: Prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$m),
               pred_test,
               tolerance = 1e-5)
})

test_that("binary: Prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(resbin_test$intercept + rowSums(resbin_test$m),
               predbin_test,
               tolerance = 1e-5)
})

test_that("Shap is computed correctly with overlapping colnames", {
  n <- 100
  p <- 2
  beta <- c(1, 1)
  beta0 <- 0
  x <- matrix(rnorm(n = n * p), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))

  lp <- x %*% beta + beta0 + 2*x[, 1] * x[, 2]
  y <- lp + rnorm(n)

  # xgboost
  set.seed(5)
  xg1 <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .1), nrounds = 10, verbose = 0)

  # Decompositions
  # x names are unique: x1, x2
  unique_names <- glex(xg1, x)$shap

  # make names overlapping such that grep("temp") also matches "atemp"
  colnames(x) <- c("temp", "atemp")

  # re-train xgboost because changing colnames of x and running glex on that crashes R
  set.seed(5)
  xg2 <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .1), nrounds = 10, verbose = 0)

  overlapping_names <- glex(xg2, x)$shap

  # We only check values for equality, colnames will of course differ
  expect_equal(unname(overlapping_names), unname(unique_names))

})
