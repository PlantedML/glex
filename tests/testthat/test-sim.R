
library(xgboost)

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

# xgboost
xg <- xgboost(data = x_train, label = y_train, params = list(max_depth = 4, eta = .1), nrounds = 10)
pred_train <- predict(xg, x_train)
pred_test <- predict(xg, x_test)

# Decompositions
res_train <- glex(xg, x_train)
res_test <- glex(xg, x_test)

test_that("Prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$shap),
               pred_train,
               tolerance = 1e-5)
})

test_that("Prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$shap),
               pred_test,
               tolerance = 1e-5)
})

test_that("Prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$m),
               pred_train,
               tolerance = 1e-5)
})

test_that("Prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$m),
               pred_test,
               tolerance = 1e-5)
})
