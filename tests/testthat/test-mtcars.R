x_train <- as.matrix(mtcars[1:26, -1])
x_test <- as.matrix(mtcars[27:32, -1])
y_train <- mtcars$mpg[1:26]
y_test <- mtcars$mpg[27:32]

# xgboost
xg <- xgboost(data = x_train, label = y_train, params = list(max_depth = 4, eta = .1), nrounds = 10, verbose = 0)
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
