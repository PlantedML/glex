library(ranger)

x_train <- as.matrix(mtcars[1:26, -1])
x_test <- as.matrix(mtcars[27:32, -1])
y_train <- mtcars$mpg[1:26]
y_test <- mtcars$mpg[27:32]

# ranger
rf <- ranger(
  x = x_train, y = y_train, node.stats = TRUE,
  num.trees = 5, max.depth = 4
)
pred_train <- predict(rf, x_train)$predictions
pred_test <- predict(rf, x_test)$predictions

# Decompositions
res_train <- glex(rf, x_train, max_interaction = 4, weighting_method = "fastpd")
res_test <- glex(rf, x_test, max_interaction = 4, weighting_method = "fastpd")

res_train_path <- glex(rf, x_train, max_interaction = 4, weighting_method = "path-dependent")
res_test_path <- glex(rf, x_test, max_interaction = 4, weighting_method = "path-dependent")


test_that("FastPD ranger prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$shap),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("FastPD ranger prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$shap),
    pred_test,
    tolerance = 1e-5
  )
})

test_that("FastPD ranger prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$m),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("FastPD ranger prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$m),
    pred_test,
    tolerance = 1e-5
  )
})


test_that("Path-dependent ranger prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(res_train_path$intercept + rowSums(res_train_path$shap),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("Path-dependent ranger prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(res_test_path$intercept + rowSums(res_test_path$shap),
    pred_test,
    tolerance = 1e-5
  )
})

test_that("Path-dependent ranger prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(res_train_path$intercept + rowSums(res_train_path$m),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("Path-dependent ranger prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(res_test_path$intercept + rowSums(res_test_path$m),
    pred_test,
    tolerance = 1e-5
  )
})
