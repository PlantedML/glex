test_that("max_interaction respects xgb's max_depth", {
  set.seed(1)
  x <- as.matrix(mtcars[, -1])

  xg <- xgboost(x, mtcars$mpg, nrounds = 50, verbose = 0, max_depth = 2)
  glexb <- glex(xg, x)
  max_degree <- max(lengths(strsplit(names(glexb$m), split = ":", fixed = TRUE)))

  # The maximum interaction degree is 2^d - 1 = 4 - 1 = 3
  expect_equal(max_degree, 3)
})

test_that("features argument only calculates for given features", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glexb <- glex(xg, x, features = c("cyl", "disp"))
  expect_equal(colnames(glexb$m), c("cyl", "cyl:disp", "disp"))
})

test_that("features argument results in same values as without", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glexb1 <- glex(xg, x, features = c("cyl", "disp"))
  glexb2 <- glex(xg, x)
  cols <- c("cyl", "disp", "cyl:disp")
  expect_equal(glexb1$m[, ..cols], glexb2$m[, ..cols])
})

test_that("features argument works together with max_interaction", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glexb <- glex(xg, x, features = c("cyl", "disp"), max_interaction = 1)
  expect_equal(colnames(glexb$m), c("cyl", "disp"))
})


x_train <- as.matrix(mtcars[1:26, -1])
x_test <- as.matrix(mtcars[27:32, -1])
y_train <- mtcars$mpg[1:26]
y_test <- mtcars$mpg[27:32]

xg <- xgboost(x_train, y_train, nrounds = 10, max_depth = 4, verbose = 0)
pred_train <- predict(xg, x_train)
pred_test <- predict(xg, x_test)

res_train <- glex(xg, x_train, max_interaction = 4, weighting_method = "fastpd")
res_test <- glex(xg, x_test, max_interaction = 4, weighting_method = "fastpd")

res_train_path <- glex(xg, x_train, max_interaction = 4, weighting_method = "path-dependent")
res_test_path <- glex(xg, x_test, max_interaction = 4, weighting_method = "path-dependent")


test_that("FastPD xgboost prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$shap),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("FastPD xgboost prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$shap),
    pred_test,
    tolerance = 1e-5
  )
})

test_that("FastPD xgboost prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(res_train$intercept + rowSums(res_train$m),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("FastPD xgboost prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(res_test$intercept + rowSums(res_test$m),
    pred_test,
    tolerance = 1e-5
  )
})


test_that("Path-dependent xgboost prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(res_train_path$intercept + rowSums(res_train_path$shap),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("Path-dependent xgboost prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(res_test_path$intercept + rowSums(res_test_path$shap),
    pred_test,
    tolerance = 1e-5
  )
})

test_that("Path-dependent xgboost prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(res_train_path$intercept + rowSums(res_train_path$m),
    pred_train,
    tolerance = 1e-5
  )
})

test_that("Path-dependent xgboost prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(res_test_path$intercept + rowSums(res_test_path$m),
    pred_test,
    tolerance = 1e-5
  )
})
