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

test_that("Prediction is approx. same as sum of decomposition + intercept, xgboost", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 15, verbose = 0)
  pred_train <- predict(xg, x)

  res_train <- glex(xg, x)
  expect_equal(res_train$intercept + rowSums(res_train$m),
    pred_train,
    tolerance = 1e-5
  )
})
