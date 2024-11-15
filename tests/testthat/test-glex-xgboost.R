test_that("max_interaction respects xgb's max_depth", {
  x <- as.matrix(mtcars[, -1])

  # xgb default: 6
  xg <- xgboost(x, mtcars$mpg, nrounds = 50, verbose = 0)
  glexb <- glex(xg, x)
  max_degree <- max(lengths(strsplit(names(glexb$m), split = ":", fixed = TRUE)))

  expect_equal(max_degree, 6)
  expect_error(glex(xg, x, max_interaction = 7))

  glexb <- glex(xg, x, max_interaction = 4)
  max_degree <- max(lengths(strsplit(names(glexb$m), split = ":", fixed = TRUE)))
  expect_equal(max_degree, 4)

  # Setting max_depth explicitly to value lower than default
  xg <- xgboost(x, mtcars$mpg, nrounds = 50, verbose = 0, params = list(max_depth = 4))
  glexb <- glex(xg, x)
  max_degree <- max(lengths(strsplit(names(glexb$m), split = ":", fixed = TRUE)))

  expect_equal(max_degree, 4)
  expect_error(glex(xg, x, max_interaction = 5))

  # Setting max_depth explicitly to value higher than default
  xg <- xgboost(x, mtcars$mpg, nrounds = 50, verbose = 0, params = list(max_depth = 7))
  glexb <- glex(xg, x)
  max_degree <- max(lengths(strsplit(names(glexb$m), split = ":", fixed = TRUE)))

  expect_equal(max_degree, 7)
  expect_error(glex(xg, x, max_interaction = 8))
})

test_that("features argument only calculates for given feautures", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glexb <- glex(xg, x, features = c("cyl", "disp"))
  expect_equal(colnames(glexb$m), c("cyl", "disp", "cyl:disp"))
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
