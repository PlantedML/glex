test_that("glex methods warn about arguments they do not use", {
  x <- as.matrix(mtcars[, -1])

  rf <- ranger::ranger(x = x, y = mtcars$mpg, num.trees = 2, node.stats = TRUE)
  expect_warning(glex(rf, x, weighting_methd = "fastpd"), "weighting_methd")

  xg <- xgboost::xgb.train(
    params = list(max_depth = 2),
    data = xgboost::xgb.DMatrix(x, label = mtcars$mpg),
    nrounds = 2
  )
  expect_warning(glex(xg, x, weighting_methd = "fastpd"), "weighting_methd")

  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(mpg ~ cyl + hp, data = mtcars, max_interaction = 1)
  expect_warning(glex(rp, mtcars, weighting_method = "fastpd"), "weighting_method")
})
