library(xgboost)
library(ranger)

test_that("Explaining XGBoost model with dataframe input works", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glex_matrix <- glex(xg, x)
  glex_dataframe <- glex(xg, mtcars)
  expect_equal(glex_matrix$m, glex_dataframe$m)
})

test_that("Explaining Ranger model with dataframe input works", {
  x <- mtcars[, -1]
  rf <- ranger(mpg ~ ., data = mtcars, node.stats = TRUE, num.trees = 10)
  glex_matrix <- glex(rf, x)
  glex_dataframe <- glex(rf, mtcars)
  expect_equal(glex_matrix$m, glex_dataframe$m)
})
