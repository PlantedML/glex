# Benchmark glex speed for somewhat larger datasets / max_interaction terms
library(glex)
library(ranger)
data("bike")

fit <- ranger(
  bikers ~ hr + workingday + temp,
  data = bike,
  node.stats = TRUE,
  max.depth = 3,
  num.trees = 10
)
glexfit <- glex(fit, x = bike)

fit <- ranger(
  x = bike[, c("hr", "season", "hum")],
  y = bike$bikers,
  node.stats = TRUE,
  max.depth = 3,
  num.trees = 10
)
glexfit <- glex(fit, x = bike)

bike_mat <- model.matrix(~ . - 1, bike[, c("hr", "season", "hum")])
fit <- ranger(
  x = bike_mat,
  y = bike$bikers,
  node.stats = TRUE,
  max.depth = 3,
  num.trees = 10
)
glexfit <- glex(fit, x = bike_mat)


bike_mat <- model.matrix(~ . - 1, bike[, c("hr", "season", "hum")])
fit <- xgboost::xgboost(data = bike_mat, label = bike$bikers, nrounds = 100)
glexfit <- glex(fit, x = bike_mat)

library(glex)
library(ranger)

x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg

rf <- ranger(x = x, y = y, node.stats = TRUE, max.depth = 5, num.trees = 10)
glex(rf, x, max_interaction = 2)


as.matrix(model.matrix(~ . - 1, bike[1:10, c("hr", "season", "hum")]))


# Tree infos ----------------------------------------------------------------------------------

info_ranger <- `test-tree-info-ranger`
info_xgb <- `test-tree-info-xgboost`
