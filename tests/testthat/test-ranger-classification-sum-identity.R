test_that("ranger binary: FastPD sum identity matches class probability", {
  skip_if_not_installed("ranger")
  x <- xdat[, c("x1", "x2", "x3")]
  y <- xdat$y

  rf <- ranger::ranger(
    x = x,
    y = y,
    probability = TRUE,
    node.stats = TRUE,
    num.trees = 20,
    max.depth = 4,
    seed = 1
  )
  gl <- glex::glex(rf, x, weighting_method = "fastpd")
  pred <- predict(rf, x)$predictions

  score <- unname(gl$intercept + rowSums(gl$m))
  class_diffs <- apply(pred, 2, function(pcol) max(abs(score - pcol)))

  expect_lt(min(class_diffs), 0.05)
})

test_that("ranger binary: path-dependent sum identity matches class probability", {
  skip_if_not_installed("ranger")
  x <- xdat[, c("x1", "x2", "x3")]
  y <- xdat$y

  rf <- ranger::ranger(
    x = x,
    y = y,
    probability = TRUE,
    node.stats = TRUE,
    num.trees = 20,
    max.depth = 4,
    seed = 1
  )
  gl <- glex::glex(rf, x, weighting_method = "path-dependent")
  pred <- predict(rf, x)$predictions

  score <- unname(gl$intercept + rowSums(gl$m))
  class_diffs <- apply(pred, 2, function(pcol) max(abs(score - pcol)))

  expect_lt(min(class_diffs), 0.05)
})

test_that("ranger multiclass: FastPD is currently unsupported", {
  skip_if_not_installed("ranger")
  x <- xdat[, c("x1", "x2", "x3")]
  y <- xdat$yk

  rf <- ranger::ranger(
    x = x,
    y = y,
    probability = TRUE,
    node.stats = TRUE,
    num.trees = 20,
    max.depth = 4,
    seed = 1
  )
  expect_error(
    glex::glex(rf, x, weighting_method = "fastpd"),
    "more than 2 classes is not supported"
  )
})

test_that("ranger multiclass: path-dependent is currently unsupported", {
  skip_if_not_installed("ranger")
  x <- xdat[, c("x1", "x2", "x3")]
  y <- xdat$yk

  rf <- ranger::ranger(
    x = x,
    y = y,
    probability = TRUE,
    node.stats = TRUE,
    num.trees = 20,
    max.depth = 4,
    seed = 1
  )
  expect_error(
    glex::glex(rf, x, weighting_method = "path-dependent"),
    "more than 2 classes is not supported"
  )
})
