# `$remainder` is the quantitative form of `$constrained`: it holds what the dropped
# terms are collectively worth, so that the decomposition reconstructs the prediction
# again once it is added back. It must therefore behave identically across model classes,
# and be present exactly when the decomposition is incomplete.

test_that("xgboost: remainder completes a constrained decomposition", {
  set.seed(1)
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 20, verbosity = 0, max_depth = 3)

  # glex decomposes the raw margin, so the remainder lives on that scale too
  pred <- predict(xg, x, outputmargin = TRUE)

  full <- glex(xg, x)
  expect_null(full$remainder)

  expect_warning(gl <- glex(xg, x, max_interaction = 1), "efficiency property")
  expect_identical(gl$constrained, "max_interaction")
  expect_equal(
    gl$intercept + rowSums(gl$m) + gl$remainder,
    unname(pred),
    tolerance = 1e-5
  )
  # the constraint dropped something, else it would not have invalidated shap
  expect_gt(max(abs(gl$remainder)), 0)
})

test_that("xgboost: features constraint also yields a remainder", {
  set.seed(1)
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 20, verbosity = 0, max_depth = 3)
  pred <- predict(xg, x, outputmargin = TRUE)

  expect_warning(
    gl <- glex(xg, x, features = c("cyl", "hp")),
    "efficiency property"
  )
  expect_identical(gl$constrained, "features")
  expect_equal(
    gl$intercept + rowSums(gl$m) + gl$remainder,
    unname(pred),
    tolerance = 1e-5
  )
})

test_that("ranger: remainder completes a constrained decomposition", {
  skip_if_not_installed("ranger")
  set.seed(1)
  x <- as.matrix(mtcars[, -1])
  rf <- ranger::ranger(
    x = x,
    y = mtcars$mpg,
    node.stats = TRUE,
    num.trees = 10,
    max.depth = 4
  )
  pred <- predict(rf, x)$predictions

  full <- glex(rf, x)
  expect_null(full$remainder)

  expect_warning(gl <- glex(rf, x, max_interaction = 1), "efficiency property")
  expect_identical(gl$constrained, "max_interaction")
  expect_equal(
    gl$intercept + rowSums(gl$m) + gl$remainder,
    unname(pred),
    tolerance = 1e-5
  )
})

test_that("rpf: remainder completes a constrained decomposition", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purification OOB read, see test-rpf-sum-identity.R
  set.seed(1)
  rp <- randomPlantedForest::rpf(
    mpg ~ cyl + hp + wt,
    data = mtcars,
    max_interaction = 3
  )
  pred <- predict(rp, mtcars)[[1]]

  full <- glex(rp, mtcars)
  expect_null(full$remainder)

  expect_warning(
    gl <- glex(rp, mtcars, max_interaction = 1),
    "efficiency property"
  )
  expect_identical(gl$constrained, "max_interaction")
  expect_equal(
    gl$intercept + rowSums(gl$m) + gl$remainder,
    unname(pred),
    tolerance = 1e-5
  )
})

test_that("an inert constraint leaves no remainder", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purification OOB read, see test-rpf-sum-identity.R

  # A model fit at the maximum order whose top-order term is exactly zero: dropping it
  # changes nothing, so the decomposition is still complete and owes no remainder.
  set.seed(42)
  n <- 120
  p <- 6
  x <- matrix(rnorm(n * p), ncol = p)
  colnames(x) <- paste0("x", seq_len(p))
  y <- x[, 1] + x[, 2] * x[, 3] + rnorm(n, sd = 0.3)
  dat <- data.frame(x, y = y)

  rp <- randomPlantedForest::rpf(
    y ~ .,
    data = dat,
    max_interaction = p,
    ntrees = 20
  )

  expect_message(
    gl <- glex(rp, dat[, -ncol(dat)], max_interaction = p - 1L),
    "dropped terms are all zero"
  )
  expect_identical(gl$constrained, character(0))
  expect_null(gl$remainder)
})

test_that("rpf multiclass: remainder is class-wise, mirroring m", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purification OOB read, see test-rpf-sum-identity.R
  set.seed(1)
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  rpk <- randomPlantedForest::rpf(
    cyl ~ mpg + hp + wt,
    data = mt,
    max_interaction = 3
  )

  expect_warning(
    glk <- glex(rpk, mt, max_interaction = 1),
    "efficiency property"
  )
  expect_identical(glk$constrained, "max_interaction")
  # rpf computes the multiclass remainder itself, one column per class
  expect_false(is.null(glk$remainder))
  expect_identical(ncol(as.matrix(glk$remainder)), length(glk$target_levels))
})
