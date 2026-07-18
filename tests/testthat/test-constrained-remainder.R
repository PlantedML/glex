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

# The remainder is defined on the scale of `$m`, which for xgboost is the raw margin --
# not the response. Reconstructing on the link scale and then applying the link's inverse
# must recover the prediction; adding the remainder to a probability must not. These pin
# the scale contract, which is what makes the remainder work for non-identity links at all.
test_that("xgboost binary:logistic: remainder lives on the link scale", {
  set.seed(1)
  n <- 300
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  x <- as.matrix(d)
  yb <- as.integer(d$x1 + d$x2 * d$x3 + rnorm(n, sd = 0.3) > 0)

  xgb_bin <- xgboost::xgb.train(
    params = list(objective = "binary:logistic", max_depth = 3, eta = 0.1),
    data = xgboost::xgb.DMatrix(x, label = yb),
    nrounds = 20,
    verbose = 0
  )

  expect_warning(
    gl <- glex(xgb_bin, x, max_interaction = 1),
    "efficiency property"
  )
  recon <- gl$intercept + rowSums(gl$m) + gl$remainder

  expect_equal(
    recon,
    unname(predict(xgb_bin, x, outputmargin = TRUE)),
    tolerance = 1e-5
  )
  expect_equal(
    stats::plogis(recon),
    unname(predict(xgb_bin, x)),
    tolerance = 1e-5
  )
  # guards against "fixing" the target to the response scale, which would silently
  # subtract a probability from a margin -- the bug that kept #25 regression-only
  expect_false(isTRUE(all.equal(
    recon,
    unname(predict(xgb_bin, x)),
    tolerance = 1e-5
  )))
})

test_that("xgboost count:poisson: remainder lives on the log-link scale", {
  set.seed(1)
  n <- 300
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  x <- as.matrix(d)
  ycount <- rpois(n, lambda = exp(0.5 * d$x1))

  xgb_pois <- xgboost::xgb.train(
    params = list(objective = "count:poisson", max_depth = 3, eta = 0.1),
    data = xgboost::xgb.DMatrix(x, label = ycount),
    nrounds = 20,
    verbose = 0
  )

  expect_warning(
    gl <- glex(xgb_pois, x, max_interaction = 1),
    "efficiency property"
  )
  recon <- gl$intercept + rowSums(gl$m) + gl$remainder

  expect_equal(
    recon,
    unname(predict(xgb_pois, x, outputmargin = TRUE)),
    tolerance = 1e-5
  )
  expect_equal(exp(recon), unname(predict(xgb_pois, x)), tolerance = 1e-5)
})

test_that("ranger probability forest: remainder lives on the response scale", {
  skip_if_not_installed("ranger")
  set.seed(1)
  n <- 300
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  x <- as.matrix(d)
  y <- factor(ifelse(d$x1 + d$x2 * d$x3 + rnorm(n, sd = 0.3) > 0, "b", "a"))

  rf <- ranger::ranger(
    x = x,
    y = y,
    probability = TRUE,
    node.stats = TRUE,
    num.trees = 20,
    max.depth = 4
  )

  expect_warning(gl <- glex(rf, x, max_interaction = 1), "efficiency property")
  recon <- gl$intercept + rowSums(gl$m) + gl$remainder

  # ranger probability forests are decomposed on the probability scale directly:
  # glex takes the second class probability, so no link is involved
  expect_equal(
    recon,
    unname(predict(rf, x)$predictions[, 2]),
    tolerance = 1e-5
  )
})

# rpf decomposes the raw score (`type = "numeric"`), not the probability. Its response
# function depends on the loss -- a clamp to [0, 1] for "L2", the inverse link for "logit"
# and "exponential" -- and `predict()` applies it by default (`type = "prob"`). Targeting
# that default would fold the back-transformation into the remainder, and for binary
# models compare against the wrong class entirely.
test_that("rpf binary: remainder is on the raw score scale, for every loss", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purification OOB read, see test-rpf-sum-identity.R

  for (loss in c("L2", "logit", "exponential")) {
    set.seed(1)
    rp <- randomPlantedForest::rpf(
      y ~ x1 + x2 + x3,
      data = xdat,
      max_interaction = 3,
      loss = loss
    )

    # unconstrained: the decomposition reconstructs the raw score exactly, and owes nothing
    full <- glex(rp, xdat)
    expect_null(full$remainder)
    expect_equal(
      full$intercept + rowSums(full$m),
      unname(predict(rp, xdat, type = "numeric")[[1]]),
      tolerance = 1e-8,
      info = loss
    )

    expect_warning(
      gl <- glex(rp, xdat, max_interaction = 1),
      "efficiency property"
    )
    expect_equal(
      gl$intercept + rowSums(gl$m) + gl$remainder,
      unname(predict(rp, xdat, type = "numeric")[[1]]),
      tolerance = 1e-8,
      info = loss
    )
  }
})

test_that("rpf binary: remainder does not target the response scale", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purification OOB read, see test-rpf-sum-identity.R
  set.seed(1)
  rp <- randomPlantedForest::rpf(
    y ~ x1 + x2 + x3,
    data = xdat,
    max_interaction = 3
  )

  expect_warning(
    gl <- glex(rp, xdat, max_interaction = 1),
    "efficiency property"
  )
  recon <- gl$intercept + rowSums(gl$m) + gl$remainder

  # `predict()` defaults to type = "prob" for classification: truncated to [0, 1] and
  # ordered by class. Reconstructing against it must NOT hold -- if it ever does, the
  # target has been silently switched back to the response scale.
  probs <- predict(rp, xdat, type = "prob")
  expect_false(isTRUE(all.equal(recon, unname(probs[[1]]), tolerance = 1e-5)))
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

  # A constant predictor cannot be split on, so every term involving it is exactly zero --
  # on every platform, unlike a high-order term that merely happens to come out zero for a
  # given fit. Dropping only those terms changes nothing, so the decomposition is still
  # complete and owes no remainder. See test-rpf-sum-identity.R for the full story.
  set.seed(42)
  n <- 120
  dat <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rep(1, n)
  )
  dat$y <- dat$x1 + dat$x2 * dat$x3 + rnorm(n, sd = 0.3)
  predictors <- dat[, c("x1", "x2", "x3", "x4")]

  rp <- randomPlantedForest::rpf(
    y ~ .,
    data = dat,
    max_interaction = 4,
    ntrees = 20
  )

  expect_message(
    gl <- glex(rp, predictors, max_interaction = 3),
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
