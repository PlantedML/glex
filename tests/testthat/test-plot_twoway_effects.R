# These tests only ensure that glex_explain can create a ggplot without error
# Plots still need to be manually expected. {vdiffr} would be an option but would only make sense
# once actual plot appearance is finalized.

# Regression / rpf ------------------------------------------------------------------------------------------------
test_that("regression rpf", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  p <- plot_twoway_effects(gl, c("wt", "hp"))
  expect_s3_class(p, "ggplot")
})

# Binary / rpf ------------------------------------------------------------------------------------------------------
test_that("binary rpf", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(y ~ x1 + x2 + x3 + x4 + x5, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  p <- plot_twoway_effects(gl, c("x1", "x2"))
  expect_s3_class(p, "ggplot")
  p <- plot_twoway_effects(gl, c("x4", "x2"))
  expect_s3_class(p, "ggplot")
  p <- plot_twoway_effects(gl, c("x4", "x5"))
  expect_s3_class(p, "ggplot")
})


# Multiclass / rpf ------------------------------------------------------------------------------------------------
test_that("multiclass rpf", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(yk ~ x1 + x2 + x3 + x4 + x5, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  p <- plot_twoway_effects(gl, c("x1", "x2"))
  expect_s3_class(p, "ggplot")
  p <- plot_twoway_effects(gl, c("x4", "x2"))
  expect_s3_class(p, "ggplot")
  p <- plot_twoway_effects(gl, c("x4", "x5"))
  expect_s3_class(p, "ggplot")

  expect_identical(p, autoplot(gl, c("x4", "x5")))
})

# Missing term ----------------------------------------------------------------------------------------------------
test_that("informative error when the interaction term is not in the decomposition", {
  # xgboost only decomposes feature subsets that co-occur on some tree path,
  # so a pair of features never split on together has no component
  set.seed(1)
  x <- as.matrix(data.frame(a = rnorm(200), b = rnorm(200), c = rnorm(200)))
  y <- factor(x[, "a"] + x[, "c"] > 0)
  xg <- xgboost(x, y, nrounds = 5, max_depth = 2, verbosity = 0)
  gl <- glex(xg, x)
  expect_false("a:b" %in% names(gl$m))

  expect_error(plot_twoway_effects(gl, c("a", "b")), "a:b.*not part of the decomposition")
  expect_error(plot_main_effect(gl, "b"), "\"b\".*not part of the decomposition")
})
