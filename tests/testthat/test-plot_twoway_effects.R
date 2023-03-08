# These tests only ensure that glex_explain can create a ggplot without error
# Plots still need to be manually expected. {vdiffr} would be an option but would only make sense
# once actual plot appearance is finalized.

# Regression / rpf ------------------------------------------------------------------------------------------------
test_that("regression rpf", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  p <- plot_twoway_effects(gl, c("wt", "hp"))
  expect_s3_class(p, "ggplot")
})

# Binary / rpf ------------------------------------------------------------------------------------------------------
test_that("binary rpf", {
  skip_if_not_installed("randomPlantedForest")
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
  skip_if_not_installed("randomPlantedForest")
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
