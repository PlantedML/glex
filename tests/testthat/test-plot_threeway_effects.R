# These tests only ensure that glex_explain can create a ggplot without error
# Plots still need to be manually expected. {vdiffr} would be an option but would only make sense
# once actual plot appearance is finalized.

# Regression / rpf ------------------------------------------------------------------------------------------------
test_that("regression rpf", {
  skip_if_not_installed("randomPlantedForest")
  mtcars$cyl <- factor(mtcars$cyl)
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  p <- plot_threeway_effects(gl, c("wt", "hp", "cyl"))
  expect_s3_class(p, "ggplot")
})

# Binary / rpf ------------------------------------------------------------------------------------------------------
test_that("binary rpf", {
  skip_if_not_installed("randomPlantedForest")
  xdat$x1fact <- as.factor(as.integer(xdat$x1 > 0))
  rp <- rpf(y ~ x1fact + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  p <- plot_threeway_effects(gl, c("x1fact", "x2", "x3"))
  expect_s3_class(p, "ggplot")
})


# Multiclass / rpf ------------------------------------------------------------------------------------------------
test_that("multiclass rpf", {
  skip_if_not_installed("randomPlantedForest")
  xdat$x1fact <- as.factor(as.integer(xdat$x1 > 0))
  rp <- rpf(yk ~ x1fact + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  p <- plot_threeway_effects(gl, c("x1fact", "x2", "x3"))
  expect_s3_class(p, "ggplot")
})
