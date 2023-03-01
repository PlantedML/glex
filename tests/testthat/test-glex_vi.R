# Regression / rpf ------------------------------------------------------------------------------------------------
test_that("regression rpf", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  vi <- glex_vi(gl) |>
    expect_s3_class("glex_vi") |>
    expect_s3_class("data.table") |>
    expect_named(c("degree", "term", "term_list", "m", "m_rel"))

  expect_false(any(sapply(vi, anyNA)))
})

test_that("regression rpf plot", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  vi <- glex_vi(gl)

  p <- autoplot(vi)
  expect_s3_class(p, "ggplot")
})


# Binary rpf ------------------------------------------------------------------------------------------------------
# Simulated data with binary/multiclass target
xdat <- data.frame(
  x1 = rnorm(100),
  x2 = rpois(100, 2),
  x3 = runif(100)
)

xdat$lp = 3 * xdat$x1 + 0.5 * (xdat$x2 + xdat$x3) + 3 * abs(xdat$x1 * xdat$x3)
xdat$p = 1/(1 + exp(-xdat$lp))
xdat$y = factor(rbinom(100, size = 1, prob = xdat$p), labels = c("Negative", "Positive"))
xdat$yk = factor(rbinom(100, size = 2, prob = xdat$p), labels = c("N", "P", "K"))

test_that("binary rpf", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl) |>
    expect_s3_class("glex_vi") |>
    expect_s3_class("data.table") |>
    expect_named(c("degree", "term", "term_list", "m", "m_rel"))

  expect_false(any(sapply(vi, anyNA)))
})

test_that("binary rpf plot", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl)

  p <- autoplot(vi)
  expect_s3_class(p, "ggplot")
})

test_that("multiclass rpf", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(yk ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl) |>
    expect_s3_class("glex_vi") |>
    expect_s3_class("data.table") |>
    expect_named(c("degree", "term", "term_list", "class", "m", "m_rel"))

  expect_false(any(sapply(vi, anyNA)))
})

test_that("multiclass rpf plot", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(yk ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl)

  p <- autoplot(vi)
  expect_s3_class(p, "ggplot")
})
