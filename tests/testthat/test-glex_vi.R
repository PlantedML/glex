# Regression / rpf ------------------------------------------------------------------------------------------------
test_that("regression rpf", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  vi <- glex_vi(gl) |>
    expect_s3_class("glex_vi") |>
    expect_s3_class("data.table") |>
    expect_named(c("degree", "term", "m", "m_rel"))

  expect_false(any(sapply(vi, anyNA)))
})

test_that("regression rpf plot", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  vi <- glex_vi(gl)

  p1 <- autoplot(vi)
  expect_s3_class(p1, "ggplot")

  p2 <- autoplot(vi, threshold = 0.02)
  expect_s3_class(p2, "ggplot")

  p3 <- autoplot(vi, max_interaction = 2)
  expect_s3_class(p3, "ggplot")

  p4 <- autoplot(vi, max_interaction = 2, threshold = 0.02)
  expect_s3_class(p4, "ggplot")
})


# Binary / rpf ------------------------------------------------------------------------------------------------------
test_that("binary rpf", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl) |>
    expect_s3_class("glex_vi") |>
    expect_s3_class("data.table") |>
    expect_named(c("degree", "term", "m", "m_rel"))

  expect_false(any(sapply(vi, anyNA)))
})

test_that("binary rpf plot", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl)

  p1 <- autoplot(vi)
  expect_s3_class(p1, "ggplot")

  p2 <- autoplot(vi, threshold = 0.05)
  expect_s3_class(p2, "ggplot")

  p3 <- autoplot(vi, max_interaction = 2)
  expect_s3_class(p3, "ggplot")

  p4 <- autoplot(vi, max_interaction = 2, threshold = 0.05)
  expect_s3_class(p4, "ggplot")
})

# Multiclass / rpf ------------------------------------------------------------------------------------------------
test_that("multiclass rpf", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(yk ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl) |>
    expect_s3_class("glex_vi") |>
    expect_s3_class("data.table") |>
    expect_named(c("degree", "term", "class", "m", "m_rel"))

  expect_false(any(sapply(vi, anyNA)))
})

test_that("multiclass rpf plot", {
  skip_if_not_installed("randomPlantedForest")
  rp <- rpf(yk ~ x1 + x2 + x3, data = xdat, max_interaction = 3, deterministic = TRUE)
  gl <- glex(rp, xdat)

  vi <- glex_vi(gl)

  p1 <- autoplot(vi)
  expect_s3_class(p1, "ggplot")

  p2 <- autoplot(vi, threshold = 0.05)
  expect_s3_class(p2, "ggplot")

  p3 <- autoplot(vi, max_interaction = 2)
  expect_s3_class(p3, "ggplot")

  p4 <- autoplot(vi, max_interaction = 2, threshold = 0.05)
  expect_s3_class(p4, "ggplot")

  p5 <- autoplot(vi, max_interaction = 1, by_degree = TRUE)
  expect_s3_class(p5, "ggplot")

  p6 <- autoplot(vi, threshold = 0.05, by_degree = TRUE)
  expect_s3_class(p6, "ggplot")
})
