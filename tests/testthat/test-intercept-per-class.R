test_that("glex_vi scales relative importance by each class's intercept", {
  gl <- fake_glex_multiclass()
  gl$intercept <- c(a = 0.2, b = 0.7)
  vi <- glex_vi(gl)
  expect_equal(vi$m_rel, vi$m / c(a = 0.2, b = 0.7)[as.character(vi$class)], ignore_attr = TRUE)
})

test_that("plot_pdp adds each class's intercept", {
  gl <- fake_glex_multiclass()
  gl$intercept <- c(a = 0.2, b = 0.7)
  d <- plot_pdp(gl, "hp")$data
  expect_equal(d$m[d$class == "b"], c(-0.4, 0.3) + 0.7)
  expect_equal(d$m[d$class == "a"], c(0.4, -0.3) + 0.2)
})
