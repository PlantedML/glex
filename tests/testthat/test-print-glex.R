test_that("print.glex works", {
  set.seed(2)
  rp <- rpf(mpg ~ cyl, data = mtcars)
  gl <- glex(rp, mtcars)

  out <- capture.output(gl)

  expect_match(out[[1]], "^glex object of subclass rpf_components")

  # snapshot test maybe?
  expect_snapshot(out)
})
