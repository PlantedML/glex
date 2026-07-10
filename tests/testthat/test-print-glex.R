test_that("print.glex works", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see test-rpf-sum-identity.R
  set.seed(2)
  rp <- rpf(mpg ~ cyl + hp, data = mtcars)
  gl <- glex(rp, mtcars)

  out <- capture.output(gl)

  expect_match(out[[1]], "^glex object of subclass rpf_components")
  expect_match(
    out[[2]],
    "^Explaining predictions of 32 observations with 2 terms of up to 1 degree"
  )
})
