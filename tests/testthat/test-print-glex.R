test_that("print.glex works", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.5.0.9000")
  set.seed(2)
  rp <- rpf(mpg ~ cyl + hp, data = mtcars, max_interaction = 1)
  gl <- glex(rp, mtcars)

  out <- capture.output(gl)

  expect_match(out[[1]], "^glex object of subclass rpf_components")
  expect_match(
    out[[2]],
    "^Explaining predictions of 32 observations with 2 terms of up to 1 degree"
  )
})

test_that("print.glex pluralizes observations, terms and degrees", {
  gl <- fake_glex()
  gl$x <- gl$x[1]
  gl$m <- gl$m[1, "hp"]
  out <- capture.output(print(gl))
  expect_match(out[[2]], "^Explaining predictions of 1 observation with 1 term of up to 1 degree$")
})
