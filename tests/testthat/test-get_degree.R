test_that("get_degree ignores the multiclass suffix", {
  expect_identical(
    get_degree(c("x1", "x1:x2", "mpg__class:4", "hp:mpg__class:4", "a:b:c__class:x:y")),
    c(1L, 2L, 1L, 2L, 3L)
  )
})

test_that("multiclass rpf objects print their actual degree", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  set.seed(1)
  rp <- rpf(cyl ~ mpg + hp, data = transform(mtcars, cyl = factor(cyl)), max_interaction = 1)
  out <- capture.output(glex(rp, mtcars))
  expect_match(out[[2]], "up to 1 degree")
})
