test_that("label_m works", {
  # Not entirely sure how to properly test expressions but welp.
  expect_equal(label_m("x1"), expression(hat(m)[plain("x1")]))
  expect_equal(label_m(letters[1:4]), expression(hat(m)[plain("a, b, c, d")]))

  expect_equal(label_m(letters[1:4], mathy = FALSE), "m(a, b, c, d)")
})
