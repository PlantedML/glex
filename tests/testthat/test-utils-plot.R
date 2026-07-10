test_that("label_m works", {
  # Not entirely sure how to properly test expressions but welp.
  expect_equal(label_m("x1"), expression(hat(m)[plain("x1")]))
  expect_equal(label_m(letters[1:4]), expression(hat(m)[plain("a, b, c, d")]))

  expect_equal(label_m(letters[1:4], mathy = FALSE), "m(a, b, c, d)")
})

test_that("discrete_palette dispatches on glex.palette_discrete", {
  old <- options(glex.palette_discrete = NULL)
  on.exit(options(old), add = TRUE)

  # default: brewer
  expect_equal(
    discrete_palette()$palette(3),
    ggplot2::scale_color_brewer(palette = "Dark2")$palette(3)
  )

  # custom color vector
  cols <- c("#E69F00", "#56B4E9", "#009E73")
  options(glex.palette_discrete = cols)
  expect_equal(discrete_palette()$palette(3), cols)

  # Okabe-Ito (manual scales return the full values vector)
  options(glex.palette_discrete = "okabe-ito")
  expect_equal(
    discrete_palette()$palette(3),
    unname(grDevices::palette.colors(palette = "Okabe-Ito"))
  )

  # scico palette name
  options(glex.palette_discrete = "batlow")
  expect_equal(
    discrete_palette()$palette(3),
    scico::scale_color_scico_d(palette = "batlow")$palette(3)
  )
})
