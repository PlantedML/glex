test_that("plot_waterfall draws one bar per selected term", {
  p <- plot_waterfall(fake_glex(), id = 2, max_terms = 4)
  expect_s3_class(p, "ggplot")
  expect_identical(p$data$term, c("hp", "cyl", "wt", "cyl:hp", "3 other terms"))
  expect_equal(p$data$alpha, c(1, 1, 1, 1 / sqrt(2), 0.5))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("the reference axis labels E[f] and f(x), merged when close", {
  et <- explain_terms(fake_glex(), id = 3)
  expect_identical(reference_axis(et, xrange = 3.75)$labels, c("E[f] = 19.6", "f(x) = 23.3"))
  close <- list(intercept = 19.56, prediction = 19.57)
  expect_identical(reference_axis(close, xrange = 1)$labels, "E[f] = 19.6, f(x) = 19.6")

  gl <- fake_glex()
  gl$m[1, (names(gl$m)) := list(-0.3, 0.1, 0.2, 0, 0, 0, 0)]
  expect_no_error(ggplot2::ggplot_build(plot_waterfall(gl, id = 1)))
})

test_that("plot_waterfall handles empty, multiclass and constrained objects", {
  gl <- fake_glex()
  gl$m[3, (names(gl$m)) := 0]
  expect_no_error(ggplot2::ggplot_build(plot_waterfall(gl, id = 3)))

  expect_error(plot_waterfall(fake_glex_multiclass(), id = 1), "`class` is required")
  p <- plot_waterfall(fake_glex_multiclass(), id = 1, class = "b")
  expect_match(p$labels$title, "(class b)", fixed = TRUE)

  gl <- fake_glex()
  gl$remainder <- c(0.5, -0.25, 0.1)
  gl$constrained <- "max_interaction"
  p <- plot_waterfall(gl, id = 2)
  expect_identical(p$data$term[nrow(p$data)], "remainder (constrained)")
})

test_that("plot_waterfall appearance", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("waterfall regression", plot_waterfall(fake_glex(), id = 2))
  vdiffr::expect_doppelganger("waterfall many terms", plot_waterfall(fake_glex_many(), id = 1, max_terms = 8))
  vdiffr::expect_doppelganger(
    "waterfall multiclass",
    plot_waterfall(fake_glex_multiclass(), id = 2, class = "a")
  )
})

test_that("an all-zero observation shows one combined reference label", {
  gl <- fake_glex()
  gl$m[3, (names(gl$m)) := 0]
  built <- ggplot2::ggplot_build(plot_waterfall(gl, id = 3))
  expect_identical(built$layout$panel_params[[1]]$x.sec$get_labels(), "E[f] = 19.6, f(x) = 19.6")
})

test_that("the subtitle pluralizes term counts", {
  et <- explain_terms(fake_glex(), id = 2, max_terms = 1)
  expect_identical(terms_subtitle(et), "Starting from E[f] = 19.6; 1 term shown, 6 aggregated")
  et <- explain_terms(fake_glex(), id = 2, threshold = 10)
  expect_identical(terms_subtitle(et), "Starting from E[f] = 19.6; 0 terms shown, 7 aggregated")
})
