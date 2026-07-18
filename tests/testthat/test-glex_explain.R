# These tests only ensure that glex_explain can create a ggplot without error
# Plots still need to be manually expected. {vdiffr} would be an option but would only make sense
# once actual plot appearance is finalized.

# Regression / rpf ------------------------------------------------------------------------------------------------
test_that("regression rpf", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3)
  gl <- glex(rp, mtcars)

  p <- glex_explain(gl, 2, threshold = 0)
  expect_s3_class(p, "ggplot")
})

# Binary / rpf ------------------------------------------------------------------------------------------------------
test_that("binary rpf", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  p <- glex_explain(gl, 2, threshold = 0)
  expect_s3_class(p, "ggplot")
})


# Multiclass / rpf ------------------------------------------------------------------------------------------------
test_that("multiclass rpf", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(yk ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  p <- glex_explain(gl, 2, threshold = 0)
  expect_s3_class(p, "ggplot")
})


# SHAP values come from `$shap` -----------------------------------------------------------------------------------
test_that("glex_explain plots the SHAP values stored in $shap", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex(rp, xdat)

  id <- 5
  p <- glex_explain(gl, id = id)

  # The SHAP layer carries one bar per predictor, holding that predictor's value
  # from `$shap` for this observation -- not a value recomputed from `$m`.
  shap_layer <- Filter(
    function(l) "shap" %in% names(l$data),
    p$layers
  )
  expect_length(shap_layer, 2) # bar + label

  plotted <- shap_layer[[1]]$data
  expect_equal(
    plotted[["shap"]][order(plotted[["reference_term"]])],
    unlist(gl$shap[id, ], use.names = FALSE)[order(names(gl$shap))],
    tolerance = 1e-10
  )

  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("glex_explain omits the SHAP bar for constrained decompositions", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)

  gl <- glex(rp, xdat)
  gl_constrained <- suppressWarnings(glex(rp, xdat, max_interaction = 1))

  p <- glex_explain(gl, id = 2)
  p_constrained <- glex_explain(gl_constrained, id = 2)

  # SHAP bar and its label are dropped, the component bars remain
  expect_length(p_constrained$layers, length(p$layers) - 2)
  expect_match(p_constrained$labels$subtitle, "SHAP values omitted")
  expect_false(grepl("omitted", p$labels$subtitle))

  expect_no_error(ggplot2::ggplot_build(p_constrained))
})

test_that("glex_explain works on objects without $shap", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.3.0")
  rp <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)

  # Objects from earlier glex versions (and predict_components() output) have no
  # `$shap`: the SHAP bar is omitted rather than erroring.
  gl <- randomPlantedForest::predict_components(rp, xdat)
  expect_null(gl$shap)

  p <- glex_explain(gl, id = 2)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})
