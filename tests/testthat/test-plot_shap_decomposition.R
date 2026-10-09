# Regression / rpf ------------------------------------------------------------------------------------------------
test_that("regression, binary and multiclass rpf", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.5.0.9000")
  set.seed(1)
  gl <- glex(rpf(mpg ~ cyl + hp + wt, data = mtcars, max_interaction = 3), mtcars)
  expect_no_error(ggplot2::ggplot_build(plot_shap_decomposition(gl, 2)))

  gl <- glex(rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3), xdat)
  expect_no_error(ggplot2::ggplot_build(plot_shap_decomposition(gl, 2)))

  gl <- glex(rpf(yk ~ x1 + x2 + x3, data = xdat, max_interaction = 3), xdat)
  expect_no_error(ggplot2::ggplot_build(plot_shap_decomposition(gl, 2)))
  expect_no_error(ggplot2::ggplot_build(plot_shap_decomposition(gl, 2, class = "P")))
})

# SHAP values come from `$shap` -----------------------------------------------------------------------------------
test_that("the SHAP rows hold the values stored in $shap", {
  gl <- fake_glex()
  p <- plot_shap_decomposition(gl, id = 2)
  shap_rows <- p$data[p$data$kind == "shap", ]
  expect_equal(
    shap_rows$m_scaled[order(shap_rows$reference_term)],
    unlist(gl$shap[2, ], use.names = FALSE)[order(names(gl$shap))]
  )
  expect_equal(shap_rows$xleft, rep(gl$intercept, 3))
})

test_that("each feature's bars sum to its SHAP value", {
  gl <- fake_glex()
  d <- plot_shap_decomposition(gl, id = 2, threshold = 0.05)$data
  sums <- d[d$kind != "shap", list(s = sum(m_scaled)), by = "reference_term"]
  shap <- d[d$kind == "shap", ]
  expect_equal(sums$s[match(shap$reference_term, sums$reference_term)], shap$m_scaled)
})

test_that("Remaining terms sit directly above SHAP", {
  d <- plot_shap_decomposition(fake_glex(), id = 2, threshold = 0.05)$data
  for (ref in unique(d$reference_term)) {
    rows <- d[d$reference_term == ref, ]
    kinds <- rows$kind[order(rows$pos)]
    expect_identical(tail(kinds, 1), "shap")
    if ("remaining" %in% kinds) {
      expect_identical(kinds[length(kinds) - 1], "remaining")
    }
  }
})

test_that("rows are drawn top to bottom in plotting order, SHAP at the bottom", {
  p <- plot_shap_decomposition(fake_glex(), id = 2, predictors = c("hp", "wt"))
  built <- ggplot2::ggplot_build(p)
  tiles <- built$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomTile"), logical(1)))]]
  d <- p$data
  for (panel in unique(tiles$PANEL)) {
    layout <- built$layout$layout
    rows <- d[d$reference_term == as.character(layout$reference_term[layout$PANEL == panel]), ]
    y <- as.numeric(tiles$y[tiles$PANEL == panel])
    expect_equal(y, max(rows$pos) - rows$pos + 1)
  }
})

test_that("narrow facets get more room for value labels", {
  labels <- c("+0.21", "-0.00516")
  expect_gt(label_expansion(labels, n_panels = 3)[1], label_expansion(labels)[1])
})

test_that("SHAP rows are omitted for constrained objects and objects without $shap", {
  gl <- fake_glex()
  gl$constrained <- "max_interaction"
  p <- plot_shap_decomposition(gl, id = 2)
  expect_false("shap" %in% p$data$kind)
  expect_match(p$labels$subtitle, "SHAP values omitted")
  expect_no_error(ggplot2::ggplot_build(p))

  gl <- fake_glex()
  gl$shap <- NULL
  p <- plot_shap_decomposition(gl, id = 2)
  expect_false("shap" %in% p$data$kind)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("glex_explain is deprecated in favour of plot_shap_decomposition", {
  lifecycle::expect_deprecated(p <- glex_explain(fake_glex(), id = 2))
  expect_s3_class(p, "ggplot")
})

test_that("plot_shap_decomposition appearance", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("shap decomposition", plot_shap_decomposition(fake_glex(), id = 2))
  vdiffr::expect_doppelganger(
    "shap decomposition threshold",
    plot_shap_decomposition(fake_glex(), id = 2, threshold = 0.05, predictors = c("hp", "wt"))
  )
})

test_that("the x axis is labeled as the prediction scale", {
  p <- plot_shap_decomposition(fake_glex(), id = 2)
  expect_match(p$labels$x, "^Prediction")
})

test_that("multiclass bars start at their class's intercept", {
  gl <- fake_glex_multiclass()
  gl$intercept <- c(a = 0.2, b = 0.7)
  # hp:wt is split evenly between hp and wt
  gl$shap <- data.table::data.table(
    `hp__class:a` = c(0.425, -0.31),
    `wt__class:a` = c(-0.075, 0.19),
    `hp__class:b` = c(-0.425, 0.31),
    `wt__class:b` = c(0.075, -0.19)
  )
  d <- plot_shap_decomposition(gl, id = 1)$data
  shap <- d[d$kind == "shap", ]
  expect_equal(shap$xleft, ifelse(shap$class == "a", 0.2, 0.7))
  first <- d[d$pos == 1, ]
  expect_equal(first$xleft, ifelse(first$class == "a", 0.2, 0.7))
})

test_that("the separator is drawn only in facets with rows above SHAP", {
  gl <- fake_glex()
  gl$x$z <- 1
  gl$shap$z <- 0
  p <- plot_shap_decomposition(gl, id = 2, predictors = c("hp", "z"))
  hline <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))]]
  expect_identical(hline$data$reference_term, "hp")
  expect_no_error(ggplot2::ggplot_build(p))
})
