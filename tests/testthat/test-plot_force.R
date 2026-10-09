test_that("force layout stacks both signs toward f(x)", {
  et <- explain_terms(fake_glex(), id = 2)
  d <- force_layout(et)
  pos <- d[d$dir == 1, ]
  neg <- d[d$dir == -1, ]
  expect_true(all(d$start < d$end))
  expect_equal(max(pos$end), et$prediction)
  expect_equal(min(neg$start), et$prediction)
  expect_equal(pos$end[nrow(pos)], et$prediction)
  expect_identical(pos$term[nrow(pos)], "wt")
  expect_identical(neg$term[1], "hp")
  expect_equal(sum(pos$m) + sum(neg$m), et$prediction - et$intercept)
})

test_that("force polygons have six points per segment", {
  et <- explain_terms(fake_glex(), id = 2)
  shapes <- force_polygons(force_layout(et), tip = 0.01)
  expect_identical(nrow(shapes), 6L * nrow(et$terms))
  expect_identical(nrow(force_polygons(force_layout(list(terms = et$terms[0, ], prediction = 1)), 0.01)), 0L)
})

test_that("plot_force labels wide segments and counts the rest", {
  p <- plot_force(fake_glex(), id = 2)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption, "^\\d+ terms? too narrow to label$")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_force handles empty, multiclass and constrained objects", {
  gl <- fake_glex()
  gl$m[3, (names(gl$m)) := 0]
  expect_no_error(ggplot2::ggplot_build(plot_force(gl, id = 3)))
  expect_error(plot_force(fake_glex_multiclass(), id = 1), "`class` is required")
  expect_no_error(ggplot2::ggplot_build(plot_force(fake_glex_multiclass(), id = 1, class = "a")))

  gl <- fake_glex()
  gl$remainder <- c(0.5, -0.25, 0.1)
  gl$constrained <- "max_interaction"
  expect_no_error(ggplot2::ggplot_build(plot_force(gl, id = 2)))
})

test_that("plot_force appearance", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("force regression", plot_force(fake_glex(), id = 2))
  vdiffr::expect_doppelganger("force many terms", plot_force(fake_glex_many(), id = 1, max_terms = 8))
})

test_that("segments meeting at f(x) have flat fronts, so colors do not blend there", {
  et <- explain_terms(fake_glex_many(), id = 1, max_terms = 8)
  d <- force_layout(et)
  shapes <- force_polygons(d, tip = 0.05)
  pos_front <- shapes[shapes$group == which(d$dir == 1 & d$at_fx), ]
  neg_front <- shapes[shapes$group == which(d$dir == -1 & d$at_fx), ]
  expect_equal(max(pos_front$x), et$prediction)
  expect_equal(min(neg_front$x), et$prediction)
})

test_that("an all-zero observation still marks E[f] and f(x)", {
  gl <- fake_glex()
  gl$m[3, (names(gl$m)) := 0]
  built <- ggplot2::ggplot_build(plot_force(gl, id = 3))
  expect_identical(built$layout$panel_params[[1]]$x.sec$get_labels(), "E[f] = 19.6, f(x) = 19.6")
})

test_that("the two label rows are far enough apart for two-line labels", {
  p <- plot_force(fake_glex_many(), id = 1, max_terms = 8)
  rows <- sort(unique(abs(p$layers[[2]]$data$y)))
  expect_length(rows, 2)
  expect_gte(diff(rows), 0.75)
})

test_that("labels sharing a row do not overlap at their estimated width", {
  feats <- sprintf("feature_%02d", 1:12)
  m <- data.table::as.data.table(matrix(0.1, nrow = 1, ncol = 12, dimnames = list(NULL, feats)))
  x <- data.table::as.data.table(matrix(0.52, nrow = 1, ncol = 12, dimnames = list(NULL, feats)))
  gl <- structure(
    list(m = m, intercept = 0, x = x, constrained = character(0)),
    class = c("glex", "xgb_components", "list")
  )
  p <- plot_force(gl, id = 1, max_terms = 12)
  labels <- p$layers[[2]]$data
  expect_true(is.numeric(labels$halfwidth) && all(labels$halfwidth > 0))
  for (row in unique(labels$y)) {
    same <- labels[labels$y == row, ]
    same <- same[order(same$mid), ]
    if (nrow(same) > 1) {
      gaps <- diff(same$mid) - (same$halfwidth[-1] + same$halfwidth[-nrow(same)])
      expect_true(all(gaps >= 0))
    }
  }
  expect_match(p$labels$caption, "too narrow to label")
})
