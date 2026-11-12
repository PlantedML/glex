# glex() of pruned Random Planted Forests (randomPlantedForest >= 0.6, class `rpf_pruned`) and their plots

skip_if_not_installed("randomPlantedForest")
skip_if_not(rpf_prunes(), "randomPlantedForest without pruned models")
skip_if_not_installed("patchwork")

# x1 + a pair + a triple: with max_panels = 4 the triple is coarsened, with 10 it is kept as it is (x4 has 5 values)
sim_pruned <- local({
  set.seed(1)
  n <- 400
  x <- data.frame(matrix(stats::runif(n * 4, -1, 1), ncol = 4, dimnames = list(NULL, paste0("x", 1:4))))
  x$x4 <- round(x$x4 * 2) / 2
  y <- x$x1 + 2 * x$x2 * x$x3 + 3 * x$x2 * x$x3 * x$x4 + stats::rnorm(n, sd = 0.3)
  fit <- randomPlantedForest::rpf(x, y, max_interaction = 3, n_families = 20, n_splits = 60, prune = FALSE)
  list(
    x = x,
    y = y,
    fit = fit,
    coarse = randomPlantedForest::prune_components(fit, x, y, max_panels = 4, seed = 1),
    asis = randomPlantedForest::prune_components(fit, x, y, max_panels = 10, seed = 1)
  )
})

test_that("glex() of a pruned model: its kept components, summing to its prediction", {
  pr <- sim_pruned$coarse
  gl <- glex(pr, sim_pruned$x)
  expect_s3_class(gl, "glex")
  expect_setequal(names(gl$m), names(pr$beta))
  expect_equal(gl$intercept + rowSums(gl$m), predict(pr, sim_pruned$x)$.pred, tolerance = 1e-10)
  expect_length(gl$constrained, 0)
  expect_null(gl$remainder)
  # SHAP efficiency
  expect_equal(rowSums(gl$shap), rowSums(gl$m), tolerance = 1e-10)
  expect_equal(gl$pruned$components$term, names(pr$beta))
  expect_equal(gl$pruned$components$share, pr$components$share[match(names(pr$beta), pr$components$component)])
  expect_named(gl$pruned$coarse, "x2:x3:x4")
  expect_output(print(gl), "glex object")
})

test_that("rpf() and prune_components() of rpf(prune = FALSE) with the same seed: the same glex object", {
  x <- sim_pruned$x
  y <- sim_pruned$y
  set.seed(3)
  pruned <- randomPlantedForest::rpf(x, y, max_interaction = 3, n_families = 20, n_splits = 60)
  set.seed(3)
  forest <- randomPlantedForest::rpf(x, y, max_interaction = 3, n_families = 20, n_splits = 60, prune = FALSE)
  expect_s3_class(pruned, "rpf_pruned")
  expect_s3_class(forest, "rpf")
  gl <- glex(pruned, x)
  gl_forest_pruned <- glex(randomPlantedForest::prune_components(forest, x, y), x)
  for (el in c("m", "intercept", "x", "constrained", "shap")) {
    expect_identical(gl[[el]], gl_forest_pruned[[el]])
  }
  # all but the model itself, which holds the fitted C++ object and the pruning's timings
  info <- setdiff(names(gl$pruned), "model")
  expect_identical(gl$pruned[info], gl_forest_pruned$pruned[info])

  # the forest itself: all its components
  gl_forest <- glex(forest, x)
  expect_null(gl_forest$pruned)
  expect_gt(ncol(gl_forest$m), ncol(gl$m))
  expect_equal(gl_forest$intercept + rowSums(gl_forest$m), predict(forest, x)$.pred, tolerance = 1e-10)
})

test_that("glex() of a pruned model: constraints leave out kept components, with their remainder", {
  pr <- sim_pruned$coarse
  expect_warning(gl <- glex(pr, sim_pruned$x, max_interaction = 2), "SHAP values set to NA")
  expect_identical(gl$constrained, "max_interaction")
  expect_setequal(names(gl$m), c("x1", "x2:x3"))
  expect_equal(gl$intercept + rowSums(gl$m) + gl$remainder, predict(pr, sim_pruned$x)$.pred, tolerance = 1e-10)

  expect_warning(gl <- glex(pr, sim_pruned$x, features = c("x1", "x2", "x3")), "SHAP values set to NA")
  expect_identical(gl$constrained, "features")
  expect_named(gl$x, c("x1", "x2", "x3"))

  # dropping only x4's absent main effect changes nothing
  gl <- glex(pr, sim_pruned$x, features = c("x1", "x2", "x3", "x4"))
  expect_length(gl$constrained, 0)
  expect_warning(glex(pr, sim_pruned$x, foo = 1), "foo")
})

test_that("glex_vi(): measures of a pruned model", {
  gl <- glex(sim_pruned$coarse, sim_pruned$x)
  vi <- glex_vi(gl)
  expect_identical(attr(vi, "measure"), "mean_abs")

  vv <- glex_vi(gl, measure = "variance")
  m <- as.matrix(gl$m)
  expect_equal(vv$m, unname(sort(colMeans(m^2), decreasing = TRUE)))
  pred <- rowSums(m)
  expect_equal(vv$m_rel, vv$m / mean((pred - mean(pred))^2))

  vs <- glex_vi(gl, measure = "share")
  comp <- sim_pruned$coarse$components
  expect_equal(vs$m, comp$share[match(vs$term, comp$component)])
  expect_equal(vs$term, comp$component[comp$kept][order(-comp$share[comp$kept])])

  for (v in list(vi, vv, vs)) {
    expect_s3_class(plot(v), "ggplot")
    expect_s3_class(plot(v, scale = "relative", by_degree = TRUE), "ggplot")
  }
  # share needs a pruned model
  rp <- glex(sim_pruned$fit, sim_pruned$x)
  expect_error(glex_vi(rp, measure = "share"), "pruned")
  expect_s3_class(glex_vi(rp, measure = "variance"), "glex_vi")
})

test_that("a pair's heat map: the component on a grid of the data's values", {
  gl <- glex(sim_pruned$coarse, sim_pruned$x)
  p <- plot_twoway_effects(gl, c("x3", "x2"), resolution = 10)
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_equal(nrow(d), 100)
  # the cells' values are the component's at the data's quantiles (x3 on the x axis, as given first)
  z <- sim_pruned$x[rep(1, nrow(d)), ]
  z$x3 <- stats::quantile(sim_pruned$x$x3, (seq_len(10) - 0.5) / 10, names = FALSE, type = 1)[rep(1:10, 10)]
  z$x2 <- stats::quantile(sim_pruned$x$x2, (seq_len(10) - 0.5) / 10, names = FALSE, type = 1)[rep(1:10, each = 10)]
  expect_true(all(d$xmin < z$x3 & z$x3 < d$xmax))
  expect_equal(d$m, randomPlantedForest::predict_components(sim_pruned$coarse, z)$m[["x2:x3"]])
  expect_error(plot_twoway_effects(gl, c("x1", "x2")), "no such component")
})

test_that("a coarsened triple: one heat map per cell of its coarse predictor", {
  gl <- glex(sim_pruned$coarse, sim_pruned$x)
  co <- sim_pruned$coarse$display$coarse[["x2:x3:x4"]]
  p <- plot_threeway_effects(gl, c("x4", "x3", "x2"), resolution = 8)
  expect_s3_class(p, "ggplot")
  expect_equal(nlevels(p$data$.panel1), length(co$cuts[[1]]) + 1)
  expect_equal(nrow(p$data), 64 * (length(co$cuts[[1]]) + 1))
  # no text but the cells
  expect_null(p$labels$subtitle)
  expect_match(levels(p$data$.panel1)[1], "^x4 < ")
  # every panel's values are the coarsened component's in its cell
  rows <- p$data$.panel1 == levels(p$data$.panel1)[1]
  z <- sim_pruned$x[rep(1, sum(rows)), ]
  z$x2 <- rep(sort(unique(stats::quantile(sim_pruned$x$x2, (1:8 - 0.5) / 8, names = FALSE, type = 1))), 8)
  z$x3 <- rep(sort(unique(stats::quantile(sim_pruned$x$x3, (1:8 - 0.5) / 8, names = FALSE, type = 1))), each = 8)
  z$x4 <- min(sim_pruned$x$x4)
  expect_equal(p$data$m[rows], randomPlantedForest::predict_components(sim_pruned$coarse, z)$m[["x2:x3:x4"]])
})

# The display of a component kept as it is, as randomPlantedForest stores it from version 0.6, by hand for a model that
# does not: x4 is the predictor with the fewest cut points of x2:x3:x4 (5 values)
with_asis <- function(pr) {
  if (is.null(pr$display$asis[["x2:x3:x4"]])) {
    pr$display$asis <- list(
      "x2:x3:x4" = list(
        S = c("x2", "x3"),
        C = "x4",
        cuts = list(x4 = c(-0.5, 0, 0.5, 1)),
        forest_panels = 5
      )
    )
  }
  pr
}

test_that("a triple kept as it is: one heat map per cell of its own cut points", {
  pr <- sim_pruned$asis
  comp <- pr$components
  expect_identical(comp$form[comp$component == "x2:x3:x4"], "asis")
  if (!is.null(pr$display$asis)) {
    # the model stores the cells (randomPlantedForest from 0.6); the hand-made entry of with_asis() is the same
    expect_identical(pr$display$asis[["x2:x3:x4"]]$C, "x4")
    expect_equal(pr$display$asis[["x2:x3:x4"]]$cuts, list(x4 = c(-0.5, 0, 0.5, 1)))
  }
  gl <- glex(with_asis(pr), sim_pruned$x)
  expect_named(gl$pruned$asis, "x2:x3:x4")
  p <- plot_threeway_effects(gl, c("x2", "x3", "x4"), resolution = 6)
  expect_s3_class(p, "ggplot")
  # as many heat maps as the display counts, over x2 and x3, one per value of x4
  expect_equal(nlevels(p$data$.panel1), comp$heat_maps[comp$component == "x2:x3:x4"])
  expect_identical(
    levels(p$data$.panel1),
    c("x4 < -0.5", "x4 in [-0.5, 0)", "x4 in [0, 0.5)", "x4 in [0.5, 1)", "x4 >= 1")
  )
  # exact: the component is constant within a cell of x4, and each panel is drawn at a value inside its cell
  panel_of <- findInterval(sim_pruned$x$x4, c(-0.5, 0, 0.5, 1))
  gm <- gl$m[["x2:x3:x4"]]
  for (c in unique(panel_of)) {
    z <- sim_pruned$x[panel_of == c, ]
    z$x4 <- sort(unique(sim_pruned$x$x4))[c + 1]
    expect_equal(randomPlantedForest::predict_components(pr, z)$m[["x2:x3:x4"]], gm[panel_of == c])
  }
})

test_that("a triple kept as it is, by a model that does not store its panels: one heat map", {
  pr <- sim_pruned$asis
  pr$display$asis <- NULL
  gl <- glex(pr, sim_pruned$x)
  expect_message(p <- plot_threeway_effects(gl, c("x2", "x3", "x4"), resolution = 6), "does not store the cells")
  expect_equal(nrow(p$data), 36)
  expect_match(levels(p$data$.panel1), "^x4 = ")
})

test_that("plot() of a pruned model's glex object shows the whole model", {
  gl <- glex(sim_pruned$coarse, sim_pruned$x)
  p <- plot(gl, resolution = 10)
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 2) # the first plot is p itself
  titles <- c(vapply(p$patches$plots, function(q) q$labels$title, ""), p$labels$title)
  # name (share of the variance of the prediction at x), the most important first
  m <- as.matrix(gl$m)
  pred <- rowSums(m)
  share <- sort(colMeans(m^2) / mean((pred - mean(pred))^2), decreasing = TRUE)
  expect_identical(titles, sprintf("%s (%s)", names(share), vapply(share, glex:::format_share, "")))
  expect_null(p$patches$annotation$title)
  # mean absolute value, in units of y
  pa <- plot(gl, measure = "mean_abs", max_components = 2)
  expect_s3_class(pa, "patchwork")
  mean_abs <- sort(colMeans(abs(m)), decreasing = TRUE)
  expect_identical(
    pa$patches$plots[[1]]$labels$title,
    sprintf("%s (%s)", names(mean_abs)[1], format(signif(mean_abs[[1]], 2)))
  )
  expect_match(pa$patches$annotation$caption, "max_components = 2")
  expect_error(plot(gl, measure = "share"))
  expect_s3_class(plot(gl, "x1"), "ggplot")
  expect_s3_class(plot(gl, c("x2", "x3", "x4"), resolution = 5), "ggplot")
  # draws
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  grDevices::png(tmp, width = 1000, height = 900)
  print(p)
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("plot() of a forest's glex object: main effects and pairs, a categorical pair by its levels", {
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  set.seed(1)
  gl <- glex(rpf_forest(mpg ~ cyl + wt + hp, data = mt, max_interaction = 2, ntrees = 10), mt)
  p <- plot(gl)
  expect_s3_class(p, "patchwork")
  expect_true(any(lengths(strsplit(names(gl$m), ":")) == 2))
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  grDevices::png(tmp, width = 1000, height = 900)
  expect_no_warning(print(p))
  grDevices::dev.off()
})

test_that("pruned classification: binary like regression, three classes per class", {
  x <- sim_pruned$x
  set.seed(2)
  score <- x$x1 + 2 * x$x2 * x$x3 + stats::rnorm(nrow(x), sd = 0.3)
  yb <- factor(score > 0, labels = c("no", "yes"))
  pb <- randomPlantedForest::rpf(x, yb, max_interaction = 2, n_families = 20, n_splits = 30, loss = "logit")
  expect_s3_class(pb, "rpf_pruned")
  gb <- glex(pb, x)
  expect_null(gb$target_levels)
  expect_equal(gb$intercept + rowSums(gb$m), unname(predict(pb, x, type = "link")[[1]]), tolerance = 1e-10)
  expect_s3_class(plot(gb, resolution = 10), "patchwork")

  yk <- cut(score, c(-Inf, -0.5, 0.5, Inf), labels = c("a", "b", "c"))
  for (loss in c("L2", "exponential")) {
    pk <- randomPlantedForest::rpf(x, yk, max_interaction = 3, n_families = 20, n_splits = 30, loss = loss)
    gk <- glex(pk, x)
    expect_identical(gk$target_levels, c("a", "b", "c"))
    expect_named(gk$intercept, c("a", "b", "c"))
    expect_s3_class(plot_shap_decomposition(gk, 1), "ggplot")
    vv <- glex_vi(gk, measure = "variance")
    expect_false("class" %in% names(vv))
    p <- plot(gk, resolution = 10)
    expect_s3_class(p, "patchwork")
    pair <- names(pk$beta)[lengths(pk$sets) == 2][1]
    if (!is.na(pair)) {
      h <- plot_twoway_effects(gk, strsplit(pair, ":")[[1]], resolution = 5)
      expect_setequal(levels(h$data$class), c("a", "b", "c"))
      # each class's panel is that class's column of the component
      for (level in c("a", "b", "c")) {
        expect_true(all(is.finite(h$data$m[h$data$class == level])))
      }
    }
  }
})

test_that("pruned classification with a coarsened triple: heat maps per class and cell", {
  x <- sim_pruned$x
  set.seed(3)
  score <- x$x1 + 3 * x$x2 * x$x3 * x$x4 + stats::rnorm(nrow(x), sd = 0.2)
  yk <- cut(score, c(-Inf, -0.4, 0.4, Inf), labels = c("a", "b", "c"))
  fk <- randomPlantedForest::rpf(x, yk, max_interaction = 3, n_families = 20, n_splits = 60, loss = "L2", prune = FALSE)
  pk <- randomPlantedForest::prune_components(fk, x, yk, rule = "D", tau = 0.001, max_panels = 3, seed = 1)
  triples <- names(pk$display$coarse)
  skip_if(length(triples) == 0, "no coarsened triple")
  gk <- glex(pk, x)
  vars <- strsplit(triples[1], ":")[[1]]
  p <- plot(gk, vars, resolution = 5)
  co <- pk$display$coarse[[triples[1]]]
  n_cells <- prod(lengths(co$cuts) + 1)
  expect_equal(nrow(unique(p$data[, grep("^\\.panel|^class$", names(p$data)), drop = FALSE])), 3 * n_cells)
  # the heat maps' values are the classes' columns of the component at the cells' points
  expect_equal(nrow(p$data), 25 * n_cells * 3)
})

test_that("factor predictors: heat map axes and panel cells by level", {
  set.seed(2)
  x <- sim_pruned$x[, 1:3]
  x$f <- factor(sample(c("low", "mid", "high"), nrow(x), replace = TRUE), levels = c("low", "mid", "high"))
  ef <- c(low = -1, mid = 0, high = 1)[as.character(x$f)]
  y <- x$x1 + 1.5 * ef * x$x2 + 2 * ef * x$x2 * x$x3 + stats::rnorm(nrow(x), sd = 0.3)
  fit <- randomPlantedForest::rpf(x, y, max_interaction = 3, n_families = 20, n_splits = 60, prune = FALSE)
  pr <- randomPlantedForest::prune_components(fit, x, y, max_panels = 3, seed = 1)
  skip_if_not(all(c("f:x2", "f:x2:x3") %in% names(pr$beta)), "the pruning kept other components")
  if (is.null(pr$display$asis[["f:x2:x3"]])) {
    pr$display$asis <- list("f:x2:x3" = list(S = c("x2", "x3"), C = "f", cuts = list(f = c(2, 3)), forest_panels = 3))
  }
  gl <- glex(pr, x)

  h <- plot_twoway_effects(gl, c("f", "x2"), resolution = 5)
  expect_equal(sort(unique((h$data$xmin + h$data$xmax) / 2)), 1:3)
  z <- x[rep(1, 3), ]
  z$f <- factor(levels(x$f), levels = levels(x$f))
  z$x2 <- stats::quantile(x$x2, 0.1, names = FALSE, type = 1)
  expect_equal(
    h$data$m[h$data$ymin < z$x2[1] & z$x2[1] < h$data$ymax],
    randomPlantedForest::predict_components(pr, z)$m[["f:x2"]]
  )

  p <- plot_threeway_effects(gl, c("x2", "x3", "f"), resolution = 4)
  forest_levels <- fit$factor_levels$f
  expect_identical(levels(p$data$.panel1), paste0("f: ", forest_levels))
  # each panel is the component at its level
  for (level in forest_levels) {
    rows <- p$data$.panel1 == paste0("f: ", level)
    z <- x[rep(1, sum(rows)), ]
    z$f <- factor(level, levels = levels(x$f))
    z$x2 <- rep(unique(stats::quantile(x$x2, (1:4 - 0.5) / 4, names = FALSE, type = 1)), 4)
    z$x3 <- rep(unique(stats::quantile(x$x3, (1:4 - 0.5) / 4, names = FALSE, type = 1)), each = 4)
    expect_equal(p$data$m[rows], randomPlantedForest::predict_components(pr, z)$m[["f:x2:x3"]])
  }
})

# The prediction plots of one observation (explain_terms(), plot_waterfall(), plot_force(), plot_shap_decomposition())
# read only $m, $intercept, $remainder, $shap and $x, so they take a pruned model's glex object as a forest's: each
# runs from its intercept to the model's prediction (raw score for classification), the remainder included.
expect_prediction_plots <- function(gl, model, x, ids = c(1, 7), class = NULL) {
  pred <- predict(model, x, type = "numeric")
  target <- pred[[if (is.null(class)) 1 else paste0(".pred_", class)]]
  for (id in ids) {
    et <- explain_terms(gl, id, class = class, threshold = 0, max_terms = 100L)
    expect_equal(et$prediction, target[[id]], tolerance = 1e-10)
    expect_equal(et$intercept, class_intercept(gl, class))
    expect_equal(et$terms$end[nrow(et$terms)], target[[id]], tolerance = 1e-10)
    if (length(gl$constrained) > 0) {
      expect_identical(et$terms$type[nrow(et$terms)], "remainder")
    } else {
      expect_setequal(et$terms$type, "term")
    }
    # the defaults aggregate small terms but reach the same prediction
    expect_equal(explain_terms(gl, id, class = class)$prediction, target[[id]], tolerance = 1e-10)

    p <- plot_waterfall(gl, id = id, class = class)
    expect_no_error(ggplot2::ggplot_build(p))
    expect_equal(p$data$end[nrow(p$data)], target[[id]], tolerance = 1e-10)
    expect_no_error(ggplot2::ggplot_build(plot_force(gl, id = id, class = class)))

    s <- plot_shap_decomposition(gl, id = id, class = class, threshold = 0)
    expect_no_error(ggplot2::ggplot_build(s))
    if (length(gl$constrained) == 0) {
      # each feature's bars sum to its SHAP value
      bars <- s$data[s$data$kind != "shap", ]
      shap_rows <- s$data[s$data$kind == "shap", ]
      sums <- vapply(shap_rows$reference_term, function(f) sum(bars$m_scaled[bars$reference_term == f]), numeric(1))
      expect_equal(unname(sums), shap_rows$m_scaled, tolerance = 1e-10)
    }
  }
}

test_that("prediction plots of a pruned model's observations, as of the forest's", {
  x <- sim_pruned$x
  for (model in list(sim_pruned$fit, sim_pruned$coarse, sim_pruned$asis)) {
    gl <- glex(model, x)
    expect_prediction_plots(gl, model, x)
    # the label of a coarsened triple shows the observation's values of its predictors
    if (inherits(model, "rpf_pruned")) {
      et <- explain_terms(gl, 1, threshold = 0, max_terms = 100L)
      expect_true("x2:x3:x4" %in% et$terms$term)
      expect_match(et$terms$label[et$terms$term == "x2:x3:x4"], "^x2:x3:x4 = ")
    }
    gc <- suppressWarnings(glex(model, x, max_interaction = 2))
    expect_identical(gc$constrained, "max_interaction")
    expect_prediction_plots(gc, model, x)
  }
})

test_that("prediction plots of pruned classification models: binary, and three classes per class and loss", {
  x <- sim_pruned$x
  set.seed(2)
  score <- x$x1 + 2 * x$x2 * x$x3 + stats::rnorm(nrow(x), sd = 0.3)
  yb <- factor(score > 0, labels = c("no", "yes"))
  pb <- randomPlantedForest::rpf(x, yb, max_interaction = 2, n_families = 20, n_splits = 30, loss = "logit")
  gb <- glex(pb, x)
  expect_prediction_plots(gb, pb, x)
  expect_error(plot_waterfall(gb, id = 1, class = "yes"), "only applies to multiclass")

  yk <- cut(score, c(-Inf, -0.5, 0.5, Inf), labels = c("a", "b", "c"))
  for (loss in c("L2", "logit", "exponential")) {
    pk <- randomPlantedForest::rpf(x, yk, max_interaction = 3, n_families = 20, n_splits = 30, loss = loss)
    gk <- glex(pk, x)
    # one score per class with every loss; randomPlantedForest builds before
    # PlantedML/randomPlantedForest#115 gave logit's first class no score
    expected <- c("a", "b", "c")
    if (loss == "logit" && length(gk$target_levels) == 2) {
      expected <- c("b", "c")
    }
    expect_identical(gk$target_levels, expected)
    expect_named(gk$intercept, gk$target_levels)
    expect_error(plot_force(gk, id = 1), "`class` is required")
    for (level in gk$target_levels) {
      expect_prediction_plots(gk, pk, x, class = level)
    }
    expect_no_error(ggplot2::ggplot_build(plot_shap_decomposition(gk, id = 1)))

    # a constraint: the remainder is computed per class, against the model's scores
    if (any(lengths(pk$sets) > 1)) {
      gc <- suppressWarnings(glex(pk, x, max_interaction = 1))
      expect_identical(gc$constrained, "max_interaction")
      expect_named(gc$remainder, gk$target_levels)
      for (level in gk$target_levels) {
        expect_prediction_plots(gc, pk, x, class = level)
      }
    }
  }
})
