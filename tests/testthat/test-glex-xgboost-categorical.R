make_cat_data <- function(n = 300, seed = 7) {
  set.seed(seed)
  df <- data.frame(
    age = rnorm(n),
    bmi = rnorm(n),
    cat = factor(sample(c("lo", "mid", "hi", "x"), n, TRUE), levels = c("lo", "mid", "hi", "x"))
  )
  df$y <- as.integer(df$age * (df$cat %in% c("hi", "x")) + 0.5 * df$bmi + rnorm(n, sd = 0.3) > 0)
  df
}

expect_sum_identity <- function(model, x, ...) {
  gl <- suppressMessages(glex(model, x, ...))
  expect_s3_class(gl$x$cat, "factor")
  expect_false(isTRUE(gl$constrained))
  margin <- xgb_margin(model, x)
  expect_equal(unname(gl$intercept + rowSums(gl$m)), unname(margin), tolerance = 1e-5)
  expect_equal(unname(gl$intercept + rowSums(gl$shap)), unname(margin), tolerance = 1e-5)
  gl
}

test_that("categorical xgboost() model decomposes to its margin", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  model <- xgboost(x, factor(df$y), nrounds = 20, max_depth = 3, verbosity = 0)

  gl <- expect_sum_identity(model, x)
  expect_true("cat" %in% names(gl$m))
  expect_true(any(grepl("cat", names(gl$m)[get_degree(names(gl$m)) == 2])))
  expect_sum_identity(model, x, weighting_method = "path-dependent")
  # new observations, including levels the model never split on
  expect_sum_identity(model, make_cat_data(seed = 8)[, c("age", "bmi", "cat")])
})

test_that("categorical xgb.train() model on a DMatrix from a data.frame decomposes", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  dm <- xgb.DMatrix(x, label = df$y)
  model <- xgb.train(params = list(objective = "binary:logistic", max_depth = 3), data = dm, nrounds = 20)
  expect_sum_identity(model, x)
})

test_that("factor columns plot as categorical", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  model <- xgboost(x, factor(df$y), nrounds = 20, max_depth = 3, verbosity = 0)
  gl <- suppressMessages(glex(model, x))

  expect_s3_class(plot_main_effect(gl, "cat"), "ggplot")
  expect_s3_class(plot_twoway_effects(gl, c("age", "cat")), "ggplot")
  expect_s3_class(glex_vi(gl), "glex_vi")
})

test_that("categorical models validate x", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  model <- xgboost(x, factor(df$y), nrounds = 5, max_depth = 3, verbosity = 0)

  expect_error(glex(model, as.matrix(transform(x, cat = as.integer(cat)))), "must be a data.frame with factor columns")
  expect_error(glex(model, transform(x, cat = as.integer(cat))), "must be factors: cat")
  expect_error(glex(model, x[, c("age", "bmi")]), "missing from `x`: cat")
  expect_error(glex(model, transform(x, cat = factor(cat, levels = c("lo", "mid")))), "Levels must match")
  expect_error(glex(model, x, weighting_method = "empirical"), "does not support categorical splits")
})

# xgboost compares in float32; mirror that when walking the parsed trees in R
lt_float <- function(a, b) {
  as_float <- function(z) {
    readBin(writeBin(as.numeric(z), raw(), size = 4), "numeric", size = 4, n = length(z))
  }
  as_float(a) < as_float(b)
}

walk_tree <- function(tree, row) {
  node <- 0L
  repeat {
    r <- tree[tree$Node == node, ]
    if (r$Feature == "Leaf") {
      return(node)
    }
    value <- row[[r$Feature]]
    categories <- r$Categories[[1]]
    yes <- if (length(categories)) value %in% categories else lt_float(value, r$Split)
    node <- if (yes) r$Yes else r$No
  }
}

test_that("parsed categorical trees route observations to the same leaves as xgboost", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  model <- xgboost(x, factor(df$y), nrounds = 10, max_depth = 3, verbosity = 0)
  trees <- xgb_trees(model)$trees
  expect_gt(sum(lengths(trees$Categories) > 0), 0)

  coded <- x
  coded$cat <- as.integer(coded$cat) - 1L
  leaves <- sapply(0:max(trees$Tree), function(k) {
    tree <- trees[trees$Tree == k, ]
    vapply(seq_len(nrow(coded)), function(i) walk_tree(tree, coded[i, ]), integer(1))
  })
  expect_equal(unname(leaves), unname(predict(model, x, type = "leaf")))
})

test_that("a binary factor decomposes identically to its integer-coded twin", {
  # {0} vs {1} category sets and a threshold between the codes induce the same
  # partitions, so xgboost fits the same trees and glex must return the same components
  set.seed(11)
  n <- 300
  df <- data.frame(age = rnorm(n), bmi = rnorm(n), grp = factor(sample(c("a", "b"), n, TRUE)))
  y <- factor(df$age * (df$grp == "b") + 0.5 * df$bmi + rnorm(n, 0.3) > 0)
  coded <- as.matrix(transform(df, grp = as.integer(grp) - 1L))
  model_factor <- xgboost(df, y, nrounds = 20, max_depth = 3, verbosity = 0)
  model_coded <- xgboost(coded, y, nrounds = 20, max_depth = 3, verbosity = 0)
  expect_gt(sum(lengths(xgb_trees(model_factor)$trees$Categories) > 0), 0)
  expect_equal(unname(predict(model_factor, df, type = "raw")), unname(predict(model_coded, coded, type = "raw")))

  for (method in c("fastpd", "path-dependent")) {
    gl_factor <- suppressMessages(glex(model_factor, df, weighting_method = method))
    gl_coded <- suppressMessages(glex(model_coded, coded, weighting_method = method))
    expect_setequal(names(gl_factor$m), names(gl_coded$m))
    expect_equal(gl_factor$intercept, gl_coded$intercept)
    expect_equal(as.matrix(gl_factor$m[, names(gl_coded$m), with = FALSE]), as.matrix(gl_coded$m), tolerance = 1e-12)
    expect_equal(as.matrix(gl_factor$shap), as.matrix(gl_coded$shap), tolerance = 1e-12)
  }
})

# Rewrite every threshold split on feature `index` (1-based) of a model fit on
# 0-based codes as a categorical split with the same partition: xgboost sends
# category-set members right, so the set is the complement of `code < threshold`
categorical_twin <- function(model, index, n_levels) {
  model_json <- jsonlite::fromJSON(rawToChar(xgboost::xgb.save.raw(model, raw_format = "json")), simplifyVector = FALSE)
  learner <- model_json$learner
  types <- rep(list("float"), as.integer(learner$learner_model_param$num_feature))
  types[[index]] <- "c"
  learner$feature_types <- types
  learner$gradient_booster$model$trees <- lapply(learner$gradient_booster$model$trees, function(tree) {
    nodes <- which(unlist(tree$split_indices) == index - 1L & unlist(tree$left_children) != -1L)
    codes <- integer(0)
    sizes <- integer(0)
    for (node in nodes) {
      members <- (seq_len(n_levels) - 1L)[!lt_float(seq_len(n_levels) - 1L, tree$split_conditions[[node]])]
      codes <- c(codes, members)
      sizes <- c(sizes, length(members))
      tree$split_type[[node]] <- 1L
      tree$split_conditions[[node]] <- 1e-45
    }
    tree$categories <- as.list(codes)
    tree$categories_nodes <- as.list(nodes - 1L)
    tree$categories_segments <- as.list(c(0L, cumsum(sizes))[seq_along(sizes)])
    tree$categories_sizes <- as.list(sizes)
    tree
  })
  model_json$learner <- learner
  xgboost::xgb.load.raw(charToRaw(jsonlite::toJSON(model_json, auto_unbox = TRUE, digits = NA, always_decimal = TRUE)))
}

test_that("multi-level categorical splits decompose identically to their threshold twin", {
  df <- make_cat_data()
  coded <- as.matrix(transform(df[, c("age", "bmi", "cat")], cat = as.integer(cat) - 1L))
  model_coded <- xgboost(coded, factor(df$y), nrounds = 20, max_depth = 3, verbosity = 0)
  model_twin <- categorical_twin(model_coded, index = 3L, n_levels = 4L)

  x <- df[, c("age", "bmi", "cat")]
  twin_trees <- xgb_trees(model_twin)
  expect_identical(twin_trees$categorical, "cat")
  expect_gt(sum(lengths(twin_trees$trees$Categories) > 1), 0)
  expect_equal(unname(predict(model_twin, x, outputmargin = TRUE)), unname(predict(model_coded, coded, type = "raw")))

  for (method in c("fastpd", "path-dependent")) {
    gl_twin <- suppressMessages(glex(model_twin, x, weighting_method = method))
    gl_coded <- suppressMessages(glex(model_coded, coded, weighting_method = method))
    expect_setequal(names(gl_twin$m), names(gl_coded$m))
    expect_equal(gl_twin$intercept, gl_coded$intercept)
    expect_equal(as.matrix(gl_twin$m[, names(gl_coded$m), with = FALSE]), as.matrix(gl_coded$m), tolerance = 1e-12)
  }
})
