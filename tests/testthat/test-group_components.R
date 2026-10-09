make_dummy_task <- function(n = 300, encoding = c("one-hot", "treatment")) {
  encoding <- match.arg(encoding)
  set.seed(1)
  f <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  x1 <- rnorm(n)
  y <- c(a = 0, b = 2, c = -1)[f] +
    x1 +
    c(a = 1, b = 0, c = -1)[f] * x1 +
    rnorm(n, sd = 0.3)
  x <- if (encoding == "one-hot") {
    cbind(model.matrix(~ f - 1), x1)
  } else {
    cbind(model.matrix(~f)[, -1, drop = FALSE], x1)
  }
  list(x = x, y = y, f = f)
}

fit_xgb <- function(x, y) {
  xgboost::xgb.train(
    params = xgboost::xgb.params(max_depth = 3, learning_rate = 0.3, nthread = 1),
    data = xgboost::xgb.DMatrix(x, label = y, nthread = 1),
    nrounds = 30
  )
}

test_that("grouping one-hot dummies is exact and collapses terms", {
  task <- make_dummy_task(encoding = "one-hot")
  xg <- fit_xgb(task$x, task$y)
  gl <- glex(xg, task$x)

  grouped <- group_components(gl, groups = list(f = c("fa", "fb", "fc")))

  # 15 dummy-level terms collapse to the three conceptual ones
  expect_setequal(names(grouped$m), c("f", "x1", "f:x1"))

  # regrouping is exact: components still sum to the model prediction
  pred <- predict(xg, task$x, outputmargin = TRUE)
  expect_equal(
    unname(grouped$intercept + rowSums(grouped$m)),
    unname(pred),
    tolerance = 1e-5
  )

  # the group's main effect is the sum of all dummy-only terms
  dummy_only <- names(gl$m)[!grepl("x1", names(gl$m), fixed = TRUE)]
  expect_equal(
    grouped$m$f,
    rowSums(as.matrix(gl$m[, dummy_only, with = FALSE]))
  )

  # SHAP: group value is the sum of its members, efficiency preserved
  expect_setequal(names(grouped$shap), c("f", "x1"))
  expect_equal(grouped$shap$f, rowSums(as.matrix(gl$shap[, c("fa", "fb", "fc")])))
  expect_equal(grouped$shap$x1, gl$shap$x1)
  expect_equal(
    unname(grouped$intercept + rowSums(grouped$shap)),
    unname(pred),
    tolerance = 1e-5
  )

  # x: factor reconstructed from the one-hot columns, ungrouped column untouched
  expect_identical(levels(grouped$x$f), c("fa", "fb", "fc"))
  expect_identical(as.integer(grouped$x$f), as.integer(task$f))
  expect_identical(grouped$x$x1, gl$x$x1)
})

test_that("treatment-coded dummies reconstruct a factor with a base level", {
  task <- make_dummy_task(encoding = "treatment")
  xg <- fit_xgb(task$x, task$y)
  gl <- glex(xg, task$x)

  grouped <- group_components(gl, groups = list(f = c("fb", "fc")))

  expect_setequal(names(grouped$m), c("f", "x1", "f:x1"))
  expect_identical(levels(grouped$x$f), c("(base)", "fb", "fc"))
  expect_identical(
    grouped$x$f == "(base)",
    task$f == "a"
  )
})

test_that("non-dummy groups aggregate terms but yield NA in x", {
  set.seed(1)
  x <- as.matrix(mtcars[, c("cyl", "hp", "wt")])
  xg <- fit_xgb(x, mtcars$mpg)
  gl <- glex(xg, x)

  grouped <- group_components(gl, groups = list(engine = c("cyl", "hp")))

  expect_setequal(names(grouped$m), c("engine", "wt", "engine:wt"))
  expect_equal(rowSums(grouped$m), rowSums(gl$m))
  expect_true(all(is.na(grouped$x$engine)))
  expect_identical(grouped$x$wt, gl$x$wt)
})

test_that("multiclass class suffixes are preserved", {
  skip_if_not_installed("randomPlantedForest", minimum_version = "0.5.0.9000")

  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  rpk <- randomPlantedForest::rpf(
    cyl ~ mpg + hp + wt,
    data = mt,
    max_interaction = 2
  )
  gl <- glex(rpk, mt)

  grouped <- group_components(gl, groups = list(g = c("mpg", "hp")))

  base <- unique(split_names(names(grouped$m), target_index = 1))
  expect_setequal(base, c("g", "wt", "g:wt"))
  for (level in gl$target_levels) {
    cols_old <- grepl(paste0("__class:", level), names(gl$m), fixed = TRUE)
    cols_new <- grepl(paste0("__class:", level), names(grouped$m), fixed = TRUE)
    expect_equal(
      rowSums(as.matrix(grouped$m[, cols_new, with = FALSE])),
      rowSums(as.matrix(gl$m[, cols_old, with = FALSE])),
      info = paste("class", level)
    )
  }
})

test_that("invalid groups error, constrained shap passes through", {
  set.seed(1)
  x <- as.matrix(mtcars[, c("cyl", "hp", "wt")])
  xg <- fit_xgb(x, mtcars$mpg)
  gl <- glex(xg, x)

  expect_error(
    group_components(gl, groups = list(a = c("cyl", "hp"), b = c("hp", "wt"))),
    "at most one group"
  )
  expect_error(
    group_components(gl, groups = list(a = c("cyl", "nope"))),
    "Unknown features"
  )
  expect_error(
    group_components(gl, groups = list(wt = c("cyl", "hp"))),
    "collide"
  )

  gl_con <- suppressWarnings(glex(xg, x, max_interaction = 1))
  grouped <- group_components(gl_con, groups = list(engine = c("cyl", "hp")))
  expect_identical(grouped$shap, NA)
  expect_identical(grouped$constrained, "max_interaction")
})

test_that("dummy_groups derives groups from model.matrix naming", {
  task <- make_dummy_task(encoding = "one-hot")
  xg <- fit_xgb(task$x, task$y)
  gl <- glex(xg, task$x)
  d <- data.frame(f = task$f, x1 = task$x[, "x1"])

  groups <- dummy_groups(gl, d)
  expect_identical(groups, list(f = c("fa", "fb", "fc")))

  grouped <- group_components(gl, groups)
  expect_setequal(names(grouped$m), c("f", "x1", "f:x1"))
})

test_that("dummy_groups skips dropped reference levels (treatment coding)", {
  task <- make_dummy_task(encoding = "treatment")
  xg <- fit_xgb(task$x, task$y)
  gl <- glex(xg, task$x)
  d <- data.frame(f = task$f, x1 = task$x[, "x1"])

  groups <- dummy_groups(gl, d)
  expect_identical(groups, list(f = c("fb", "fc")))
})

test_that("dummy_groups supports custom naming functions", {
  set.seed(1)
  n <- 200
  f <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  x1 <- rnorm(n)
  y <- c(a = 0, b = 2, c = -1)[f] + x1 + rnorm(n)
  x <- cbind(model.matrix(~ f - 1), x1)
  colnames(x) <- sub("^f", "f_", colnames(x))

  xg <- fit_xgb(x, y)
  gl <- glex(xg, x)

  # default naming does not match the "_"-separated columns
  expect_error(
    suppressMessages(dummy_groups(gl, data.frame(f = f))),
    "No factor levels matched"
  )

  groups <- dummy_groups(
    gl,
    data.frame(f = f),
    naming = function(feature, levels) paste(feature, levels, sep = "_")
  )
  expect_identical(groups, list(f = c("f_a", "f_b", "f_c")))
})

test_that("dummy_groups messages on unmatched factors, errors without factors", {
  task <- make_dummy_task(encoding = "one-hot")
  xg <- fit_xgb(task$x, task$y)
  gl <- glex(xg, task$x)

  d <- data.frame(f = task$f, other = factor(c("p", "q"))[rep(1:2, 150)])
  expect_message(groups <- dummy_groups(gl, d), "No encoded columns found for: other")
  expect_named(groups, "f")

  expect_error(
    dummy_groups(gl, data.frame(x1 = task$x[, "x1"])),
    "no factor or character columns"
  )
})

test_that("reconstructed factors plot, groups without x values error clearly", {
  task <- make_dummy_task(encoding = "one-hot")
  xg <- fit_xgb(task$x, task$y)
  gl <- glex(xg, task$x)

  grouped <- group_components(gl, groups = list(f = c("fa", "fb", "fc")))
  expect_s3_class(plot_main_effect(grouped, "f"), "ggplot")
  expect_s3_class(plot_twoway_effects(grouped, c("f", "x1")), "ggplot")

  mixed <- group_components(gl, groups = list(g = c("fa", "x1")))
  expect_true(all(is.na(mixed$x$g)))
  expect_error(plot_main_effect(mixed, "g"), "No values in `x` for \"g\"")
  expect_error(plot_twoway_effects(mixed, c("g", "fb")), "No values in `x` for \"g\"")
})

test_that("group names with ':' and non-glex objects are rejected", {
  task <- make_dummy_task(encoding = "one-hot")
  gl <- glex(fit_xgb(task$x, task$y), task$x)
  expect_error(group_components(gl, groups = list(`a:b` = c("fa", "fb"))), "must not contain")
  expect_error(group_components(gl$m, groups = list(f = c("fa", "fb"))), "glex")
})

test_that("grouping a factor column of x yields NA instead of crashing", {
  skip_if_not_installed("xgboost", minimum_version = "3.0.0")
  set.seed(2)
  n <- 200
  d <- data.frame(v = factor(sample(c("0", "1"), n, TRUE)), x1 = rnorm(n))
  y <- (d$v == "1") * 2 + d$x1 + rnorm(n, sd = 0.3)
  xg <- xgboost::xgboost(d, y, nrounds = 10, max_depth = 2, nthreads = 1)
  gl <- glex(xg, d)
  grouped <- group_components(gl, groups = list(vv = c("v", "x1")))
  expect_true(all(is.na(grouped$x$vv)))
  expect_equal(rowSums(grouped$m), rowSums(gl$m))
})

test_that("dummy_groups ignores non-dummy columns and accepts character columns", {
  set.seed(3)
  n <- 200
  d <- data.frame(
    x = sample(c("1", "2", "3"), n, TRUE),
    x1 = rnorm(n),
    stringsAsFactors = FALSE
  )
  y <- c(`1` = 0, `2` = 2, `3` = -1)[d$x] + d$x1 + rnorm(n, sd = 0.3)
  # treatment-like coding drops the dummy for level "1", so the numeric column
  # `x1` is what the naming convention would (wrongly) match for that level
  x <- cbind(model.matrix(~ x - 1, d)[, c("x2", "x3")], x1 = d$x1)
  gl <- glex(fit_xgb(x, y), x)
  expect_identical(dummy_groups(gl, d), list(x = c("x2", "x3")))
})
