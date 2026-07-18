# These tests crash R on Windows: randomPlantedForest's purify_3() has an
# out-of-bounds read (grid sized lim_list[dim-1].size() instead of .size() - 1
# in src/lib/rpf.cpp, read at gridPoint + 1) that these tests happen to trigger.
# Fixed upstream in https://github.com/PlantedML/randomPlantedForest/pull/61 —
# remove the skips once that (or a minimal fix) is merged.

# For classification, rpf decomposes the *raw score*, which `predict(type = "numeric")`
# returns. The default `type = "prob"` applies rpf's response function -- a clamp to
# [0, 1] for `loss = "L2"`, the inverse link for `"logit"` / `"exponential"` -- which the
# decomposition knows nothing about, so the components never sum to it. These tests used
# to compare against `type = "prob"` and needed tolerances loose enough (0.03, 0.8) to
# swallow that gap, plus a min() over both class columns to avoid committing to one.
# Against the raw score the identity is exact.
#
# There is deliberately no `weighting_method` here: it selects how glex weights leaves
# when *it* walks the trees, which it only does for xgboost and ranger. rpf purifies its
# own components in C++, so `glex.rpf()` ignores the argument entirely. Two tests used to
# pass "fastpd" and "path-dependent" and assert the same thing twice.

test_that("rpf binary: sum identity matches the predicted raw score", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  rp <- randomPlantedForest::rpf(
    y ~ x1 + x2 + x3,
    data = xdat,
    max_interaction = 3
  )
  gl <- glex::glex(rp, xdat)

  expect_equal(
    unname(gl$intercept + rowSums(gl$m)),
    unname(predict(rp, xdat, type = "numeric")[[1]]),
    tolerance = 1e-8
  )
})

test_that("rpf multiclass: classwise sum identity holds up to the class intercept", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  rp <- randomPlantedForest::rpf(
    yk ~ x1 + x2 + x3,
    data = xdat,
    max_interaction = 3
  )
  gl <- glex::glex(rp, xdat)
  pred <- predict(rp, xdat, type = "numeric")

  # Each class has its own decomposition terms ending in "__class:<level>", but rpf
  # reports a single intercept for all classes rather than one per class. The class terms
  # therefore reconstruct that class's raw score exactly up to an additive constant -- the
  # class intercept we never receive. Assert exactly that: the per-observation residual is
  # constant within a class, which is much stronger than bounding its magnitude.
  for (level in gl$target_levels) {
    idx <- grepl(paste0("__class:", level), names(gl$m), fixed = TRUE)
    score <- unname(
      gl$intercept + rowSums(as.matrix(gl$m[, idx, with = FALSE]))
    )
    residual <- score - pred[[paste0(".pred_", level)]]

    expect_equal(
      max(residual) - min(residual),
      0,
      tolerance = 1e-8,
      info = paste("class", level)
    )
  }
})

test_that("rpf: shap is derived from components and satisfies efficiency", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  rp <- randomPlantedForest::rpf(
    mpg ~ cyl + hp + wt,
    data = mtcars,
    max_interaction = 3
  )
  gl <- glex::glex(rp, mtcars)

  expect_identical(gl$constrained, character(0))
  expect_false(anyNA(gl$shap))
  expect_equal(
    unname(gl$intercept + rowSums(gl$shap)),
    unname(predict(rp, mtcars)[[1]]),
    tolerance = 1e-6
  )
})

test_that("rpf: constrained decompositions invalidate shap", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  rp <- randomPlantedForest::rpf(
    mpg ~ cyl + hp + wt,
    data = mtcars,
    max_interaction = 3
  )

  expect_warning(
    gl_mi <- glex::glex(rp, mtcars, max_interaction = 1),
    "efficiency property"
  )
  expect_identical(gl_mi$constrained, "max_interaction")
  expect_identical(gl_mi$shap, NA)
  expect_false(anyNA(gl_mi$m))

  expect_warning(
    gl_ft <- glex::glex(rp, mtcars, features = c("hp", "wt")),
    "efficiency property"
  )
  expect_identical(gl_ft$constrained, "features")
  expect_identical(gl_ft$shap, NA)
  expect_false(anyNA(gl_ft$m))
})

test_that("rpf multiclass: shap mirrors the class-suffixed structure of m", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  rpk <- randomPlantedForest::rpf(
    cyl ~ mpg + hp + wt,
    data = mt,
    max_interaction = 2
  )
  glk <- glex::glex(rpk, mt)

  expect_identical(glk$constrained, character(0))
  # one shap column per feature and class, like the terms in `m`
  expect_setequal(
    names(glk$shap),
    paste0(rep(names(glk$x), each = 3), "__class:", rep(glk$target_levels, 3))
  )

  # SHAP values redistribute the components of their own class, so per class the
  # two must sum to the same thing exactly. (rpf reports a single intercept for all
  # classes, so the class scores themselves are only reconstructed approximately.)
  for (level in glk$target_levels) {
    shap_cols <- grepl(paste0("__class:", level), names(glk$shap), fixed = TRUE)
    m_cols <- grepl(paste0("__class:", level), names(glk$m), fixed = TRUE)

    expect_equal(
      rowSums(glk$shap[, shap_cols, with = FALSE]),
      rowSums(glk$m[, m_cols, with = FALSE]),
      tolerance = 1e-10,
      info = paste("class", level)
    )
  }
})

test_that("rpf: constraints that drop only zero terms keep shap valid", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  # The inert term has to be zero *by construction*, not by luck of the fit. An earlier
  # version of this test fit at the maximum order and assumed the top-order term came out
  # exactly zero because the data had no structure at that order; it usually did, but the
  # forest is not bit-identical across platforms, and on macOS that term came out at 4e-4
  # -- non-zero, so dropping it moved the SHAP values and the premise collapsed.
  #
  # A constant predictor cannot be split on, so *every* term involving it is exactly zero
  # on every platform. With four features, the sole degree-4 term must involve it too,
  # which makes both constraint axes deterministically inert.
  set.seed(42)
  n <- 120
  dat <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rep(1, n)
  )
  dat$y <- dat$x1 + dat$x2 * dat$x3 + rnorm(n, sd = 0.3)
  predictors <- dat[, c("x1", "x2", "x3", "x4")]

  rp <- randomPlantedForest::rpf(
    y ~ .,
    data = dat,
    max_interaction = 4,
    ntrees = 20
  )
  full <- glex::glex(rp, predictors)

  involves_x4 <- grepl("(^|:)x4($|:)", names(full$m))
  expect_gt(sum(involves_x4), 0)
  expect_equal(
    max(abs(as.matrix(full$m[, involves_x4, with = FALSE]))),
    0
  )

  # `max_interaction`: the only degree-4 term contains x4, so it is zero and dropping it
  # changes nothing. A message rather than a warning, since a constraint *was* requested.
  expect_message(
    gl_mi <- glex::glex(rp, predictors, max_interaction = 3),
    "dropped terms are all zero"
  )
  expect_identical(gl_mi$constrained, character(0))
  expect_null(gl_mi$remainder)
  expect_false(anyNA(gl_mi$shap))
  expect_equal(as.matrix(gl_mi$shap), as.matrix(full$shap), tolerance = 1e-8)

  # `features`: dropping x4 drops only its (zero) terms
  expect_message(
    gl_ft <- glex::glex(rp, predictors, features = c("x1", "x2", "x3")),
    "dropped terms are all zero"
  )
  expect_identical(gl_ft$constrained, character(0))
  expect_null(gl_ft$remainder)
  expect_equal(
    as.matrix(gl_ft$shap),
    as.matrix(full$shap[, c("x1", "x2", "x3")]),
    tolerance = 1e-8
  )

  # Dropping terms that carry weight still invalidates
  expect_warning(
    gl_real <- glex::glex(rp, predictors, max_interaction = 1),
    "efficiency property"
  )
  expect_identical(gl_real$constrained, "max_interaction")
  expect_identical(gl_real$shap, NA)
  expect_false(is.null(gl_real$remainder))
})
