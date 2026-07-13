# These tests crash R on Windows: randomPlantedForest's purify_3() has an
# out-of-bounds read (grid sized lim_list[dim-1].size() instead of .size() - 1
# in src/lib/rpf.cpp, read at gridPoint + 1) that these tests happen to trigger.
# Fixed upstream in https://github.com/PlantedML/randomPlantedForest/pull/61 —
# remove the skips once that (or a minimal fix) is merged.

test_that("rpf binary: FastPD sum identity matches predicted class probability", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  rp <- randomPlantedForest::rpf(
    y ~ x1 + x2 + x3,
    data = xdat,
    max_interaction = 3
  )
  gl <- glex::glex(rp, xdat, weighting_method = "fastpd")
  pred <- predict(rp, xdat)

  score <- unname(gl$intercept + rowSums(gl$m))
  diff_col1 <- max(abs(score - pred[[1]]))
  diff_col2 <- max(abs(score - pred[[2]]))

  expect_lt(min(diff_col1, diff_col2), 0.03)
})

test_that("rpf binary: path-dependent sum identity matches predicted class probability", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  rp <- randomPlantedForest::rpf(
    y ~ x1 + x2 + x3,
    data = xdat,
    max_interaction = 3
  )
  gl <- glex::glex(rp, xdat, weighting_method = "path-dependent")
  pred <- predict(rp, xdat)

  score <- unname(gl$intercept + rowSums(gl$m))
  diff_col1 <- max(abs(score - pred[[1]]))
  diff_col2 <- max(abs(score - pred[[2]]))

  expect_lt(min(diff_col1, diff_col2), 0.03)
})

test_that("rpf multiclass: classwise sum identity matches predicted probabilities", {
  skip_if_not_installed("randomPlantedForest")
  skip_on_os("windows") # rpf purify_3() OOB read, see comment at top of file

  rp <- randomPlantedForest::rpf(
    yk ~ x1 + x2 + x3,
    data = xdat,
    max_interaction = 3
  )
  gl <- glex::glex(rp, xdat)
  pred <- predict(rp, xdat)

  # Each class has its own decomposition terms ending in "__class:<level>".
  # Summing those terms plus intercept should reconstruct that class score.
  class_diffs <- vapply(
    gl$target_levels,
    function(level) {
      idx <- grepl(paste0("__class:", level), names(gl$m), fixed = TRUE)
      score <- unname(
        gl$intercept + rowSums(as.matrix(gl$m[, idx, with = FALSE]))
      )
      max(abs(score - pred[[paste0(".pred_", level)]]))
    },
    FUN.VALUE = numeric(1)
  )

  expect_true(all(class_diffs < 0.8))
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
  expect_true(all(is.na(gl_mi$shap)))
  expect_false(anyNA(gl_mi$m))

  expect_warning(
    gl_ft <- glex::glex(rp, mtcars, features = c("hp", "wt")),
    "efficiency property"
  )
  expect_identical(gl_ft$constrained, "features")
  expect_true(all(is.na(gl_ft$shap)))
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
