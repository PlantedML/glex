test_that("rpf binary: FastPD sum identity matches predicted class probability", {
  skip_if_not_installed("randomPlantedForest")

  rp <- randomPlantedForest::rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex::glex(rp, xdat, weighting_method = "fastpd")
  pred <- predict(rp, xdat)

  score <- unname(gl$intercept + rowSums(gl$m))
  diff_col1 <- max(abs(score - pred[[1]]))
  diff_col2 <- max(abs(score - pred[[2]]))

  expect_lt(min(diff_col1, diff_col2), 0.03)
})

test_that("rpf binary: path-dependent sum identity matches predicted class probability", {
  skip_if_not_installed("randomPlantedForest")

  rp <- randomPlantedForest::rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex::glex(rp, xdat, weighting_method = "path-dependent")
  pred <- predict(rp, xdat)

  score <- unname(gl$intercept + rowSums(gl$m))
  diff_col1 <- max(abs(score - pred[[1]]))
  diff_col2 <- max(abs(score - pred[[2]]))

  expect_lt(min(diff_col1, diff_col2), 0.03)
})

test_that("rpf multiclass: classwise sum identity matches predicted probabilities", {
  skip_if_not_installed("randomPlantedForest")

  rp <- randomPlantedForest::rpf(yk ~ x1 + x2 + x3, data = xdat, max_interaction = 3)
  gl <- glex::glex(rp, xdat)
  pred <- predict(rp, xdat)

  # Each class has its own decomposition terms ending in "__class:<level>".
  # Summing those terms plus intercept should reconstruct that class score.
  class_diffs <- vapply(gl$target_levels, function(level) {
    idx <- grepl(paste0("__class:", level), names(gl$m), fixed = TRUE)
    score <- unname(gl$intercept + rowSums(as.matrix(gl$m[, idx, with = FALSE])))
    max(abs(score - pred[[paste0(".pred_", level)]]))
  }, FUN.VALUE = numeric(1))

  expect_true(all(class_diffs < 0.8))
})
