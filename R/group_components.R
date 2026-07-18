#' Group features of a decomposition
#'
#' Aggregates the terms of a decomposition such that a set of features is
#' treated as a single feature, named after the group. All terms involving only
#' members of a group are summed into the group's main effect, and terms mixing
#' group members with other features become interactions of the group. Since the
#' decomposition is additive, this regrouping is exact: components still sum to
#' the model prediction, and SHAP values of a group are the sums of its members'
#' SHAP values, preserving the efficiency property.
#'
#' The motivating use case is dummy-encoded categorical features in `xgboost`:
#' the decomposition of a one-hot encoded factor spreads its effect across the
#' dummies and their interactions (e.g. `fa:fb`), which are artifacts of the
#' encoding rather than meaningful interactions. Grouping the dummies restores
#' the factor as one feature, e.g. `groups = list(f = c("fa", "fb", "fc"))`
#' turns `fa`, `fb`, `fc`, `fa:fb`, ... into `f`, and `fa:x1`, `fa:fb:x1`, ...
#' into `f:x1`.
#'
#' For the grouped columns of `x`, a factor is reconstructed when the group's
#' columns form a dummy encoding (0/1 values, at most one active per row), with
#' the active column's name as the level and `"(base)"` for rows with no active
#' column (treatment coding). Otherwise the group's column in `x` is `NA` and
#' plot functions cannot be used for that group's main effect.
#'
#' @param object (`glex`) Object of class `glex`.
#' @param groups (`list`) Named list of character vectors of feature names in
#'   `object$x`. Each vector becomes a single feature named after the list
#'   element, e.g. `list(f = c("fa", "fb", "fc"))`. Features may appear in at
#'   most one group; features not listed remain unchanged.
#'
#' @returns Object of class `glex` with grouped `m`, `shap` and `x`.
#' @export
#'
#' @examplesIf requireNamespace("xgboost", quietly = TRUE)
#' library(xgboost)
#' set.seed(1)
#' n <- 200
#' f <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
#' x1 <- rnorm(n)
#' y <- c(a = 0, b = 2, c = -1)[f] + x1 + c(a = 1, b = 0, c = -1)[f] * x1 + rnorm(n)
#' x <- cbind(model.matrix(~ f - 1), x1)
#'
#' xg <- xgboost(x, y, nrounds = 10, max_depth = 3, nthreads = 1)
#' gl <- glex(xg, x)
#' names(gl$m)
#'
#' grouped <- group_components(gl, groups = list(f = c("fa", "fb", "fc")))
#' names(grouped$m)
#' all.equal(rowSums(grouped$m), rowSums(gl$m))
group_components <- function(object, groups) {
  checkmate::assert_list(
    groups,
    types = "character",
    names = "unique",
    min.len = 1
  )
  features <- names(object$x)

  members <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(members)) {
    stop(
      "Features may appear in at most one group: ",
      paste(unique(members[duplicated(members)]), collapse = ", ")
    )
  }
  unknown <- setdiff(members, features)
  if (length(unknown) > 0) {
    stop(
      "Unknown features in `groups`: ",
      paste(unknown, collapse = ", ")
    )
  }
  clashes <- intersect(names(groups), setdiff(features, members))
  if (length(clashes) > 0) {
    stop(
      "Group names collide with remaining features: ",
      paste(clashes, collapse = ", ")
    )
  }

  # feature -> label under the grouping (ungrouped features keep their name)
  label <- stats::setNames(features, features)
  for (group in names(groups)) {
    label[groups[[group]]] <- group
  }

  object$m <- regroup_terms(object$m, label)
  # `shap` is a scalar NA for constrained decompositions and absent in objects
  # from earlier glex versions; there is nothing to aggregate then.
  if (is.data.frame(object$shap)) {
    object$shap <- regroup_terms(object$shap, label)
  }
  object$x <- regroup_x(object$x, groups, label)

  object
}

#' Sum term columns that coincide under a feature grouping
#'
#' Term names are mapped feature-wise through `label`; multiclass
#' `__class:<level>` suffixes are preserved. Column order follows the first
#' occurrence of each grouped term.
#' @param dt `data.table` of terms (`m` or `shap`).
#' @param label Named character vector mapping features to group labels.
#' @keywords internal
#' @noRd
regroup_terms <- function(dt, label) {
  base <- split_names(names(dt), target_index = 1)
  cls <- split_names(names(dt), target_index = 2)

  new_names <- vapply(
    seq_along(base),
    function(i) {
      feats <- strsplit(base[i], ":", fixed = TRUE)[[1]]
      name <- paste(sort(unique(unname(label[feats]))), collapse = ":")
      if (is.na(cls[i])) {
        name
      } else {
        paste0(name, "__class:", cls[i])
      }
    },
    character(1)
  )

  idx_by_term <- split(seq_along(new_names), new_names)[unique(new_names)]
  res <- lapply(idx_by_term, function(idx) {
    rowSums(as.matrix(dt[, idx, with = FALSE]))
  })
  data.table::setDT(res)
}

#' Replace grouped columns of `x` by one column per group
#'
#' A factor is reconstructed for dummy-encoded groups (0/1 columns, at most one
#' active per row); other groups get an `NA` column. The group column takes the
#' position of its first member.
#' @keywords internal
#' @noRd
regroup_x <- function(x, groups, label) {
  res <- lapply(unique(unname(label)), function(name) {
    if (!name %in% names(groups)) {
      return(x[[name]])
    }
    mat <- as.matrix(x[, groups[[name]], with = FALSE])
    is_dummy <- all(mat %in% c(0, 1)) && all(rowSums(mat) <= 1)
    if (!is_dummy) {
      return(rep(NA, nrow(x)))
    }
    levels <- colnames(mat)
    values <- levels[max.col(mat, ties.method = "first")]
    if (any(rowSums(mat) == 0)) {
      values[rowSums(mat) == 0] <- "(base)"
      levels <- c("(base)", levels)
    }
    factor(values, levels = levels)
  })
  names(res) <- unique(unname(label))
  data.table::setDT(res)
}

#' Derive feature groups from the factors behind a dummy encoding
#'
#' Builds the `groups` list for [group_components()] from the original,
#' un-encoded data: every factor column of `data` whose encoded level columns
#' are found in `object$x` becomes one group. By default, level columns are
#' expected under the [model.matrix()] naming convention
#' (`paste0(feature, levels)`, e.g. `season` with level `"Winter"` becomes
#' `"seasonWinter"`), which covers both one-hot (`~ f - 1`) and treatment
#' (`~ f`) coding -- levels without a matching column (such as a dropped
#' reference level) are simply skipped.
#'
#' If the data was encoded with a different scheme, pass `naming`: a function
#' of `(feature, levels)` returning the encoded column names, e.g.
#' `function(feature, levels) paste(feature, levels, sep = "_")`. It must
#' reproduce the column names the model was trained with, i.e. those found in
#' `object$x`.
#'
#' @param object (`glex`) Object of class `glex`.
#' @param data (`data.frame`) The data before dummy encoding; its factor
#'   columns define the candidate groups. Non-factor columns are ignored.
#' @param naming (`function(feature, levels)`) Maps a factor name and its
#'   levels to the encoded column names. Defaults to the [model.matrix()]
#'   convention `paste0(feature, levels)`.
#'
#' @returns Named list of encoded column names, one element per matched
#'   factor, suitable as the `groups` argument of [group_components()].
#' @export
#'
#' @examplesIf requireNamespace("xgboost", quietly = TRUE)
#' library(xgboost)
#' set.seed(1)
#' n <- 200
#' d <- data.frame(
#'   f = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
#'   x1 = rnorm(n)
#' )
#' y <- c(a = 0, b = 2, c = -1)[d$f] + d$x1 + rnorm(n)
#' x <- model.matrix(~ f + x1 - 1, d)
#'
#' xg <- xgboost(x, y, nrounds = 10, max_depth = 3, nthreads = 1)
#' gl <- glex(xg, x)
#'
#' dummy_groups(gl, d)
#' grouped <- group_components(gl, dummy_groups(gl, d))
#' names(grouped$m)
dummy_groups <- function(
  object,
  data,
  naming = function(feature, levels) paste0(feature, levels)
) {
  checkmate::assert_data_frame(data)
  checkmate::assert_function(naming)

  encoded <- names(object$x)
  factors <- names(data)[vapply(data, is.factor, logical(1))]
  if (length(factors) == 0) {
    stop("`data` contains no factor columns to derive groups from.")
  }

  groups <- list()
  unmatched <- character(0)
  for (feature in factors) {
    candidates <- naming(feature, levels(data[[feature]]))
    present <- intersect(candidates, encoded)
    if (length(present) == 0) {
      unmatched <- c(unmatched, feature)
    } else {
      groups[[feature]] <- present
    }
  }

  if (length(unmatched) > 0) {
    message(
      "No encoded columns found for: ",
      paste(unmatched, collapse = ", "),
      ". If these were encoded, pass a `naming` function matching the ",
      "column names in `object$x`."
    )
  }
  if (length(groups) == 0) {
    stop(
      "No factor levels matched the columns of `object$x`. ",
      "Pass a `naming` function matching your encoding."
    )
  }

  groups
}
