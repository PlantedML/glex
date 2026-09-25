#' Extract xgboost trees from the model's JSON representation
#'
#' `xgboost::xgb.model.dt.tree()` refuses models with categorical features, so
#' the tree table is built from the raw JSON model instead. Node layout matches
#' `xgb.model.dt.tree(use_int_id = TRUE)`, plus a `Categories` list-column with
#' the 0-based category codes routed to `Yes` at categorical splits (empty for
#' numeric splits and leaves).
#'
#' @param object An `xgb.Booster`.
#' @returns A `list` with `trees` (`data.table`), `feature_names` and
#'   `categorical` (names of categorical features, `character(0)` if none).
#' @keywords internal
#' @noRd
xgb_trees <- function(object) {
  raw <- xgboost::xgb.save.raw(object, raw_format = "json")
  model <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)$learner
  feature_names <- unlist(model$feature_names)
  feature_types <- unlist(model$feature_types)
  # Models fit on unnamed matrices carry neither names nor types; refer to
  # features by index like xgb.model.dt.tree()
  n_features <- as.integer(model$learner_model_param$num_feature)
  if (is.null(feature_names)) {
    feature_names <- as.character(seq_len(n_features) - 1L)
  }
  if (is.null(feature_types)) {
    feature_types <- rep("float", n_features)
  }
  categorical <- feature_names[feature_types == "c"]

  trees <- data.table::rbindlist(lapply(
    seq_along(model$gradient_booster$model$trees),
    function(i) xgb_tree_table(model$gradient_booster$model$trees[[i]], i - 1L, feature_names)
  ))
  list(trees = trees, feature_names = feature_names, categorical = categorical)
}

xgb_tree_table <- function(tree, index, feature_names) {
  left <- unlist(tree$left_children)
  right <- unlist(tree$right_children)
  leaf <- left == -1L
  n <- length(left)
  condition <- unlist(tree$split_conditions)
  is_categorical <- unlist(tree$split_type) == 1L
  # Category codes are stored flat; each categorical node owns one segment
  categories <- rep(list(integer(0)), n)
  nodes <- unlist(tree$categories_nodes)
  segments <- unlist(tree$categories_segments)
  sizes <- unlist(tree$categories_sizes)
  codes <- unlist(tree$categories)
  for (k in seq_along(nodes)) {
    categories[[nodes[k] + 1L]] <- as.integer(codes[segments[k] + seq_len(sizes[k])])
  }
  # A categorical split routes members of the set to the right child
  yes <- ifelse(is_categorical, right, left)
  no <- ifelse(is_categorical, left, right)
  missing <- ifelse(unlist(tree$default_left) == 1L, left, right)

  data.table::data.table(
    Tree = index,
    Node = seq_len(n) - 1L,
    Feature = ifelse(leaf, "Leaf", feature_names[unlist(tree$split_indices) + 1L]),
    Split = ifelse(leaf, NA_real_, condition),
    Yes = ifelse(leaf, NA_integer_, yes),
    No = ifelse(leaf, NA_integer_, no),
    Missing = ifelse(leaf, NA_integer_, missing),
    # Leaves store their value where inner nodes store the split threshold
    Gain = ifelse(leaf, condition, unlist(tree$loss_changes)),
    Cover = unlist(tree$sum_hessian),
    Categories = categories
  )
}

#' Per-node category sets in the layout the C++ explainers expect
#' @keywords internal
#' @noRd
node_categories <- function(tree_info) {
  categories <- tree_info[["Categories"]]
  if (is.null(categories)) {
    return(rep(list(integer(0)), nrow(tree_info)))
  }
  lapply(categories, function(codes) if (is.null(codes)) integer(0) else codes)
}

#' Validate `x` against a model with categorical features
#'
#' xgboost stores category codes, not levels, so the levels of `x` must match
#' the training data in order. Only an insufficient number of levels can be
#' detected; the same contract applies to `predict()`.
#' @keywords internal
#' @noRd
check_xgb_categorical <- function(x, categorical, trees) {
  if (!is.data.frame(x)) {
    stop(sprintf(
      "The model has categorical features (%s), so `x` must be a data.frame with factor columns, not a %s.",
      paste(categorical, collapse = ", "),
      class(x)[1]
    ))
  }
  missing <- setdiff(categorical, names(x))
  if (length(missing) > 0) {
    stop(sprintf(
      "Categorical features of the model are missing from `x`: %s",
      paste(missing, collapse = ", ")
    ))
  }
  not_factor <- categorical[!vapply(x[categorical], is.factor, logical(1))]
  if (length(not_factor) > 0) {
    stop(sprintf(
      "The model has categorical features, so these columns of `x` must be factors: %s",
      paste(not_factor, collapse = ", ")
    ))
  }
  for (feature in categorical) {
    codes <- unlist(trees[trees$Feature == feature, ][["Categories"]])
    if (length(codes) > 0 && max(codes) >= nlevels(x[[feature]])) {
      stop(sprintf(
        "Factor `%s` in `x` has %d levels but the model splits on level %d. Levels must match the training data in number and order.",
        feature,
        nlevels(x[[feature]]),
        max(codes) + 1L
      ))
    }
  }
  x
}

#' Predict on the margin scale for both xgboost interfaces
#'
#' `predict.xgboost()` (models from `xgboost()`) ignores `outputmargin` and
#' selects the scale via `type` instead.
#' @keywords internal
#' @noRd
xgb_margin <- function(object, x) {
  if (inherits(object, "xgboost")) {
    stats::predict(object, x, type = "raw")
  } else {
    stats::predict(object, x, outputmargin = TRUE)
  }
}
