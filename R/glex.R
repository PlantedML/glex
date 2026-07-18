#' Global explanations for tree-based models.
#'
#' Global explanations for tree-based models by decomposing
#' regression or classification functions into the sum of main components and
#' interaction components of arbitrary order. Calculates SHAP values and
#' q-interaction SHAP for all values of q for tree-based models such as xgboost.
#'
#' For parallel execution using `xgboost` models, register a backend, e.g. with
#' `doParallel::registerDoParallel()`.
#'
#' @param object Model to be explained, either of class `xgb.Booster` or `rpf`.
#' @param x Data to be explained.
#' @param max_interaction (`integer(1): NULL`)\cr
#'  Maximum interaction size to consider.
#'  Defaults to using all possible interactions available in the model.\cr
#'  For [`xgboost`][xgboost::xgb.train], this defaults to the `max_depth` parameter of the model fit.\cr
#'  If not set in `xgboost`, the default value of `6` is assumed.
#' @param features Vector of column names in `x` to calculate components for. Default is `NULL`, i.e. all features are used.
#' @param ... Further arguments passed to methods.
#'
#' @returns Decomposition of the regression or classification function.
#' A `list` with elements:
#' * `shap`: SHAP values, derived from the functional decomposition as
#'   \eqn{\phi_j = \sum_{S \ni j} m_S / |S|}. This reconstruction is only valid if the
#'   decomposition is complete: if it is constrained (see `constrained`), the components
#'   no longer sum to the full model prediction and the SHAP efficiency property cannot
#'   hold, so `shap` is a scalar `NA` (with a warning) while `m` remains valid.
#'   For multiclass models, columns are class-specific like those of `m`. Note that
#'   `randomPlantedForest` models report a single `intercept` for all classes, so for
#'   multiclass models `intercept + rowSums(shap)` reconstructs the predicted class
#'   scores only approximately.
#' * `m`: Functional decomposition into all main and interaction
#'   components in the model, up to the degree specified by `max_interaction`.
#'   The variable names correspond to the original variable names,
#'   with `:` separating interaction terms as one would specify in a [`formula`] interface.
#' * `intercept`: Intercept term, the expected value of the prediction.
#' * `constrained`: Character vector naming the arguments that constrained the
#'   decomposition (`"max_interaction"`, `"features"`), or `character(0)` if it is
#'   complete. Use `length(x$constrained) > 0` to check whether `shap` is valid.
#'   A constraint that only drops terms whose value is zero leaves the decomposition
#'   unchanged; `glex()` confirms this against the model's predictions, reports it with
#'   a message, and treats the result as complete.
#' * `remainder`: What the dropped terms are collectively worth, per observation:
#'   `prediction - (intercept + rowSums(m))`. Present exactly when the decomposition is
#'   constrained, and absent otherwise, so `intercept + rowSums(m) + remainder`
#'   reconstructs the prediction in either case. For multiclass `randomPlantedForest`
#'   models it is class-wise, mirroring `m`.
#'
#'   Like `m` and `shap`, it is on the scale that the model is decomposed on, which for
#'   `xgboost` is the **link** scale and not the response: for a `binary:logistic` model
#'   the reconstruction gives the margin, and `plogis(intercept + rowSums(m) + remainder)`
#'   gives the predicted probability. `ranger` probability forests and
#'   `randomPlantedForest` are decomposed on the response scale, where no such
#'   back-transformation is needed. Adding `remainder` to a probability is therefore
#'   never correct for `xgboost`.
#' @export
glex <- function(object, x, max_interaction = NULL, features = NULL, ...) {
  UseMethod("glex")
}

#' @rdname glex
#' @export
glex.default <- function(object, ...) {
  stop(
    "`glex()` is not defined for a '",
    class(object)[1],
    "'.",
    call. = FALSE
  )
}

#' @rdname glex
#' @export
#' @examplesIf requireNamespace("randomPlantedForest", quietly = TRUE)
#'
#' # Random Planted Forest -----
#' library(randomPlantedForest)
#'
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
#'
#' glex_rpf <- glex(rp, mtcars[27:32, ])
#' str(glex_rpf, list.len = 5)
glex.rpf <- function(object, x, max_interaction = NULL, features = NULL, ...) {
  if (!requireNamespace("randomPlantedForest", quietly = TRUE)) {
    stop(paste0(
      "randomPlantedForest needs to be installed: ",
      "remotes::install_github(\"PlantedML/randomPlantedForest\")"
    ))
  }

  ret <- randomPlantedForest::predict_components(
    object = object,
    new_data = x,
    max_interaction = max_interaction,
    predictors = features
  )

  model_features <- names(object$blueprint$ptypes$predictors)
  ret$constrained <- constrained_by(
    max_interaction = max_interaction,
    features = features,
    # An rpf model cannot contain interactions of higher order than it was fit with
    available_interaction = min(
      object$params$max_interaction,
      length(model_features)
    ),
    model_features = model_features
  )

  # SHAP values are derived from the components, like in the other methods.
  # For multiclass models `$shap` mirrors the class-suffixed columns of `$m`.
  if (is.null(ret$target_levels)) {
    shap <- shap_from_components(ret$m, names(ret$x))
  } else {
    term_class <- split_names(names(ret$m), "__class:", target_index = 2)

    shap <- do.call(
      cbind,
      lapply(ret$target_levels, function(level) {
        m_class <- ret$m[, names(ret$m)[term_class == level], with = FALSE]
        data.table::setnames(
          m_class,
          split_names(names(m_class), "__class:", target_index = 1)
        )

        shap_class <- shap_from_components(m_class, names(ret$x))
        colnames(shap_class) <- paste0(
          colnames(shap_class),
          "__class:",
          level
        )
        shap_class
      })
    )
  }

  ret$shap <- data.table::setDT(as.data.frame(shap))

  # rpf decomposes the raw score, which is what `type = "numeric"` returns. The default
  # for classification is `type = "prob"`, which applies rpf's response function: a clamp
  # to [0, 1] for `loss = "L2"`, the inverse link for `"logit"` and `"exponential"`.
  # Comparing against that would confound the dropped terms with the back-transformation
  # (and, for binary models, silently compare against the wrong class).
  #
  # Multiclass rpf models report a single intercept for all classes, so the components
  # only reconstruct the class scores approximately and the numeric confirmation is not
  # reliable: the structural verdict is final there, and rpf supplies `$remainder` itself.
  target <- if (is.null(ret$target_levels)) {
    stats::predict(object, x, type = "numeric")[[1]]
  }
  ret <- confirm_constrained(ret, target = target)

  # class(ret) <- c("glex", "rpf_components", class(ret))
  ret
}

#' @rdname glex
#' @export
#' @useDynLib glex, .registration = TRUE
#' @import Rcpp
#' @import data.table
#' @import foreach
#' @import progress
#' @importFrom stats predict
#' @importFrom utils combn
#'
#' @param max_background_sample_size The maximum number of background samples used for the FastPD algorithm, only used when `weighting_method = "fastpd"`. Defaults to `nrow(x)`.
#' @param weighting_method Use either "path-dependent", "fastpd" (default), or "empirical". See References for details.
#' @examplesIf requireNamespace("xgboost", quietly = TRUE)
#' # xgboost -----
#' library(xgboost)
#' x <- as.matrix(mtcars[, -1])
#' y <- mtcars$mpg
#' xg <- xgboost(x[1:26, ], y[1:26],
#'   max_depth = 4, learning_rate = .1,
#'   nrounds = 10, verbosity = 0, nthreads = 1
#' )
#' glex(xg, x[27:32, ])
#' glex(xg, mtcars[27:32, ])
#'
#' \dontrun{
#' # Parallel execution
#' doParallel::registerDoParallel()
#' glex(xg, x[27:32, ])
#' }
glex.xgb.Booster <- function(
  object,
  x,
  max_interaction = NULL,
  features = NULL,
  max_background_sample_size = NULL,
  weighting_method = "fastpd",
  ...
) {
  if (!is.matrix(x)) {
    if (is.data.frame(x) && any(!sapply(x, is.numeric))) {
      stop(
        "Input 'x' contains non-numeric columns. Please ensure all columns are numeric or convert them appropriately (e.g., using model.matrix) to match model training data before calling glex."
      )
    }
    x <- as.matrix(x)
  }
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("xgboost needs to be installed: install.packages(\"xgboost\")")
  }

  if (is.null(max_background_sample_size)) {
    max_background_sample_size <- nrow(x)
  }
  if (is.null(max_interaction)) {
    max_interaction <- 9999
  }

  checkmate::assert_int(max_interaction, lower = 1, upper = Inf)
  checkmate::assert_int(max_background_sample_size, lower = 1, upper = Inf)
  # Convert model
  trees <- xgboost::xgb.model.dt.tree(model = object, use_int_id = TRUE)
  trees$Type <- "<"

  # Early stopping stores the 0-based best round as a booster attribute and
  # predict() defaults to using only trees up to it; decompose the same model
  # predict() evaluates. A round is several trees for multiclass models and
  # num_parallel_tree > 1, so translate rounds to trees via their ratio.
  best_iteration <- xgboost::xgb.attributes(object)$best_iteration
  if (!is.null(best_iteration)) {
    n_rounds <- xgboost::xgb.get.num.boosted.rounds(object)
    trees_per_round <- length(unique(trees$Tree)) / n_rounds
    trees <- trees[
      trees$Tree < (as.integer(best_iteration) + 1) * trees_per_round,
    ]
  }

  # Calculate components
  res <- calc_components(
    trees,
    x,
    max_interaction,
    features,
    weighting_method,
    max_background_sample_size
  )
  res$intercept <- res$intercept + get_xgb_base_score(object)

  # glex decomposes the raw margin, so the constraint is confirmed against it
  res <- confirm_constrained(
    res,
    target = stats::predict(object, x, outputmargin = TRUE)
  )

  # Return components
  res
}


#' @keywords internal
#' @noRd
get_xgb_base_score <- function(object) {
  parse_base_score <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_real_)
    }

    x <- as.character(x[[1]])
    # Newer xgboost may serialize as "[2.0090626E1]".
    x <- gsub("^\\[|\\]$", "", x)
    x <- trimws(x)

    # Handle potential vector-like encodings by taking the first element.
    x <- strsplit(x, ",", fixed = TRUE)[[1]][1]
    x <- trimws(x)

    suppressWarnings(as.numeric(x))
  }

  parse_objective <- function(config) {
    if (is.list(config)) {
      objective_name <- tryCatch(
        config$learner$objective$name,
        error = function(e) NULL
      )
      if (!is.null(objective_name) && length(objective_name) > 0) {
        return(as.character(objective_name[[1]]))
      }
    }

    if (is.character(config) && length(config) == 1) {
      m <- regexpr('"name"\\s*:\\s*"([^"]+)"', config, perl = TRUE)
      if (m != -1) {
        match_str <- regmatches(config, m)
        objective_name <- sub(
          '.*"name"\\s*:\\s*"([^"]+)".*',
          '\\1',
          match_str,
          perl = TRUE
        )
        return(objective_name)
      }
    }

    NA_character_
  }

  base_score_to_margin <- function(base_score, objective) {
    # For logistic objectives, xgboost interprets base_score as a probability
    # and internally transforms it to margin via logit.
    if (objective %in% c("binary:logistic", "reg:logistic")) {
      eps <- .Machine$double.eps
      base_score <- min(max(base_score, eps), 1 - eps)
      return(stats::qlogis(base_score))
    }
    # For log-link objectives, xgboost interprets base_score on response scale
    # and internally transforms it to margin via log.
    if (objective %in% c("count:poisson", "reg:gamma", "reg:tweedie")) {
      eps <- .Machine$double.eps
      base_score <- max(base_score, eps)
      return(log(base_score))
    }

    base_score
  }

  # Newer xgboost versions store base_score in config; older versions may expose xgb.attr().
  config <- tryCatch(xgboost::xgb.config(object), error = function(e) NULL)
  objective <- parse_objective(config)

  if (is.list(config)) {
    base_score <- tryCatch(
      config$learner$learner_model_param$base_score,
      error = function(e) NULL
    )
    if (!is.null(base_score)) {
      base_score_num <- parse_base_score(base_score)
      if (!is.na(base_score_num)) {
        return(base_score_to_margin(base_score_num, objective))
      }
    }
  }

  if (is.character(config) && length(config) == 1) {
    # Fallback parser for JSON-string configs to avoid introducing a jsonlite dependency.
    m <- regexpr('"base_score"\\s*:\\s*"?([^",}]+)"?', config, perl = TRUE)
    if (m != -1) {
      match_str <- regmatches(config, m)
      base_score <- sub(
        '.*"base_score"\\s*:\\s*"?([^",}]+)"?.*',
        '\\1',
        match_str,
        perl = TRUE
      )
      base_score_num <- parse_base_score(base_score)
      if (!is.na(base_score_num)) {
        return(base_score_to_margin(base_score_num, objective))
      }
    }
  }

  base_score_attr <- parse_base_score(xgboost::xgb.attr(object, "base_score"))
  if (length(base_score_attr) == 1 && !is.na(base_score_attr)) {
    return(base_score_to_margin(base_score_attr, objective))
  }

  warning(
    "Could not determine xgboost base_score. Falling back to legacy default 0.5."
  )
  0.5
}

#' @rdname glex
#' @export
#' @useDynLib glex, .registration = TRUE
#' @import Rcpp
#' @import data.table
#' @import foreach
#' @import progress
#' @importFrom stats predict
#' @importFrom utils combn
#' @details
#' The different weighting methods are described in detail in Liu et al. (2025). The default
#' method is `"fastpd"` as it consistently estimates the correct partial dependence function.
#' @references
#' Liu, J., Steensgaard, T., Wright, M. N., Pfister, N., & Hiabu, M. (2025).
#' *Fast Estimation of Partial Dependence Functions using Trees*.
#' Proceedings of the 42nd International Conference on Machine Learning, PMLR 267:39496-39534.
#' [PMLR](https://proceedings.mlr.press/v267/liu25bm.html) |
#' [arXiv:2410.13448](https://arxiv.org/abs/2410.13448)
#' @examplesIf requireNamespace("ranger", quietly = TRUE)
#' # ranger -----
#' library(ranger)
#' x <- as.matrix(mtcars[, -1])
#' y <- mtcars$mpg
#' rf <- ranger(
#'   x = x[1:26, ], y = y[1:26],
#'   num.trees = 5, max.depth = 3,
#'   node.stats = TRUE
#' )
#' glex(rf, x[27:32, ])
#' glex(rf, mtcars[27:32, ])
#'
#' \dontrun{
#' # Parallel execution
#' doParallel::registerDoParallel()
#' glex(rf, x[27:32, ])
#' }
glex.ranger <- function(
  object,
  x,
  max_interaction = NULL,
  features = NULL,
  max_background_sample_size = NULL,
  weighting_method = "fastpd",
  ...
) {
  if (!is.matrix(x)) {
    if (is.data.frame(x) && any(!sapply(x, is.numeric))) {
      stop(
        "Input 'x' contains non-numeric columns. Please ensure all columns are numeric or convert them appropriately (e.g., using model.matrix) to match model training data before calling glex."
      )
    }
    x <- as.matrix(x)
  }
  # To avoid data.table check issues
  terminal <- NULL
  splitvarName <- NULL
  splitStat <- NULL
  splitvarID <- NULL
  tree <- NULL

  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("ranger needs to be installed: install.packages(\"ranger\")")
  }

  if (is.null(object$forest$num.samples.nodes)) {
    stop("ranger needs to be called with node.stats=TRUE for glex.")
  }
  if (is.null(max_background_sample_size)) {
    max_background_sample_size <- nrow(x)
  }
  if (is.null(max_interaction)) {
    max_interaction <- ncol(x)
  }

  checkmate::assert_int(max_interaction, lower = 1, upper = Inf)
  checkmate::assert_int(max_background_sample_size, lower = 1, upper = Inf)

  # Convert model into xgboost format
  trees <- rbindlist(lapply(seq_len(object$num.trees), function(i) {
    as.data.table(ranger::treeInfo(object, tree = i))[, tree := i - 1]
  }))
  prediction_cols <- grep("^pred\\.", colnames(trees), value = TRUE)
  prediction_col <- if ("prediction" %in% colnames(trees)) {
    "prediction"
  } else if (length(prediction_cols) == 2L) {
    # For binary probability forests, use the second class probability.
    prediction_cols[2L]
  } else if (length(prediction_cols) > 0L) {
    stop(
      "ranger classification with more than 2 classes is not supported by glex.ranger yet."
    )
  } else {
    stop("Could not identify ranger prediction column from treeInfo output.")
  }

  trees[terminal == TRUE, splitvarName := "Leaf"]
  trees[terminal == TRUE, splitStat := get(prediction_col)]
  trees[, splitvarID := NULL]
  trees[, terminal := NULL]
  drop_cols <- unique(c("prediction", prediction_cols))
  drop_cols <- intersect(drop_cols, colnames(trees))
  if (length(drop_cols) > 0L) {
    trees[, (drop_cols) := NULL]
  }
  setcolorder(
    trees,
    c(
      "nodeID",
      "leftChild",
      "rightChild",
      "splitvarName",
      "splitval",
      "numSamples",
      "splitStat",
      "tree"
    )
  )
  colnames(trees) <- c(
    "Node",
    "Yes",
    "No",
    "Feature",
    "Split",
    "Cover",
    "Gain",
    "Tree"
  )
  trees$Type <- "<="

  # Calculate components
  res <- calc_components(
    trees,
    x,
    max_interaction,
    features,
    weighting_method,
    max_background_sample_size
  )

  # Divide everything by the number of trees
  res$shap <- res$shap / object$num.trees
  res$m <- res$m / object$num.trees
  res$intercept <- res$intercept / object$num.trees

  ranger_pred <- stats::predict(object, x)$predictions
  if (is.matrix(ranger_pred)) {
    # Probability forests: glex decomposes the second class probability
    ranger_pred <- ranger_pred[, 2L]
  }
  res <- confirm_constrained(res, target = ranger_pred)

  # Return components
  res
}


tree_fun_path_dependent <- function(tree, trees, x, all_S, max_interaction) {
  # Prepare tree_info for C++ function
  tree_info <- trees[get("Tree") == tree, ]
  max_node <- max(tree_info$Node)
  tree_info <- tree_info[data.table::data.table(Node = 0:max_node), on = "Node"]
  tree_info[, "Feature" := get("Feature_num") - 1L] # Adjust to 0-based for C++ bitmasks
  to_select <- c("Feature", "Split", "Yes", "No", "Gain")
  tree_mat <- tree_info[, to_select, with = FALSE] # Use with=FALSE to avoid data.table check issues
  tree_mat[is.na(tree_mat)] <- -1L # Use -1 for leaf nodes
  tree_mat <- as.matrix(tree_mat)

  is_weak_inequality <- tree_info$Type[1] == "<="

  # Call the optimized C++ function
  m_all <- explainTreePathDependent(
    x,
    tree_mat,
    lapply(all_S, function(S) S - 1L),
    max_interaction,
    is_weak_inequality
  )

  # The C++ function returns the final m_all matrix with column names
  m_all
}

tree_fun_emp <- function(tree, trees, x, all_S, max_interaction) {
  # To avoid data.table check issues
  Tree <- NULL
  Feature <- NULL
  Feature_num <- NULL
  Node <- NULL
  Yes <- NULL
  No <- NULL
  Split <- NULL

  # Calculate matrix
  tree_info <- trees[Tree == tree, ]
  max_node <- trees[Tree == tree, max(Node)]
  tree_info <- tree_info[data.table::data.table(Node = 0:max_node), on = "Node"]

  num_nodes <- max_node + 1
  lb <- matrix(-Inf, nrow = num_nodes, ncol = ncol(x))
  ub <- matrix(Inf, nrow = num_nodes, ncol = ncol(x))
  for (nn in 0:max_node) {
    if (trees[Tree == tree & Node == nn, !is.na(Yes)]) {
      left_child <- trees[Tree == tree & Node == nn, Yes]
      right_child <- trees[Tree == tree & Node == nn, No]
      splitvar <- trees[Tree == tree & Node == nn, Feature_num]

      # Children inherit bounds
      ub[left_child + 1, ] <- ub[nn + 1, ]
      ub[right_child + 1, ] <- ub[nn + 1, ]
      lb[left_child + 1, ] <- lb[nn + 1, ]
      lb[right_child + 1, ] <- lb[nn + 1, ]

      # Restrict by new split
      ub[left_child + 1, splitvar] <- trees[Tree == tree & Node == nn, Split]
      lb[right_child + 1, splitvar] <- trees[Tree == tree & Node == nn, Split]
    }
  }

  subsets_in_tree <- setdiff(tree_info[, sort(unique(Feature_num))], 0L)
  U <- get_all_subsets_cpp(subsets_in_tree, max_interaction)
  mat <- recurseRcppEmpProbfunction(
    x,
    tree_info$Feature_num,
    tree_info$Split,
    tree_info$Yes,
    tree_info$No,
    tree_info$Gain,
    lb,
    ub,
    integer(0),
    U,
    0
  )

  colnames(mat) <- vapply(
    U,
    function(u) {
      paste(sort(colnames(x)[u]), collapse = ":")
    },
    FUN.VALUE = character(1)
  )
  # Init m matrix
  m_all <- matrix(0, nrow = nrow(x), ncol = length(all_S))
  colnames(m_all) <- vapply(
    all_S,
    function(s) {
      paste(sort(colnames(x)[s]), collapse = ":")
    },
    FUN.VALUE = character(1)
  )

  # Calculate contribution, use only selected features and subsets with not more than max_interaction involved features
  for (S in intersect(U, all_S)) {
    colname <- paste(sort(colnames(x)[S]), collapse = ":")
    if (nchar(colname) == 0) {
      colnum <- 1
    } else {
      colnum <- which(colnames(m_all) == colname)
    }
    contribute(mat, m_all, S, subsets_in_tree, U, colnum - 1)
  }

  # Return m matrix
  m_all
}

tree_fun_emp_fastPD <- function(
  tree,
  trees,
  x,
  background_sample,
  all_S,
  max_interaction
) {
  # Calculate matrix
  tree_info <- trees[get("Tree") == tree, ]
  max_node <- max(tree_info$Node)
  tree_info <- tree_info[data.table::data.table(Node = 0:max_node), on = "Node"]
  tree_info[, "Feature" := get("Feature_num") - 1L]
  to_select <- c("Feature", "Split", "Yes", "No", "Gain")
  tree_mat <- tree_info[, to_select, with = FALSE]
  tree_mat[is.na(tree_mat)] <- -1L
  tree_mat <- as.matrix(tree_mat)

  is_weak_inequality <- tree_info$Type[1] == "<="
  m_all <- explainTreeFastPDBitmask(
    x,
    background_sample,
    tree_mat,
    lapply(all_S, function(S) S - 1L),
    max_interaction,
    is_weak_inequality
  )
  m_all
}


#' Compute SHAP values from a functional decomposition
#'
#' Each interaction term is distributed equally across the features it involves:
#' \eqn{\phi_j = \sum_{S \ni j} m_S / |S|}. Shared by all `glex()` methods so
#' that `$shap` is derived from `$m` in exactly one place.
#' @param m Component matrix or `data.table` (without the intercept column).
#' @param features Character vector of feature names to compute SHAP values for.
#' @keywords internal
#' @noRd
shap_from_components <- function(m, features) {
  m <- as.matrix(m)
  d <- get_degree(colnames(m))

  # Interaction terms contribute m_S / |S| to each involved feature
  interactions <- sweep(m, MARGIN = 2, d, "/")

  vapply(
    features,
    function(col) {
      idx <- find_term_matches(col, colnames(interactions))

      if (length(idx) == 0) {
        numeric(nrow(interactions))
      } else {
        rowSums(interactions[, idx, drop = FALSE])
      }
    },
    FUN.VALUE = numeric(nrow(m))
  )
}

#' Report which constraints were applied to a decomposition
#'
#' SHAP values can only be reconstructed from a complete decomposition: if terms
#' are dropped, the components no longer sum to the full model prediction and the
#' SHAP efficiency property cannot hold. Returns the names of the constrained
#' arguments, or `character(0)` if the decomposition is complete.
#' @param max_interaction,features As supplied to `glex()`.
#' @param available_interaction Interaction order available in the model. Only
#'   evaluated when `max_interaction` could bind at all (R's lazy argument
#'   evaluation keeps a potentially expensive computation out of the default path).
#' @param model_features Features the model actually uses.
#' @keywords internal
#' @noRd
constrained_by <- function(
  max_interaction,
  features,
  available_interaction,
  model_features
) {
  constrained <- character(0)

  # A model can never contain interactions of higher order than it has features,
  # so this cheap check short-circuits the common unconstrained case.
  if (
    !is.null(max_interaction) &&
      max_interaction < length(model_features) &&
      max_interaction < available_interaction
  ) {
    constrained <- c(constrained, "max_interaction")
  }
  if (!is.null(features) && !all(model_features %in% features)) {
    constrained <- c(constrained, "features")
  }

  constrained
}

#' Confirm a structural constraint numerically, and invalidate SHAP values if real
#'
#' The structural check (`constrained_by()`) only knows which terms were dropped, not
#' what they were worth: a model can contain a high-order term whose value is exactly
#' zero, in which case dropping it changes nothing and the SHAP values remain valid.
#' This confirms the constraint against the model's own predictions before discarding
#' anything: if the components still reconstruct the prediction, the dropped terms were
#' inert and `$shap` is kept (with a message, since the user did ask for a constraint).
#' Otherwise `$shap` is invalidated with a warning.
#'
#' The gap between the components and the prediction is what the dropped terms are
#' collectively worth, and it is reported as `$remainder`. It is the quantitative form of
#' `$constrained` and exists exactly when the decomposition is incomplete, so a complete
#' one carries no remainder.
#' @param res `glex` object with `$m`, `$shap`, `$intercept` and `$constrained`.
#'   For `rpf` models `$remainder` may already be set by
#'   `randomPlantedForest::predict_components()`; it is only overwritten where `target`
#'   lets us compute it ourselves, which keeps the multiclass remainder rpf provides.
#' @param target Model predictions on the scale of the decomposition, or `NULL` to skip
#'   the numeric confirmation and treat the structural verdict as final.
#' @keywords internal
#' @noRd
confirm_constrained <- function(res, target = NULL) {
  if (length(res$constrained) == 0) {
    res$remainder <- NULL
    return(res)
  }

  constrained_labels <- paste0("`", res$constrained, "`", collapse = " and ")

  if (!is.null(target)) {
    reconstruction <- res$intercept + rowSums(res$m)
    res$remainder <- unname(target - reconstruction)

    # The dropped terms are inert only if they are *numerically zero*, which is what the
    # remainder measures directly. Judge it elementwise: `all.equal()` reports the mean
    # relative difference, which averages a discrepancy concentrated in a few observations
    # away to nothing and lets a genuinely non-zero term pass as "all zero" -- and this
    # verdict decides whether `$shap` is trustworthy or `NA`, so it must not be fuzzy.
    inert <- max(abs(res$remainder)) <= 1e-8 * max(1, max(abs(unname(target))))

    if (inert) {
      message(
        "The decomposition is constrained by ",
        constrained_labels,
        ", but the dropped terms are all zero: the components still sum to the model ",
        "prediction, so SHAP values remain valid."
      )
      res$constrained <- character(0)
      res$remainder <- NULL
      return(res)
    }
  }

  warning(
    "SHAP values set to NA: the decomposition is constrained by ",
    constrained_labels,
    " and does not sum to the full model prediction, so SHAP values cannot be ",
    "reconstructed from it (the efficiency property would be violated). ",
    "The components in `$m` are unaffected."
  )
  # Scalar: there are no per-feature values to report, and code that consumes
  # `$shap` numerically should fail loudly rather than propagate NAs.
  res$shap <- NA

  res
}

#' Maximum interaction order present in the model's decomposition.
#'
#' An interaction between features can only arise where they split on the same
#' root-to-leaf path, so the order of a tree is the largest number of *distinct*
#' features on any one of its paths, and the model's order is the maximum over
#' trees. Tree depth alone is not a substitute: a path that splits repeatedly on
#' the same feature is deeper than its interaction order, which would make a
#' complete decomposition look constrained and needlessly invalidate `$shap`.
#'
#' Read from the trees table rather than model metadata because ranger has no
#' usable depth field (`max.depth = 0` means unlimited) and xgboost's config
#' parsing is version-fragile. Relies on child node ids being larger than their
#' parent's, which holds for both `xgb.model.dt.tree(use_int_id = TRUE)` and
#' `ranger::treeInfo()` numbering, so parents are processed before their children.
#' @param trees data.table with columns Node, Yes, No, Feature_num, Tree;
#'   leaves have `Yes = NA` and `Feature_num = 0`
#' @keywords internal
#' @noRd
max_order <- function(trees) {
  Tree <- Node <- NULL

  max(vapply(
    0:max(trees$Tree),
    function(tree) {
      tree_info <- trees[Tree == tree, ][order(Node)]

      # Distinct features seen on the path from the root to each node
      path_features <- vector("list", nrow(tree_info))
      path_features[[1L]] <- integer(0)
      order_tree <- 0L

      for (i in seq_len(nrow(tree_info))) {
        if (is.na(tree_info$Yes[i])) {
          # Leaf: the path ends here
          order_tree <- max(order_tree, length(path_features[[i]]))
          next
        }

        features <- union(path_features[[i]], tree_info$Feature_num[i])
        path_features[[tree_info$Yes[i] + 1L]] <- features
        path_features[[tree_info$No[i] + 1L]] <- features
      }

      order_tree
    },
    integer(1)
  ))
}

#' Internal tree function wrapper that returns the actual tree function
#' @param trees data.table
#' @param x observerations, matrix like data-structure
#' @param all_S all combinations of interactions up to certain order
#' @param weighting_method the weighting method that was supplied to `glex`
#' @keywords internal
#' @noRd
tree_fun_wrapper <- function(
  trees,
  x,
  all_S,
  weighting_method,
  max_interaction,
  max_background_sample_size
) {
  if (is.null(weighting_method)) {
    weighting_method <- "fastpd"
  }

  checkmate::assert_string(weighting_method)
  if (weighting_method == "path-dependent") {
    return(function(tree) {
      tree_fun_path_dependent(tree, trees, x, all_S, max_interaction)
    })
  } else if (weighting_method == "empirical") {
    if (trees$Type[1] != "<=") {
      warning(
        "Using `weighting_method = 'empirical'` with models that apply strict inequality (<) in the splitting rule may lead to inaccuracies. It is recommended to use the default setting (`weighting_method = 'fastpd'`) instead."
      )
    }
    return(function(tree) tree_fun_emp(tree, trees, x, all_S, max_interaction))
  } else if (weighting_method == "fastpd") {
    if (max_background_sample_size > nrow(x)) {
      warning(
        "max_background_sample_size is larger than the number of observations in x. Using all observations."
      )
    }
    background_sample <- x[
      sample(nrow(x), min(max_background_sample_size, nrow(x))),
    ]

    return(function(tree) {
      tree_fun_emp_fastPD(
        tree,
        trees,
        x,
        background_sample,
        all_S,
        max_interaction
      )
    })
  } else {
    stop(
      "The weighting method can either be 'path-dependent', 'empirical', or 'fastpd'"
    )
  }
}

#' Internal function to calculate the components
#' @keywords internal
#' @noRd
calc_components <- function(
  trees,
  x,
  max_interaction,
  features,
  weighting_method = NULL,
  max_background_sample_size = nrow(x)
) {
  # data.table NSE global variable workaround
  Feature <- NULL
  Feature_num <- NULL
  Tree <- NULL

  # Splits are evaluated as plain comparisons, which route an NA to the "No"
  # branch regardless of the missing-value direction the model learned, and NA
  # rows distort the background sample used for marginalization (#41).
  if (anyNA(x)) {
    warning(
      "`x` contains missing values. glex routes them through splits without ",
      "the model's learned missing-value direction, so the decomposition is ",
      "unreliable and will not sum to the model prediction."
    )
  }

  # Convert features to numerics (leaf = 0)
  unique_features_in_tree <- unique(trees$Feature)
  unique_features_in_tree <- unique_features_in_tree[
    unique_features_in_tree != "Leaf"
  ]
  all_is_integer <- suppressWarnings(all(
    !is.na(as.integer(unique_features_in_tree))
  ))

  if (all_is_integer) {
    trees[Feature == "Leaf", Feature_num := 0L]
    trees[Feature != "Leaf", Feature_num := as.integer(Feature) + 1L]
  } else {
    trees[,
      Feature_num := as.integer(factor(
        Feature,
        levels = c("Leaf", colnames(x))
      )) -
        1L
    ]
  }

  # Calculate coverage from theoretical distribution, if given

  if (is.null(features)) {
    # All subsets S (that appear in any of the trees)
    all_S <- unique(do.call(
      c,
      lapply(0:max(trees$Tree), function(tree) {
        unique_features <- trees[
          Tree == tree & Feature_num > 0,
          sort(unique(Feature_num))
        ]
        s <- get_all_subsets_cpp(unique_features, max_interaction)
        s
      })
    ))
  } else {
    # All subsets with supplied features
    if (!all(features %in% colnames(x))) {
      stop("All selected features have to be column names of x.")
    }
    features_num <- as.integer(factor(
      features,
      levels = c("Leaf", colnames(x))
    )) -
      1L
    all_S <- get_all_subsets_cpp(sort(unique(features_num)), max_interaction)
  }
  # Features the model actually splits on, for the constraint check below
  model_features <- colnames(x)[
    setdiff(sort(unique(trees$Feature_num)), 0L)
  ]
  constrained <- constrained_by(
    max_interaction = max_interaction,
    features = features,
    available_interaction = max_order(trees),
    model_features = model_features
  )

  # Keep only those with not more than max_interaction involved features
  d <- lengths(all_S)

  # Run in parallel if a parallel backend is registered
  j <- NULL
  idx <- 0:max(trees$Tree)

  tree_fun <- tree_fun_wrapper(
    trees,
    x,
    all_S,
    weighting_method,
    max_interaction,
    max_background_sample_size
  )
  m_all <- matrix(0, nrow = nrow(x), ncol = length(all_S))
  pb <- progress::progress_bar$new(
    format = "  Explaining trees... [:bar] :percent   Tree :current/:total   ETA :eta",
    total = length(idx),
    clear = FALSE,
    width = 60,
  )
  if (foreach::getDoParRegistered()) {
    m_all <- foreach(j = idx, .combine = "+") %dopar%
      {
        pb$tick()
        tree_fun(j)
      }
  } else {
    for (j in idx) {
      pb$tick()
      m_all <- m_all + tree_fun(j)
    }
  }

  shap <- shap_from_components(m_all[, -1, drop = FALSE], colnames(x))

  # Return shap values, decomposition and intercept. Whether a structural constraint
  # actually invalidates `shap` is confirmed against the model's predictions by the
  # calling method, which has the model object (see `confirm_constrained()`).
  ret <- list(
    shap = data.table::setDT(as.data.frame(shap)),
    m = data.table::setDT(as.data.frame(m_all[, -1])),
    intercept = unique(m_all[, 1]),
    x = data.table::setDT(as.data.frame(x)),
    constrained = constrained
  )
  class(ret) <- c("glex", "xgb_components", class(ret))
  ret
}
