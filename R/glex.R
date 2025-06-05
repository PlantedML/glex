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
#' @param features Vector of column names in x to calculate components for. Default is \code{NULL}, i.e. all features are used.
#' @param ... Further arguments passed to methods.
#'
#' @return Decomposition of the regression or classification function.
#' A `list` with elements:
#' * `shap`: SHAP values (`xgboost` method only).
#' * `m`: Functional decomposition into all main and interaction
#'   components in the model, up to the degree specified by `max_interaction`.
#'   The variable names correspond to the original variable names,
#'   with `:` separating interaction terms as one would specify in a [`formula`] interface.
#' * `intercept`: Intercept term, the expected value of the prediction.
#' @export
glex <- function(object, x, max_interaction = NULL, features = NULL, ...) {
  UseMethod("glex")
}

#' @noRd
glex.default <- function(object, ...) {
  stop(
    "`glex()` is not defined for a '", class(object)[1], "'.",
    call. = FALSE
  )
}

#' @rdname glex
#' @export
#' @examples
#'
#' # Random Planted Forest -----
#' if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
#' library(randomPlantedForest)
#'
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
#'
#' glex_rpf <- glex(rp, mtcars[27:32, ])
#' str(glex_rpf, list.len = 5)
#' }
glex.rpf <- function(object, x, max_interaction = NULL, features = NULL, ...) {
  if (!requireNamespace("randomPlantedForest", quietly = TRUE)) {
    stop(paste0("randomPlantedForest needs to be installed: ",
                "remotes::install_github(\"PlantedML/randomPlantedForest\")"))
  }

  ret <- randomPlantedForest::predict_components(
    object = object, new_data = x, max_interaction = max_interaction,
    predictors = features
  )
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
#' @examples
#' # xgboost -----
#' if (requireNamespace("xgboost", quietly = TRUE)) {
#' library(xgboost)
#' x <- as.matrix(mtcars[, -1])
#' y <- mtcars$mpg
#' xg <- xgboost(data = x[1:26, ], label = y[1:26],
#'               params = list(max_depth = 4, eta = .1),
#'               nrounds = 10, verbose = 0)
#' glex(xg, x[27:32, ])
#' glex(xg, mtcars[27:32, ])
#' 
#' \dontrun{
#' # Parallel execution
#' doParallel::registerDoParallel()
#' glex(xg, x[27:32, ])
#' }
#' }
glex.xgb.Booster <- function(object, x, max_interaction = NULL, features = NULL, max_background_sample_size = NULL, weighting_method = "fastpd", ...) {
  if (!is.matrix(x)) {
    if (is.data.frame(x) && any(!sapply(x, is.numeric))) {
      stop("Input 'x' contains non-numeric columns. Please ensure all columns are numeric or convert them appropriately (e.g., using model.matrix) to match model training data before calling glex.")
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

  # Calculate components
  res <- calc_components(trees, x, max_interaction, features, weighting_method, max_background_sample_size)
  res$intercept <- res$intercept + 0.5

  # Return components
  res
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
#' The different weighting methods are described in detail in Liu et al. (2024). The default method is "fastpd" as it consistently estimates the correct partial dependence function.
#' @references
#' Liu, J., Steensgaard, T., Wright, M. N., Pfister, N., & Hiabu, M. (2024).
#' \emph{Fast Estimation of Partial Dependence Functions using Trees}.
#' arXiv preprint \href{https://arxiv.org/abs/2410.13448}{arXiv:2410.13448}.
#' @examples
#' # ranger -----
#' if (requireNamespace("ranger", quietly = TRUE)) {
#' library(ranger)
#' x <- as.matrix(mtcars[, -1])
#' y <- mtcars$mpg
#' rf <- ranger(x = x[1:26, ], y = y[1:26],
#'              num.trees = 5, max.depth = 3,
#'              node.stats = TRUE)
#' glex(rf, x[27:32, ])
#' glex(rf, mtcars[27:32, ])
#' 
#' \dontrun{
#' # Parallel execution
#' doParallel::registerDoParallel()
#' glex(rf, x[27:32, ])
#' }
#' }
glex.ranger <- function(object, x, max_interaction = NULL, features = NULL, max_background_sample_size = NULL, weighting_method = "fastpd", ...) {
  if (!is.matrix(x)) {
    if (is.data.frame(x) && any(!sapply(x, is.numeric))) {
      stop("Input 'x' contains non-numeric columns. Please ensure all columns are numeric or convert them appropriately (e.g., using model.matrix) to match model training data before calling glex.")
    }
    x <- as.matrix(x)
  }
  # To avoid data.table check issues
  terminal <- NULL
  splitvarName <- NULL
  splitStat <- NULL
  prediction <- NULL
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
    as.data.table(ranger::treeInfo(object, tree = i))[, tree := i-1]
  }))
  trees[terminal == TRUE, splitvarName := "Leaf"]
  trees[terminal == TRUE, splitStat := prediction]
  trees[, splitvarID := NULL]
  trees[, terminal := NULL]
  trees[, prediction := NULL]
  colnames(trees) <- c("Node", "Yes", "No", "Feature", "Split", "Cover", "Quality", "Tree")
  trees$Type <- "<="

  # Calculate components
  res <- calc_components(trees, x, max_interaction, features, weighting_method, max_background_sample_size)

  # Divide everything by the number of trees
  res$shap <- res$shap / object$num.trees
  res$m <- res$m / object$num.trees
  res$intercept <- res$intercept / object$num.trees

  # Return components
  res
}


tree_fun_path_dependent <- function(tree, trees, x, all_S, max_interaction) {
  # Prepare tree_info for C++ function
  tree_info <- trees[get("Tree") == tree, ]
  tree_info[, "Feature" := get("Feature_num") - 1L] # Adjust to 0-based for C++ bitmasks
  to_select <- c("Feature", "Split", "Yes", "No", "Quality")
  tree_mat <- tree_info[, to_select, with = FALSE] # Use with=FALSE to avoid data.table check issues
  tree_mat[is.na(tree_mat)] <- -1L # Use -1 for leaf nodes
  tree_mat <- as.matrix(tree_mat)

  is_weak_inequality <- tree_info$Type[1] == "<="

  # Call the optimized C++ function
  m_all <- explainTreePathDependent(x, tree_mat, lapply(all_S, function(S) S - 1L), max_interaction, is_weak_inequality)

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
  mat <- recurseRcppEmpProbfunction(x,
    tree_info$Feature_num, tree_info$Split,
    tree_info$Yes, tree_info$No,
    tree_info$Quality, lb, ub, integer(0), U, 0)

  colnames(mat) <- vapply(U, function(u) {
    paste(sort(colnames(x)[u]), collapse = ":")
  }, FUN.VALUE = character(1))
  # Init m matrix
  m_all <- matrix(0, nrow = nrow(x), ncol = length(all_S))
  colnames(m_all) <- vapply(all_S, function(s) {
    paste(sort(colnames(x)[s]), collapse = ":")
  }, FUN.VALUE = character(1))

  # Calculate contribution, use only selected features and subsets with not more than max_interaction involved features
  for (S in intersect(U, all_S)) {
    colname <- paste(sort(colnames(x)[S]), collapse = ":")
    if (nchar(colname) == 0) {
      colnum <- 1
    } else {
      colnum <- which(colnames(m_all) == colname)
    }
    contribute(mat, m_all, S, subsets_in_tree, U, colnum-1)
  }

  # Return m matrix
  m_all
}

tree_fun_emp_fastPD <- function(tree, trees, x, background_sample, all_S, max_interaction) {
  # Calculate matrix
  tree_info <- trees[get("Tree") == tree, ]
  tree_info[, "Feature" := get("Feature_num") - 1L]
  to_select <- c("Feature", "Split", "Yes", "No", "Quality")
  tree_mat <- tree_info[, to_select, with = FALSE]
  tree_mat[is.na(tree_mat)] <- -1L
  tree_mat <- as.matrix(tree_mat)

  is_weak_inequality <- tree_info$Type[1] == "<="
  m_all <- explainTreeFastPDBitmask(x, background_sample, tree_mat, lapply(all_S, function(S) S - 1L), max_interaction, is_weak_inequality)
  m_all
}


#' Internal tree function wrapper that returns the actual tree function
#' @param trees data.table
#' @param x observerations, matrix like data-structure
#' @param all_S all combinations of interactions up to certain order
#' @param weighting_method the weighting method that was supplied to \code{glex}
#' @keywords internal
#' @noRd
tree_fun_wrapper <- function(trees, x, all_S, weighting_method, max_interaction, max_background_sample_size) {
  if (is.null(weighting_method)) {
    weighting_method <- "fastpd"
  }

  checkmate::assert_string(weighting_method)
  if (weighting_method == "path-dependent") {
    return(function(tree) tree_fun_path_dependent(tree, trees, x, all_S, max_interaction))
  }
  else if (weighting_method == "empirical") {
    if (trees$Type[1] != "<=") {
      warning("Using `weighting_method = 'empirical'` with models that apply strict inequality (<) in the splitting rule may lead to inaccuracies. It is recommended to use the default setting (`weighting_method = 'fastpd'`) instead.")
    }
    return(function(tree) tree_fun_emp(tree, trees, x, all_S, max_interaction))
  } else if (weighting_method == "fastpd") {
    if (max_background_sample_size > nrow(x)) {
      warning("max_background_sample_size is larger than the number of observations in x. Using all observations.")
    }
    background_sample <- x[sample(nrow(x), min(max_background_sample_size, nrow(x))), ]

    return(function(tree) tree_fun_emp_fastPD(tree, trees, x, background_sample, all_S, max_interaction))
  } else {
    stop("The weighting method can either be 'path-dependent', 'empirical', or 'fastpd'")
  }
}

#' Internal function to calculate the components
#' @keywords internal
#' @noRd
calc_components <- function(trees, x, max_interaction, features, weighting_method = NULL, max_background_sample_size = nrow(x)) {

  # data.table NSE global variable workaround
  Feature <- NULL
  Feature_num <- NULL
  Tree <- NULL

  # Convert features to numerics (leaf = 0)
  unique_features_in_tree <- unique(trees$Feature)
  unique_features_in_tree <- unique_features_in_tree[unique_features_in_tree != "Leaf"]
  all_is_integer <- suppressWarnings(all(!is.na(as.integer(unique_features_in_tree))))

  if (all_is_integer) {
    trees[Feature == "Leaf", Feature_num := 0L]
    trees[Feature != "Leaf", Feature_num := as.integer(Feature) + 1L]
  } else {
    trees[, Feature_num := as.integer(factor(Feature, levels = c("Leaf", colnames(x)))) - 1L]
  }

  # Calculate coverage from theoretical distribution, if given

  if (is.null(features)) {
    # All subsets S (that appear in any of the trees)
    all_S <- unique(do.call(c,lapply(0:max(trees$Tree), function(tree) {
      unique_features <- trees[Tree == tree & Feature_num > 0, sort(unique(Feature_num))]
      s <- get_all_subsets_cpp(unique_features, max_interaction)
      s
    })))
  } else {
    # All subsets with supplied features
    if (!all(features %in% colnames(x))) {
      stop("All selected features have to be column names of x.")
    }
    features_num <- as.integer(factor(features, levels = c("Leaf", colnames(x)))) - 1L
    all_S <- get_all_subsets_cpp(sort(unique(features_num)), max_interaction)
  }
  # Keep only those with not more than max_interaction involved features
  d <- lengths(all_S)

  # Run in parallel if a parallel backend is registered
  j <- NULL
  idx <- 0:max(trees$Tree)

  tree_fun <- tree_fun_wrapper(trees, x, all_S, weighting_method, max_interaction, max_background_sample_size)
  m_all <- matrix(0, nrow = nrow(x), ncol = length(all_S))
  pb <- progress::progress_bar$new(
    format = "  Explaining trees... [:bar] :percent   Tree :current/:total   ETA :eta",
    total = length(idx),
    clear = FALSE,
    width = 60,
  )
  if (foreach::getDoParRegistered()) {
    m_all <- foreach(j = idx, .combine = "+") %dopar% {
      pb$tick()
      tree_fun(j)
    }
  } else {
    for (j in idx) {
      pb$tick()
      m_all <- m_all + tree_fun(j)
    }
  }

  d <- get_degree(colnames(m_all))

  # Overall feature effect is sum of all elements where feature is involved
  interactions <- sweep(m_all[, -1, drop = FALSE], MARGIN = 2, d[-1], "/")

  # SHAP values are the sum of the m's * 1/d
  shap <- vapply(colnames(x), function(col) {
    idx <- find_term_matches(col, colnames(interactions))

    if (length(idx) == 0) {
      numeric(nrow(interactions))
    } else {
      rowSums(interactions[, idx, drop = FALSE])
    }
  }, FUN.VALUE = numeric(nrow(x)))

  # Return shap values, decomposition and intercept
  ret <- list(
    shap = data.table::setDT(as.data.frame(shap)),
    m = data.table::setDT(as.data.frame(m_all[, -1])),
    intercept = unique(m_all[, 1]),
    x = data.table::setDT(as.data.frame(x))
  )
  class(ret) <- c("glex", "xgb_components", class(ret))
  ret
}
