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
#' @param features Vector of column names in x to calculate components for. If \code{NULL}, all features are used.
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
#' @importFrom stats predict
#' @importFrom utils combn
#'
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
#'
#' \dontrun{
#' # Parallel execution
#' doParallel::registerDoParallel()
#' glex(xg, x[27:32, ])
#' }
#' }
glex.xgb.Booster <- function(object, x, max_interaction = NULL, features = NULL, ...) {

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("xgboost needs to be installed: install.packages(\"xgboost\")")
  }

  # To avoid data.table check issues
  Tree <- NULL
  Feature <- NULL
  Feature_num <- NULL

  # If max_interaction is not specified, we set it to the max_depth param of the xgb model.
  # If max_depth is not defined in xgb, we assume its default of 6.
  xgb_max_depth <- ifelse(is.null(object$params$max_depth), 6L, object$params$max_depth)

  if (is.null(max_interaction)) {
    max_interaction <- xgb_max_depth
  }

  checkmate::assert_int(max_interaction, lower = 1, upper = xgb_max_depth)

  # Convert model
  trees <- xgboost::xgb.model.dt.tree(model = object, use_int_id = TRUE)

  # Function to get all subsets of set
  subsets <- function(x) {
    if (length(x) == 1) {
      list(integer(0), x)
    } else {
      do.call(c, lapply(0:length(x), combn, x = x, simplify = FALSE))
    }
  }

  # Convert features to numerics (leaf = 0)
  trees[, Feature_num := as.integer(factor(Feature, levels = c("Leaf", colnames(x)))) - 1L]
  
  if (is.null(features)) {
    # All subsets S (that appear in any of the trees)
    all_S <- unique(do.call(c,lapply(0:max(trees$Tree), function(tree) {
      subsets(trees[Tree == tree & Feature_num > 0, sort(unique(Feature_num))])
    })))
  } else {
    # All subsets with supplied features
    if (!all(features %in% colnames(x))) {
      stop("All selected features have to be column names of x.")
    }
    features_num <- as.integer(factor(features, levels = c("Leaf", colnames(x)))) - 1L
    all_S <- subsets(sort(unique(features_num)))
  }

  # Keep only those with not more than max_interaction involved features
  d <- lengths(all_S)
  all_S <- all_S[d <= max_interaction]

  # For each tree, calculate matrix and contribution
  tree_fun <- function(tree) {
    # Calculate matrix
    tree_info <- trees[Tree == tree, ]

    T <- setdiff(tree_info[, sort(unique(Feature_num))], 0L)
    U <- subsets(T)
    mat <- recurse(x, tree_info$Feature_num, tree_info$Split, tree_info$Yes, tree_info$No,
                   tree_info$Quality, tree_info$Cover, U, 0)
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
      contribute(mat, m_all, S, T, U, colnum-1)
    }

    # Return m matrix
    m_all
  }

  # Run in parallel if a parallel backend is registered
  j <- NULL
  idx <- 0:max(trees$Tree)
  if (foreach::getDoParRegistered()) {
    m_all <- foreach(j = idx, .combine = "+") %dopar% tree_fun(j)
  } else {
    m_all <- foreach(j = idx, .combine = "+") %do% tree_fun(j)
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
    intercept = unique(m_all[, 1]) + 0.5,
    x = data.table::setDT(as.data.frame(x))
  )
  class(ret) <- c("glex", "xgb_components", class(ret))
  ret
}
