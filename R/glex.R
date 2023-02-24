
#' Global explanations for tree-based models.
#'
#' Global explanations for tree-based models by decomposing
#' regression or classification functions into the sum of main components and
#' interaction components of arbitrary order. Calculates SHAP values and
#' q-interaction SHAP for all values of q for tree-based models such as xgboost.
#'
#' For parallel execution, register a backend, e.g. with
#' `doParallel::registerDoParallel()`.
#'
#' @param object Model to be explained.
#' @param x Data to be explained.
#' @param max_interaction Maximum interaction size to consider.
#'  Defaults to using all possible interactions.
#' @param ... Further arguments passed to methods.
#'
#' @return Decomposition of the regression or classification function.
#' A `list` with elements:
#' * `shap`: SHAP values (`xgboost` method only).
#' * `m`: Functional decomposition, i.e., all main and interaction
#'   components in the model, up to the degree specified by `max_interaction`.
#' * `intercept`: Intercept, i.e., expected value of the prediction.
#' @export
glex <- function(object, x, ...) {
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
#' glex(rp, mtcars[27:32, ])
#'
#' }
glex.rpf <- function(object, x, max_interaction = NULL, ...) {
  if (!requireNamespace("randomPlantedForest", quietly = TRUE)) {
    stop(paste0("randomPlantedForest needs to be installed: ",
                "remotes::install_github(\"PlantedML/randomPlantedForest\")"))
  }

  randomPlantedForest::predict_components(
    object = object, new_data = x, max_interaction = max_interaction,
    predictors = NULL
  )

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
#'               nrounds = 10)
#' glex(xg, x[27:32, ])
#'
#' \dontrun{
#' # Parallel execution
#' doParallel::registerDoParallel()
#' glex(xg, x[27:32, ])
#' }
#' }
glex.xgb.Booster <- function(object, x, max_interaction = NULL, ...) {

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("xgboost needs to be installed: install.packages(\"xgboost\")")
  }

  # To avoid data.table check issues
  Tree <- NULL
  Feature <- NULL
  Feature_num <- NULL

  # If max_interaction is not specified, we set it to max
  if (is.null(max_interaction)) max_interaction <- Inf

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
  trees[, Feature_num := as.numeric(factor(Feature, levels = c("Leaf", colnames(x)))) - 1]

  # All subsets S (that appear in any of the trees)
  all_S <- unique(do.call(c,lapply(0:max(trees$Tree), function(tree) {
    subsets(trees[Tree == tree & Feature_num > 0, sort(unique(as.integer(Feature_num)))])
  })))

  # Keep only those with not more than max_interaction involved features
  d <- sapply(all_S, length)
  all_S <- all_S[d <= max_interaction]

  # For each tree, calculate matrix and contribution
  tree_fun <- function(tree) {
    # Calculate matrix
    tree_info <- trees[Tree == tree, ]

    T <- setdiff(tree_info[, sort(unique(Feature_num))], 0)
    U <- subsets(T)
    mat <- recurse(x, tree_info$Feature_num, tree_info$Split, tree_info$Yes, tree_info$No,
                   tree_info$Quality, tree_info$Cover, U, 0)
    colnames(mat) <- sapply(U, function(u) {
      paste(colnames(x)[u], collapse = ":")
    })

    # Init m matrix
    m_all <- matrix(0, nrow = nrow(x), ncol = length(all_S))
    colnames(m_all) <- sapply(all_S, function(s) {
      paste(colnames(x)[s], collapse = ":")
    })

    # Calculate contribution, use only subsets with not more than max_interaction involved features
    d <- sapply(U, length)
    for (S in U[d <= max_interaction]) {
      colname <- paste(colnames(x)[S], collapse = ":")
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

  d <- lengths(regmatches(colnames(m_all), gregexpr(":", colnames(m_all)))) + 1

  # Overall feature effect is sum of all elements where feature is involved
  interactions <- sweep(m_all[, -1, drop = FALSE], MARGIN = 2, d[-1], "/")

  # SHAP values are the sum of the m's *1/d
  shap <- sapply(colnames(x), function(col) {
    # FIXME: Avoid grep'ing overlapping names, e.g. "xy", "xyz"
    idx <- grep(col, colnames(interactions))
    if (length(idx) == 0) {
      rep(0, nrow(interactions))
    } else {
      rowSums(interactions[, idx, drop = FALSE])
    }
  })

  # Return shap values, decomposition and intercept
  list(shap = shap,
       m = m_all[, -1],
       intercept = unique(m_all[, 1]) + 0.5)
}
