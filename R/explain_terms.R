#' Terms of one observation's prediction, ready to plot
#'
#' Selects the terms of observation `id` (and `class`, for multiclass objects)
#' and aggregates the rest, so the rows always run from the intercept to the
#' prediction.
#' @inheritParams plot_waterfall
#' @returns A `list` with `terms` (a `data.table` with columns `term`, `m`,
#'   `degree`, `type`, `end`, `start`, `label`), `intercept`, `prediction` and
#'   `n_other` (number of aggregated terms).
#' @keywords internal
#' @noRd
explain_terms <- function(
  object,
  id,
  class = NULL,
  threshold = 0,
  max_terms = 10L,
  max_interaction = NULL
) {
  checkmate::assert_class(object, "glex")
  checkmate::assert_int(id, lower = 1, upper = nrow(object$x))
  checkmate::assert_number(threshold, lower = 0, finite = TRUE)
  checkmate::assert_int(max_terms, lower = 1)
  checkmate::assert_int(max_interaction, lower = 1, null.ok = TRUE)
  check_class_arg(object, class)

  m <- unlist(object$m[id, ], use.names = TRUE)
  if (!is.null(class)) {
    suffix <- paste0("__class:", class)
    m <- m[endsWith(names(m), suffix)]
    names(m) <- substr(names(m), 1L, nchar(names(m)) - nchar(suffix))
  }
  m <- m[m != 0]
  degree <- get_degree(names(m))

  drop <- abs(m) <= threshold
  if (!is.null(max_interaction)) {
    drop <- drop | degree > max_interaction
  }
  kept <- which(!drop)
  kept <- kept[order(abs(m[kept]), decreasing = TRUE)]
  if (length(kept) > max_terms) {
    drop[kept[-seq_len(max_terms)]] <- TRUE
    kept <- kept[seq_len(max_terms)]
  }

  terms <- data.table::data.table(
    term = names(m)[kept],
    m = unname(m[kept]),
    degree = degree[kept],
    type = rep("term", length(kept))
  )
  n_other <- sum(drop)
  if (n_other > 0) {
    terms <- rbind(
      terms,
      data.table::data.table(
        term = sprintf("%d other %s", n_other, if (n_other == 1) "term" else "terms"),
        m = sum(m[drop]),
        degree = NA_integer_,
        type = "other"
      )
    )
  }
  remainder <- class_remainder(object, id, class)
  if (!is.null(remainder)) {
    terms <- rbind(
      terms,
      data.table::data.table(
        term = "remainder (constrained)",
        m = remainder,
        degree = NA_integer_,
        type = "remainder"
      )
    )
  }

  intercept <- class_intercept(object, class)
  terms$end <- intercept + cumsum(terms$m)
  terms$start <- terms$end - terms$m
  terms$label <- terms$term
  is_term <- terms$type == "term"
  terms$label[is_term] <- term_label(terms$term[is_term], object$x[id, ])

  list(
    terms = terms,
    intercept = intercept,
    prediction = intercept + sum(terms$m),
    n_other = as.integer(n_other)
  )
}

#' Label terms with the observation's feature values, e.g. "cyl:hp = 8, 264"
#' @param x_row One-row `data.table` of feature values.
#' @keywords internal
#' @noRd
term_label <- function(terms, x_row) {
  vapply(
    strsplit(terms, ":", fixed = TRUE),
    function(features) {
      values <- vapply(
        features,
        function(f) format_feature_value(x_row[[f]]),
        character(1)
      )
      sprintf("%s = %s", paste(features, collapse = ":"), paste(values, collapse = ", "))
    },
    character(1)
  )
}

# Grouped objects can lack a feature in `$x` or hold NA for it
format_feature_value <- function(v) {
  if (length(v) == 0 || is.na(v)) {
    return("NA")
  }
  if (is.numeric(v)) {
    return(format(v, digits = 3))
  }
  as.character(v)
}

#' Validate `class` against the object's target levels
#' @keywords internal
#' @noRd
check_class_arg <- function(object, class) {
  if (is.null(object$target_levels)) {
    if (!is.null(class)) {
      stop("`class` only applies to multiclass objects.", call. = FALSE)
    }
    return(invisible(NULL))
  }
  if (is.null(class)) {
    stop(
      sprintf(
        "`class` is required for multiclass objects, one of: %s",
        paste(object$target_levels, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  checkmate::assert_choice(class, object$target_levels)
}

# rpf currently stores one intercept for all classes; use per-class values when present
class_intercept <- function(object, class) {
  if (!is.null(class) && length(object$intercept) == length(object$target_levels)) {
    return(unname(object$intercept[[match(class, object$target_levels)]]))
  }
  unname(object$intercept[[1]])
}

class_remainder <- function(object, id, class) {
  r <- object$remainder
  if (is.null(r)) {
    return(NULL)
  }
  if (is.data.frame(r)) {
    return(r[[class]][[id]])
  }
  r[[id]]
}
