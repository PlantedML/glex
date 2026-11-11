# Support for the pruned models of randomPlantedForest (class `rpf_pruned`): what glex.rpf_pruned() stores about the
# pruning, the evaluation of a kept component on a grid, and the heat maps of pairs and of components of three or
# more predictors as the pruned model's display shows them.

#' The pruning's description of the kept components, stored as `$pruned` of a glex object
#' @param object An `rpf_pruned` model.
#' @param x The data explained, all predictors (`predict_components()`'s `$x`).
#' @keywords internal
#' @noRd
pruned_info <- function(object, x) {
  comp <- object$components
  comp <- comp[match(names(object$beta), comp$component), , drop = FALSE]
  col_or <- function(name, missing) {
    if (is.null(comp[[name]])) rep(missing, nrow(comp)) else comp[[name]]
  }
  components <- data.frame(
    term = comp$component,
    degree = as.integer(comp$order),
    share = comp$share,
    beta = unname(object$beta),
    form = col_or("form", NA_character_),
    heat_maps = col_or("heat_maps", NA_real_),
    stringsAsFactors = FALSE
  )
  rownames(components) <- NULL
  # the panels of the components of three or more predictors: coarsened ones, and (randomPlantedForest from 0.6) those
  # kept as they are, at their own cut points
  panel_form <- function(co, coarsened) {
    list(
      C = co$C,
      cuts = stats::setNames(unname(co$cuts), co$C),
      coarsened = coarsened,
      var_kept = if (coarsened) co$var_kept else 1
    )
  }
  coarse <- lapply(object$display$coarse, panel_form, coarsened = TRUE)
  asis <- lapply(object$display$asis, panel_form, coarsened = FALSE)
  max_panels <- object$display$max_panels
  list(
    components = components,
    coarse = coarse,
    asis = asis,
    max_panels = if (is.null(max_panels)) Inf else max_panels,
    intercept = object$intercept,
    mode = object$fit$mode,
    loss = object$fit$params$loss,
    factor_levels = object$fit$factor_levels,
    # the predictors a component does not depend on are set to this row when it is evaluated on a grid
    base = as.data.frame(x[1, ]),
    model = object
  )
}

#' Is this the glex object of a pruned randomPlantedForest?
#' @keywords internal
#' @noRd
is_pruned_glex <- function(object) {
  !is.null(object$pruned)
}

#' The term of `object$m` whose predictors are `predictors` (in any order), or an error
#' @keywords internal
#' @noRd
find_term <- function(object, predictors) {
  terms <- unique(split_names(names(object$m), "__class:", target_index = 1))
  sets <- strsplit(terms, ":", fixed = TRUE)
  hit <- terms[vapply(
    sets,
    function(s) length(s) == length(predictors) && setequal(s, predictors),
    logical(1)
  )]
  if (length(hit) == 0) {
    stop(sprintf(
      "Term \"%s\" is not part of the decomposition: %s",
      paste(predictors, collapse = ":"),
      if (is_pruned_glex(object)) {
        "the model has no such component, see `names(object$m)`."
      } else {
        "only feature subsets that occur together in at least one tree have a component, see `names(object$m)`."
      }
    ))
  }
  hit[[1]]
}

#' Kept component `term` of a pruned model at points: `values` is a named list of equally long vectors (the
#' component's predictors), the other predictors are those of `object$pruned$base`. An n x V matrix (V score columns:
#' one per class for multiclass models), as the component enters the prediction (coefficient times component).
#' @keywords internal
#' @noRd
pruned_values <- function(object, term, values) {
  info <- object$pruned
  n <- length(values[[1]])
  Z <- info$base[rep(1L, n), , drop = FALSE]
  for (v in names(values)) {
    Z[[v]] <- values[[v]]
  }
  rownames(Z) <- NULL
  pc <- tryCatch(
    randomPlantedForest::predict_components(info$model, Z),
    error = function(e) {
      stop(
        "Can't evaluate the model, which the plot needs (a model saved to a file must be restored with ",
        "randomPlantedForest's `rpf_unmarshal()`): ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  cols <- if (is.null(object$target_levels)) term else paste0(term, "__class:", object$target_levels)
  as.matrix(pc$m[, cols, with = FALSE])
}

#' Rows of `d` once per class, with the values of that class's column of `value` (n x V) in `m` and a factor `class`;
#' without classes, `d` with `m`
#' @keywords internal
#' @noRd
pruned_by_class <- function(d, value, classes) {
  if (is.null(classes)) {
    d$m <- value[, 1]
    return(d)
  }
  out <- d[rep(seq_len(nrow(d)), length(classes)), , drop = FALSE]
  out$m <- as.vector(value)
  out$class <- factor(rep(classes, each = nrow(d)), levels = classes)
  rownames(out) <- NULL
  out
}

#' The cells of a heat map along one predictor with values `v` (of the data explained): at most `resolution` values
#' (the distinct ones, or the values at the quantiles (i - 0.5) / resolution), each the centre of a cell reaching
#' halfway to its neighbours; a factor has one cell per level. `value` holds the points to evaluate at.
#' @keywords internal
#' @noRd
pruned_axis <- function(v, resolution) {
  if (is.factor(v)) {
    lv <- levels(v)
    at <- seq_along(lv)
    return(list(at = at, lower = at - 0.5, upper = at + 0.5, value = factor(lv, levels = lv), levels = lv))
  }
  v <- v[!is.na(v)]
  at <- sort(unique(v))
  if (length(at) > resolution) {
    at <- unique(stats::quantile(v, (seq_len(resolution) - 0.5) / resolution, names = FALSE, type = 1))
  }
  m <- length(at)
  if (m == 1) {
    return(list(at = at, lower = at - 0.5, upper = at + 0.5, value = at))
  }
  mid <- (at[-1] + at[-m]) / 2
  list(at = at, lower = c(2 * at[1] - mid[1], mid), upper = c(mid, 2 * at[m] - mid[m - 1]), value = at)
}

#' Position scale of a heat map axis: the levels of a factor at their codes
#' @keywords internal
#' @noRd
pruned_axis_scale <- function(aesthetic, axis, name) {
  scale <- switch(aesthetic, x = ggplot2::scale_x_continuous, y = ggplot2::scale_y_continuous)
  if (is.null(axis$levels)) {
    scale(name)
  } else {
    scale(name, breaks = axis$at, labels = axis$levels, minor_breaks = NULL)
  }
}

#' The panel predictors of a kept component of three or more predictors (`vars`) as the pruned model's display
#' stores them: for a coarsened component its coarse predictors and their cut points (`$pruned$coarse`), for one kept
#' as it is the predictors and its own cut points on them (`$pruned$asis`). A model from a randomPlantedForest that does
#' not store the latter gets one heat map over the two predictors with the most distinct values in `x`, the others at
#' their values in the first row of `x` (with a message). A list with the predictors' names, cut points, a value
#' inside each cell, the cells' labels, the display's form (NULL for the fallback) and `fixed`, the predictors held at
#' one value.
#' @keywords internal
#' @noRd
pruned_panels <- function(object, term, vars) {
  info <- object$pruned
  co <- info$coarse[[term]]
  if (is.null(co)) {
    co <- info$asis[[term]]
  }
  fixed <- character(0)
  if (!is.null(co)) {
    name <- co$C
    cuts <- co$cuts[name]
  } else {
    message(
      "The model does not store the cells of ",
      term,
      " (it was built by an earlier version of randomPlantedForest): one heat map over two of its predictors, ",
      "the others at their values in the first row of `x`."
    )
    distinct <- vapply(vars, function(v) data.table::uniqueN(object$x[[v]]), numeric(1))
    name <- vars[-order(-distinct)[1:2]]
    cuts <- stats::setNames(rep(list(numeric(0)), length(name)), name)
    fixed <- name
  }
  values <- labels <- vector("list", length(name))
  for (f in seq_along(name)) {
    xv <- object$x[[name[f]]]
    t <- cuts[[f]]
    forest_levels <- info$factor_levels[[name[f]]]
    if (name[f] %in% fixed) {
      values[[f]] <- info$base[[name[f]]]
      labels[[f]] <- paste(name[f], "=", format(values[[f]], digits = 3))
    } else if (!is.null(forest_levels)) {
      # factor codes in the order of the forest's levels; a cell holds the codes c with t_(i-1) <= c < t_i
      codes <- seq_along(forest_levels)
      cell <- findInterval(codes, t)
      cell_ids <- sort(unique(cell))
      first <- vapply(cell_ids, function(c) codes[cell == c][1], integer(1))
      values[[f]] <- factor(forest_levels[first], levels = levels(xv))
      labels[[f]] <- vapply(
        cell_ids,
        function(c) paste0(name[f], ": ", paste(forest_levels[cell == c], collapse = ", ")),
        character(1)
      )
      if (length(cell_ids) == 1) {
        labels[[f]] <- name[f]
      }
    } else if (length(t) == 0) {
      # averaged out (q = 1): the component does not depend on it
      values[[f]] <- xv[1]
      labels[[f]] <- name[f]
    } else {
      lo <- min(xv, na.rm = TRUE)
      if (!(lo < t[1])) {
        lo <- t[1] - 1
      }
      values[[f]] <- c(lo, t)
      s <- format_cuts(t)
      m <- length(t)
      labels[[f]] <- c(
        paste(name[f], "<", s[1]),
        if (m > 1) sprintf("%s in [%s, %s)", name[f], s[-m], s[-1]),
        paste(name[f], ">=", s[m])
      )
    }
  }
  list(name = name, cuts = cuts, values = values, labels = labels, form = co, fixed = fixed)
}

#' Cut points to 3 significant digits, more if needed to tell them apart
#' @keywords internal
#' @noRd
format_cuts <- function(cuts) {
  for (digits in 3:15) {
    s <- signif(cuts, digits)
    if (!anyDuplicated(s)) {
      break
    }
  }
  vapply(s, format, character(1), digits = digits)
}

#' A share as a percentage with 2 significant digits
#' @keywords internal
#' @noRd
format_share <- function(s) {
  if (is.na(s)) {
    return("NA")
  }
  if (s == 0) {
    return("0")
  }
  paste0(trimws(formatC(signif(100 * s, 2), digits = 2, format = "fg")), "%")
}

#' The cells of the heat maps of kept component `term` over the predictors `axes`, one block per cell of the panel
#' predictors (`panels`, NULL for a pair): bounds, `m`, a factor per panel predictor with more than one cell, and
#' `class` for multiclass models
#' @keywords internal
#' @noRd
pruned_cells <- function(object, term, axes, panels, resolution) {
  ga <- pruned_axis(object$x[[axes[1]]], resolution)
  gb <- pruned_axis(object$x[[axes[2]]], resolution)
  grid <- expand.grid(i = seq_along(ga$at), j = seq_along(gb$at))
  cells <- expand.grid(lapply(panels$values, seq_along))
  n_cells <- max(1L, nrow(cells))
  g <- rep(seq_len(nrow(grid)), n_cells)
  cell <- rep(seq_len(n_cells), each = nrow(grid))
  values <- list(ga$value[grid$i[g]], gb$value[grid$j[g]])
  names(values) <- axes
  for (f in seq_along(panels$name)) {
    values[[panels$name[f]]] <- panels$values[[f]][cells[cell, f]]
  }
  d <- data.frame(
    xmin = ga$lower[grid$i[g]],
    xmax = ga$upper[grid$i[g]],
    ymin = gb$lower[grid$j[g]],
    ymax = gb$upper[grid$j[g]]
  )
  for (f in which(lengths(panels$values) > 1 | panels$name %in% panels$fixed)) {
    d[[paste0(".panel", f)]] <- factor(panels$labels[[f]][cells[cell, f]], levels = panels$labels[[f]])
  }
  d <- pruned_by_class(d, pruned_values(object, term, values), object$target_levels)
  attr(d, "axes") <- list(ga, gb)
  d
}

#' A heat map (pair) or heat maps (one per panel cell, and per class) on one diverging colour scale
#' @keywords internal
#' @noRd
pruned_heatmap <- function(d, axes, vars, subtitle = NULL, per_row = NULL) {
  ax <- attr(d, "axes")
  limits <- get_m_limits(d)
  if (!(limits[2] > 0)) {
    limits <- c(-1, 1)
  }
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      xmin = .data[["xmin"]],
      xmax = .data[["xmax"]],
      ymin = .data[["ymin"]],
      ymax = .data[["ymax"]],
      colour = .data[["m"]],
      fill = ggplot2::after_scale(.data[["colour"]])
    )
  ) +
    ggplot2::geom_rect(linewidth = 0.1) +
    pruned_axis_scale("x", ax[[1]], axes[1]) +
    pruned_axis_scale("y", ax[[2]], axes[2]) +
    diverging_palette(limits = limits) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(color = label_m(vars), subtitle = subtitle)
  panel_facets <- grep("^\\.panel", names(d), value = TRUE)
  facets <- c(panel_facets, intersect("class", names(d)))
  if (length(panel_facets) > 0 && "class" %in% names(d)) {
    # multiclass heat maps: a row per panel cell, a column per class
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!!lapply(panel_facets, as.name)),
        cols = ggplot2::vars(.data[["class"]]),
        labeller = ggplot2::labeller(class = ggplot2::label_both)
      )
  } else if (length(facets) > 0) {
    p <- p +
      ggplot2::facet_wrap(
        facets,
        ncol = per_row,
        labeller = ggplot2::labeller(class = ggplot2::label_both)
      )
  }
  p +
    theme_glex() +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(1.5, "lines"), panel.spacing.y = ggplot2::unit(0.8, "lines"))
}

#' The heat map of a kept pair of a pruned model, evaluated on a grid of the data's values
#' @keywords internal
#' @noRd
plot_pruned_pair <- function(object, predictors, resolution = 100L) {
  term <- find_term(object, predictors)
  d <- pruned_cells(object, term, predictors, NULL, resolution)
  pruned_heatmap(d, predictors, predictors)
}

#' The heat maps of a kept component of three or more predictors of a pruned model, as its display shows it: over two
#' of its predictors, one per cell of the others (and per class). `ncol`: the columns of the overview it is part of,
#' which sets the heat maps per row.
#' @keywords internal
#' @noRd
plot_pruned_panels <- function(object, predictors, resolution = 100L, ncol = NULL) {
  term <- find_term(object, predictors)
  vars <- strsplit(term, ":", fixed = TRUE)[[1]]
  panels <- pruned_panels(object, term, vars)
  axes <- setdiff(vars, panels$name)
  d <- pruned_cells(object, term, axes, panels, resolution)
  n_panels <- prod(lengths(panels$values))
  V <- max(1L, length(object$target_levels))
  n <- n_panels * V
  per_row <- if (V > 1L) {
    V
  } else if (is.null(ncol)) {
    min(n, 5L)
  } else {
    min(n, max(2L, round(1.25 * ncol)))
  }
  p <- pruned_heatmap(
    d,
    axes,
    vars,
    per_row = per_row
  )
  attr(p, "n_maps") <- n
  attr(p, "per_row") <- per_row
  p
}

#' The overview of plot.glex() without predictors: one plot per component, largest first
#' @keywords internal
#' @noRd
plot_glex_overview <- function(object, measure = NULL, max_components = NULL, ncol = NULL, resolution = 100L, ...) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("The overview of all components needs the patchwork package: install.packages(\"patchwork\")")
  }
  pruned <- is_pruned_glex(object)
  if (is.null(measure)) {
    measure <- if (pruned) "variance" else "mean_abs"
  }
  rank <- component_ranking(object, measure)
  degree <- get_degree(names(rank))
  # components of three or more predictors are drawn for the models that store their cells
  drawable <- pruned | degree <= 2
  caption <- character(0)
  if (any(!drawable)) {
    caption <- c(
      caption,
      sprintf(
        "Not drawn: %d component%s of 3 or more predictors",
        sum(!drawable),
        if (sum(!drawable) == 1) "" else "s"
      )
    )
  }
  terms <- names(rank)[drawable]
  if (length(terms) == 0) {
    stop(
      "No component to draw: ",
      if (ncol(object$m) == 0) {
        "the model's prediction is its intercept."
      } else {
        "glex draws components of up to two predictors of this model."
      }
    )
  }
  if (is.null(max_components)) {
    max_components <- if (pruned) length(terms) else 12L
  }
  checkmate::assert_int(max_components, lower = 1)
  if (length(terms) > max_components) {
    caption <- c(
      caption,
      sprintf(
        "Not drawn: the %d smaller components (max_components = %d)",
        length(terms) - max_components,
        max_components
      )
    )
    terms <- terms[seq_len(max_components)]
  }
  degree <- get_degree(terms)
  V <- max(1L, length(object$target_levels))
  if (is.null(ncol)) {
    ncol <- if (V > 1L) min(4L, V) else min(4L, max(1L, sum(degree <= 2)))
  }
  checkmate::assert_int(ncol, lower = 1)

  plots <- vector("list", length(terms))
  rows <- integer(length(terms))
  for (k in seq_along(terms)) {
    vars <- strsplit(terms[k], ":", fixed = TRUE)[[1]]
    if (length(vars) <= 2 && V > 1L) {
      # multiclass: one panel per class, the plot spans a row band
      per_row <- min(V, max(2L, round(1.25 * ncol)))
      rows[k] <- max(1L, ceiling(ceiling(V / per_row) * ncol / per_row))
    }
    if (length(vars) == 1) {
      p <- plot_main_effect(object, vars, ...)
    } else if (length(vars) == 2) {
      p <- plot_twoway_effects(object, vars, resolution = resolution, ...)
    } else {
      p <- plot_pruned_panels(object, vars, resolution, ncol = ncol)
      n <- attr(p, "n_maps")
      per_row <- attr(p, "per_row")
      rows[k] <- max(1L, ceiling(ceiling(n / per_row) * ncol / per_row))
    }
    p <- p +
      ggplot2::labs(title = sprintf("%s (%s)", terms[k], format_measure(rank[[terms[k]]], measure))) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.9)))
    colour_scale <- p$scales$get_scales("colour")
    if (length(vars) >= 2) {
      p <- p + ggplot2::theme(legend.position = "right")
    }
    if (length(vars) >= 2 && inherits(colour_scale, "ScaleContinuous")) {
      # colour bars to the right, so that the heat maps keep their height (a forest's pair of a continuous and a
      # categorical predictor is coloured by the category instead)
      p <- p +
        ggplot2::guides(
          colour = ggplot2::guide_colourbar(
            barwidth = ggplot2::unit(0.6, "lines"),
            barheight = ggplot2::unit(5, "lines"),
            title.position = "top",
            direction = "vertical"
          )
        )
    }
    plots[[k]] <- p
  }

  out <- patchwork::wrap_plots(plots, design = overview_layout(rows, ncol))
  if (length(caption) > 0) {
    out <- out + patchwork::plot_annotation(caption = paste(caption, collapse = "\n"))
  }
  out
}

#' A component's size in its title
#' @keywords internal
#' @noRd
format_measure <- function(value, measure) {
  if (measure == "mean_abs") format(signif(value, 2)) else format_share(value)
}

#' The patchwork layout: mains and pairs fill rows of `ncol` cells in order; a plot with `rows[k] > 0` starts a new row
#' and spans all columns and `rows[k]` rows
#' @keywords internal
#' @noRd
overview_layout <- function(rows, ncol) {
  areas <- vector("list", length(rows))
  t <- 1L
  l <- 1L
  for (k in seq_along(rows)) {
    if (rows[k] > 0) {
      if (l > 1L) {
        t <- t + 1L
        l <- 1L
      }
      areas[[k]] <- patchwork::area(t, 1L, t + rows[k] - 1L, ncol)
      t <- t + rows[k]
    } else {
      areas[[k]] <- patchwork::area(t, l)
      l <- l + 1L
      if (l > ncol) {
        l <- 1L
        t <- t + 1L
      }
    }
  }
  do.call(c, areas)
}
