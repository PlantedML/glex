#' Force plot of a single prediction
#'
#' A compact view of the same information as [plot_waterfall()]: the components
#' of one observation's prediction as segments of a single band. Positive terms
#' push the prediction up from the left, negative terms push it down from the
#' right, and both meet at the prediction f(x). The largest terms sit next to
#' f(x).
#'
#' @inheritParams plot_waterfall
#' @details
#' Only segments at least 5% as wide as the plotted range are labeled; the
#' caption states how many are not. Use `max_terms` or `threshold` to aggregate
#' small terms instead.
#'
#' @returns A [ggplot][ggplot2::ggplot] object.
#' @export
#' @family Visualization functions
#' @examplesIf requireNamespace("randomPlantedForest", quietly = TRUE)
#' library(randomPlantedForest)
#' set.seed(1)
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
#' gl <- glex(rp, mtcars[27:32, ])
#'
#' plot_force(gl, id = 3, max_terms = 6)
plot_force <- function(
  object,
  id,
  threshold = 0,
  max_terms = 10L,
  max_interaction = NULL,
  class = NULL
) {
  et <- explain_terms(object, id, class, threshold, max_terms, max_interaction)
  d <- force_layout(et)
  xrange <- diff(range(c(d$start, d$end, et$intercept, et$prediction)))
  if (xrange == 0) {
    xrange <- 1
  }

  d$labelled <- abs(d$m) >= 0.05 * xrange
  d$mid <- (d$start + d$end) / 2
  # Neighboring labels on the same side alternate between two rows to avoid overlap
  d$tier <- 0
  for (side in c(1, -1)) {
    idx <- which(d$labelled & d$dir == side)
    idx <- idx[order(d$mid[idx])]
    d$tier[idx] <- (seq_along(idx) - 1) %% 2
  }
  d$y <- d$dir * (0.55 + 0.55 * d$tier)
  d$vjust <- ifelse(d$dir == 1, 0, 1)
  d$text <- paste0(d$label, "\n", format_contribution(d$m))
  n_unlabelled <- sum(!d$labelled)

  ggplot() +
    geom_polygon(
      data = force_polygons(d, tip = 0.008 * xrange),
      aes(
        x = .data$x,
        y = .data$y,
        group = .data$group,
        fill = .data$sign,
        alpha = .data$alpha
      ),
      colour = "white",
      linewidth = 0.3
    ) +
    geom_text(
      data = d[d$labelled, ],
      aes(x = .data$mid, y = .data$y, label = .data$text, vjust = .data$vjust),
      size = 3.2,
      lineheight = 0.9
    ) +
    scale_fill_manual(values = sign_colors(), guide = "none") +
    scale_alpha_identity() +
    scale_x_continuous(
      expand = expansion(mult = 0.05),
      sec.axis = reference_axis(et, xrange)
    ) +
    scale_y_continuous(limits = c(-2.2, 2.2), breaks = NULL) +
    coord_cartesian(clip = "off") +
    labs(
      title = prediction_title(id, et, class),
      subtitle = terms_subtitle(et),
      caption = if (n_unlabelled > 0) {
        sprintf(
          "%d %s too narrow to label",
          n_unlabelled,
          if (n_unlabelled == 1) "term" else "terms"
        )
      },
      x = "Prediction",
      y = NULL
    ) +
    theme_glex(grid_x = FALSE, grid_y = TRUE)
}

#' Place force plot segments: positives end at f(x), negatives start there
#' @param et Result of `explain_terms()`.
#' @keywords internal
#' @noRd
force_layout <- function(et) {
  d <- data.table::copy(et$terms)
  # Largest segments sit next to f(x)
  pos <- d[d$m > 0, ][order(d$m[d$m > 0]), ]
  neg <- d[d$m < 0, ][order(d$m[d$m < 0]), ]

  pos$end <- et$prediction - sum(pos$m) + cumsum(pos$m)
  pos$start <- pos$end - pos$m
  pos$dir <- rep(1, nrow(pos))
  pos$at_fx <- seq_len(nrow(pos)) == nrow(pos)

  neg$start <- et$prediction + c(0, cumsum(-neg$m))[seq_len(nrow(neg))]
  neg$end <- neg$start - neg$m
  neg$dir <- rep(-1, nrow(neg))
  neg$at_fx <- seq_len(nrow(neg)) == 1L

  rbind(pos, neg)
}

#' Chevron-shaped polygons, six points per segment, pointing toward f(x)
#' @keywords internal
#' @noRd
force_polygons <- function(d, tip) {
  if (nrow(d) == 0) {
    return(data.frame(
      x = numeric(0),
      y = numeric(0),
      group = integer(0),
      sign = character(0),
      alpha = numeric(0)
    ))
  }
  half <- 0.4
  do.call(
    rbind,
    lapply(seq_len(nrow(d)), function(i) {
      a <- d$start[i]
      b <- d$end[i]
      # Fronts meeting at f(x) stay flat so both signs' tips do not overlap there
      front <- if (d$at_fx[i]) 0 else tip
      x <- if (d$dir[i] == 1) {
        c(a, b, b + front, b, a, a + tip)
      } else {
        c(b, a, a - front, a, b, b - tip)
      }
      data.frame(
        x = x,
        y = c(-half, -half, 0, half, half, 0),
        group = i,
        sign = as.character(d$dir[i]),
        alpha = degree_alpha(d$degree[i])
      )
    })
  )
}
