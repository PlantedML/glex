m_to_shap <- function(object) {
  m_long <- melt_m(object$m, object$target_levels)
  m_long[, degree := get_degree(term)]
  m_long[, m := m / degree]
  main_terms <- names(object$x)

  # Iterate over all predictors and keep them as "reference term", so all main- and interaction effects can be
  # collected under one reference term, e.g. "x1" and "x1:x2" belong to x1, but "x1:x2" also belongs to x2
  xdf <- data.table::rbindlist(lapply(main_terms, function(main_term) {
    # get all terms associated with the current reference term
    mtemp <- m_long[find_term_matches(main_term, term), ]
    mtemp$reference_term <- main_term
    mtemp
  }))

  shaps <- xdf[, list(shap = sum(m)), by = c(".id", "reference_term")]
  data.table::dcast(shaps, .id ~ reference_term, value.var = "shap")
}


bench::mark(
  m_to_shap(components),
  #m_to_shap2(components),
  #m_to_shap3(components),
  iterations = 2,
  check = TRUE
)


profvis::profvis(m_to_shap(components))

terms <- names(components$m)


# Ho many main terms
p <- 4

# generate interaction terms
terms <- unlist(sapply(seq_len(p), \(k) {
  combn(letters[1:p], k, simplify = FALSE) |>
    vapply(\(x) paste(x, collapse = ":"), character(1))
}))

head(terms, 20)
