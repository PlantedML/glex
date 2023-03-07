test_that("find_term_matches works", {
  # Try ot some p, degree combinations
  for (p in 2:6) {
    for (degree in seq_len(p)) {

      main_terms <- letters[seq_len(p)]

      # Generate terms from main to `degree` of interaction
      term_list <- unlist(sapply(seq_len(degree), \(k) {
        combn(main_terms, k, simplify = FALSE) |>
          vapply(function(x) paste(x, collapse = ":"), character(1))
      }))

      for (match_term in main_terms) {
        expect_equal(
          find_term_matches(match_term, term_list),
          which(vapply(strsplit(term_list, ":"), function(i) match_term %in% i, FUN.VALUE = logical(1)))
        )
      }
    }
  }

  # Overlaps
  term_list <- c("x", "xx", "xxx:x")
  match_term <- "x"
  expect_equal(
    find_term_matches(match_term, term_list),
    which(vapply(strsplit(term_list, ":"), function(i) match_term %in% i, FUN.VALUE = logical(1)))
  )

  # No matches return empty integer
  expect_equal(find_term_matches("x", c("a", "b", "cdx:b")), integer(0))

  # If the term to match contains : it doesn't find anything, which is expected (but suboptimal)
  # It should not error though
  expect_equal(find_term_matches("x:a", c("a", "b", "cdx:b", "x:a")), integer(0))

  # Unsupported types
  expect_error(find_term_matches(5, c("a", "b", "cdx:b", "x:a")))
  expect_error(find_term_matches(list("a", "b"), c("a", "b", "cdx:b", "x:a")))
  expect_error(find_term_matches("a", list("a", "b", "cdx:b", "x:a")))
})
