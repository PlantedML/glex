#include <Rcpp.h>

// In a vector of terms like c("x1", "x2", "x1:x2"), find the indices of elements that contain a main_term, e.g. "x1".
// Avoiding regex, it's probably okay to make the somewhat strong assumption that ":" is only used to delimit
// interaction terms. Edge cases would be when terms include e.g. "x1" and "x12", where a simple grep-like
// approach would look for main_term "x1" and also match "x12", hence the strong delimiter assumption feels necessary.
// [[Rcpp::export]]
std::vector<int> find_term_matches(std::string main_term, std::vector<std::string> terms) {
  // Don't know how many matches there are so can't pre-allocate
  std::vector<int> match_indices;

  for (int i = 0; i < terms.size(); i++) {
    std::string term = terms[i];
    std::size_t pos = term.find(":");

    // Run as long as there are ":" present in the remaining string
    while (pos != std::string::npos) {
      // Substring of term up to the :
      std::string substring = term.substr(0, pos);
      if (substring == main_term) {
        match_indices.push_back(i + 1);
        break;
      }
      term = term.substr(pos + 1);
      pos = term.find(":");
    }

    // After all : are accounted for (or there never was one), check if remaining term is a match
    if (term == main_term) {
      match_indices.push_back(i + 1);
    }
  }

  return match_indices;
}
