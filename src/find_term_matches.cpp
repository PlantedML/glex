#include <Rcpp.h>

// [[Rcpp::export]]
std::vector<int> find_term_matches(std::string main_term, std::vector<std::string> terms) {
  // Don't know how many matches there are so can't pre-allocate
  std::vector<int> match_indices;

  for (int i = 0; i < terms.size(); i++) {
    std::string term = terms[i];
    std::size_t pos = term.find(":");

    while (pos != std::string::npos) {
      std::string substring = term.substr(0, pos);
      if (substring == main_term) {
        match_indices.push_back(i + 1);
        break;
      }
      term = term.substr(pos + 1);
      pos = term.find(":");
    }

    if (term == main_term) {
      match_indices.push_back(i + 1);
    }
  }

  return match_indices;
}
