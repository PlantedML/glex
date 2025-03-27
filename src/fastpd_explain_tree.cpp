#include <Rcpp.h>
#include "../inst/include/glex.h"
#include "comparison_policies.h"

using namespace Rcpp;

std::vector<std::set<unsigned int>> get_all_subsets(std::set<unsigned int> &set, unsigned int maxSize);

LeafData augmentTreeWeakComparison(NumericMatrix &tree, NumericMatrix &dataset, unsigned int max_interaction);
LeafData augmentTreeStrictComparison(NumericMatrix &tree, NumericMatrix &dataset, unsigned int max_interaction);

NumericMatrix recurseMarginalizeSWeakComparison(
    NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &Ss, unsigned int node,
    LeafData &leaf_data);
NumericMatrix recurseMarginalizeSStrictComparison(
    NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &Ss, unsigned int node,
    LeafData &leaf_data);

void contributeFastPD2(
    Rcpp::NumericMatrix &mat,
    Rcpp::NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::vector<std::set<unsigned int>> &T_subsets,
    unsigned int colnum);


// [[Rcpp::export]]
Rcpp::NumericMatrix explainTreeFastPD(
    Rcpp::NumericMatrix &x,
    NumericMatrix &tree,
    Rcpp::List &to_explain_list,
    unsigned int max_interaction,
    bool is_weak_inequality)
{
  // Augment step
  LeafData leaf_data = is_weak_inequality ? augmentTreeWeakComparison(tree, x, max_interaction) : augmentTreeStrictComparison(tree, x, max_interaction);
  std::vector<std::set<unsigned int>> U = get_all_subsets(leaf_data.all_encountered, max_interaction);

  // Explain/expectation/marginalization step
  std::vector<std::set<unsigned int>> to_explain; // List of S'es to explain
  unsigned int to_explain_size = to_explain_list.size();
  NumericMatrix m_all = NumericMatrix(x.nrow(), to_explain_size);
  CharacterVector m_all_col_names = CharacterVector(to_explain_size);
  CharacterVector x_col_names = colnames(x);

  std::set<unsigned int> needToComputePDfunctionsFor; // The set of coords we need to compute PD functions of
  for (int S_idx = 0; S_idx < to_explain_size; S_idx++)
  {
    std::set<unsigned int> S = std::set<unsigned int>(
        as<IntegerVector>(to_explain_list[S_idx]).begin(),
        as<IntegerVector>(to_explain_list[S_idx]).end());
    to_explain.push_back(S);

    if (int k = S.size(); k != 0)
    {
      auto it = S.begin();
      std::ostringstream oss;
      for (int i = 0; i < k - 1; i++, it++)
        oss << x_col_names[*it] << ":";
      oss << x_col_names[*it];
      m_all_col_names[S_idx] = oss.str();
    }

    // Check if S \subset T
    if (std::find(U.begin(), U.end(), S) == U.end())
      continue;

    needToComputePDfunctionsFor.insert(S_idx);
  }

  // Compute expectation of all necessary subsets
  NumericMatrix mat = is_weak_inequality ? recurseMarginalizeSWeakComparison(x, tree, U, 0, leaf_data) : recurseMarginalizeSStrictComparison(x, tree, U, 0, leaf_data);

  for (int S_idx : needToComputePDfunctionsFor)
  {
    std::set<unsigned int> S = to_explain[S_idx];
    contributeFastPD2(mat, m_all, S, U, S_idx);
  }
  colnames(m_all) = m_all_col_names;
  return m_all;
}
