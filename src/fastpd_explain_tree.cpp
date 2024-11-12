#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"

using namespace Rcpp;

std::vector<std::set<unsigned int>> get_all_subsets_(std::set<unsigned int> &set);
void contributeFastPD(
    Rcpp::NumericMatrix &mat,
    Rcpp::NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::set<unsigned int> &T,
    std::vector<std::set<unsigned int>> &T_subsets,
    unsigned int colnum);
LeafData augmentTree_(NumericMatrix &tree, NumericMatrix &dataset);

Rcpp::NumericMatrix recurseMarginalizeU_(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data);

// [[Rcpp::export]]
Rcpp::NumericMatrix explainTreeFastPD(
    Rcpp::NumericMatrix &x,
    NumericMatrix &tree,
    Rcpp::List &to_explain_list)
{
  // Augment step
  LeafData leaf_data = augmentTree_(tree, x);
  std::vector<std::set<unsigned int>> U = get_all_subsets_(leaf_data.all_encountered);

  // Explain/expectation/marginalization step
  std::vector<std::set<unsigned int>> to_explain;    // List of S'es to explain
  std::set<unsigned int> needToComputePDfunctionsOf; // The set of coords we need to compute PD functions of
  unsigned int to_explain_size = to_explain.size();
  NumericMatrix m_all = NumericMatrix(x.nrow(), to_explain_size);
  CharacterVector m_all_col_names = CharacterVector(to_explain_size);
  CharacterVector x_col_names = colnames(x);

  for (int S_idx = 0; S_idx < to_explain_size; S_idx++)
  {
    std::set<unsigned int> to_explain_set = std::set<unsigned int>(
        as<IntegerVector>(to_explain_list[S_idx]).begin(),
        as<IntegerVector>(to_explain_list[S_idx]).end());
    to_explain.push_back(to_explain_set);
    // Check if S \subset T
    if (std::find(U.begin(), U.end(), to_explain_set) == U.end())
    {
      continue;
    }

    needToComputePDfunctionsOf.insert(to_explain_set.begin(), to_explain_set.end());
  }

  // Compute expectation of all necessary subsets
  NumericMatrix mat = recurseMarginalizeU_(x, tree, U, 0, leaf_data);

  for (int S_idx = 0; S_idx < to_explain_size; S_idx++)
  {
    std::set<unsigned int> S = to_explain[S_idx];

    if (int k = S.size(); k != 0)
    {
      auto it = S.begin();
      std::ostringstream oss;
      for (int i = 0; i < k - 1; i++, it++)
        oss << x_col_names[*it] << ":";
      oss << x_col_names[*it];
      m_all_col_names[S_idx] = oss.str();
    }

    if (std::find(U.begin(), U.end(), S) == U.end())
      continue;
    // std::set<unsigned int> S_set = std::set<unsigned int>(S.begin(), S.end());
    contributeFastPD(mat, m_all, S, leaf_data.all_encountered, U, S_idx);
  }
  colnames(m_all) = m_all_col_names;
  return m_all;
}
