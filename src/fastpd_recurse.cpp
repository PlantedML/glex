
#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"

using namespace Rcpp;

bool containsNumber(IntegerVector &vec, int target);
std::vector<std::set<unsigned int>> get_all_subsets_(std::set<unsigned int> &set);

NumericMatrix recurseMarginalizeU_(
    NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data)
{
  NumericMatrix::Row current_node = tree(node, _);
  int current_feature = current_node[Index::FEATURE];

  // Start with all 0
  unsigned int n = x.nrow();
  NumericMatrix mat(n, U.size());

  // If leaf, just return value
  if (current_feature == -1)
  {
    for (unsigned int j = 0; j < U.size(); ++j)
    {
      std::set<unsigned int> to_explain = {};
      std::set_difference(
          leaf_data.encountered[node].begin(), leaf_data.encountered[node].end(),
          U[j].begin(), U[j].end(),
          std::inserter(to_explain, to_explain.begin()));

      double p = leaf_data.leafProbs[node][to_explain];
      double quality = tree(node, Index::QUALITY);
      double expected_value = quality * p;

      NumericMatrix::Column to_fill = mat(_, j);
      std::fill(to_fill.begin(), to_fill.end(), expected_value);
    }
  }
  else
  {
    unsigned int yes = current_node[Index::YES];
    unsigned int no = current_node[Index::NO];

    // Call both children, they give a matrix each of all obs and subsets
    NumericMatrix mat_yes = recurseMarginalizeU_(x, tree, U, yes, leaf_data);
    NumericMatrix mat_no = recurseMarginalizeU_(x, tree, U, no, leaf_data);

    for (unsigned int j = 0; j < U.size(); ++j)
    {
      // Is splitting feature out in this subset?

      if (U[j].find(current_feature) != U[j].end())
      {
        // For subsets where feature is out, weighted average of left/right
        for (unsigned int i = 0; i < n; ++i)
          mat(i, j) += mat_yes(i, j) + mat_no(i, j);
      }
      else
      {
        double split = current_node[Index::SPLIT];
        // For subsets where feature is in, split to left/right
        for (unsigned int i = 0; i < n; ++i)
        {
          mat(i, j) += (x(i, current_feature) < split) ? mat_yes(i, j) : mat_no(i, j);
        }
      }
    }
  }

  // Return combined matrix
  return mat;
}

LeafData augmentTree_(NumericMatrix &tree, NumericMatrix &dataset);

// [[Rcpp::export]]
NumericMatrix marginalizeAllSplittedSubsetsinTree(
    NumericMatrix &x,
    NumericMatrix &tree)
{
  LeafData leaf_data = augmentTree_(tree, x);
  std::vector<std::set<unsigned int>> U = get_all_subsets_(leaf_data.all_encountered);
  return recurseMarginalizeU_(x, tree, U, 0, leaf_data);
}

void contributeFastPD(
    NumericMatrix &mat,
    NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::set<unsigned int> &T,
    std::vector<std::set<unsigned int>> &T_subsets,
    unsigned int colnum)
{
  std::set<unsigned int> sTS;
  std::set_difference(T.begin(), T.end(), S.begin(), S.end(), std::inserter(sTS, sTS.begin()));

  for (unsigned int i = 0; i < T_subsets.size(); ++i)
  {
    std::set<unsigned int> U = T_subsets[i];
    if (sTS.size() != 0)
    {
      std::set<unsigned int> ssTSU;
      std::set_difference(sTS.begin(), sTS.end(), U.begin(), U.end(), std::inserter(ssTSU, ssTSU.begin()));
      if (ssTSU.size() != 0)
        continue;
    }

    std::set<unsigned int> sTU;
    std::set_difference(T.begin(), T.end(), U.begin(), U.end(), std::inserter(sTU, sTU.begin()));

    if (((S.size() - sTU.size()) % 2) == 0)
      m_all(_, colnum) = m_all(_, colnum) + mat(_, i);
    else
      m_all(_, colnum) = m_all(_, colnum) - mat(_, i);
  }
}
