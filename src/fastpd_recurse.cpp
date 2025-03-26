#include <Rcpp.h>
#include <algorithm>
#include "../inst/include/glex.h"
#include "comparison_policies.h"

using namespace Rcpp;

template <typename ComparisonPolicy>
NumericMatrix recurseMarginalizeS(
    NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &Ss, unsigned int node,
    LeafData &leaf_data)
{
  NumericMatrix::Row current_node = tree(node, _);
  int current_feature = current_node[Index::FEATURE];

  // Start with all 0
  unsigned int n = x.nrow();
  NumericMatrix mat(n, Ss.size());

  // If leaf, just return value
  if (current_feature == -1)
  {
    for (unsigned int j = 0; j < Ss.size(); ++j)
    {
      std::set<unsigned int> to_explain = {};
      std::set_intersection(
          Ss[j].begin(), Ss[j].end(),
          leaf_data.encountered[node].begin(), leaf_data.encountered[node].end(),
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
    NumericMatrix mat_yes = recurseMarginalizeS<ComparisonPolicy>(x, tree, Ss, yes, leaf_data);
    NumericMatrix mat_no = recurseMarginalizeS<ComparisonPolicy>(x, tree, Ss, no, leaf_data);

    for (unsigned int j = 0; j < Ss.size(); ++j)
    {
      // Is splitting feature out in this subset?
      if (Ss[j].find(current_feature) == Ss[j].end())
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
          mat(i, j) += ComparisonPolicy::compare(x(i, current_feature), split) ? mat_yes(i, j) : mat_no(i, j);
        }
      }
    }
  }

  // Return combined matrix
  return mat;
}

template <typename ComparisonPolicy>
Rcpp::NumericMatrix recurseMarginalizeU(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data)
{
  NumericMatrix::Row current_node = tree(node, _);
  int current_feature = current_node[Index::FEATURE];

  // Start with all 0
  unsigned int n = x.nrow();
  Rcpp::NumericMatrix mat(n, U.size());

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

      Rcpp::NumericMatrix::Column to_fill = mat(Rcpp::_, j);
      std::fill(to_fill.begin(), to_fill.end(), expected_value);
    }
  }
  else
  {
    unsigned int yes = current_node[Index::YES];
    unsigned int no = current_node[Index::NO];

    // Call both children, they give a matrix each of all obs and subsets
    Rcpp::NumericMatrix mat_yes = recurseMarginalizeU<ComparisonPolicy>(x, tree, U, yes, leaf_data);
    Rcpp::NumericMatrix mat_no = recurseMarginalizeU<ComparisonPolicy>(x, tree, U, no, leaf_data);

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
          mat(i, j) += ComparisonPolicy::compare(x(i, current_feature), split) ? mat_yes(i, j) : mat_no(i, j);
        }
      }
    }
  }

  // Return combined matrix
  return mat;
}

// R interface functions
Rcpp::NumericMatrix recurseMarginalizeSWeakComparison(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &Ss, unsigned int node,
    LeafData &leaf_data)
{
  return recurseMarginalizeS<glex::WeakComparison>(x, tree, Ss, node, leaf_data);
}

Rcpp::NumericMatrix recurseMarginalizeSStrictComparison(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &Ss, unsigned int node,
    LeafData &leaf_data)
{
  return recurseMarginalizeS<glex::StrictComparison>(x, tree, Ss, node, leaf_data);
}

Rcpp::NumericMatrix recurseMarginalizeUWeakComparison(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data)
{
  return recurseMarginalizeU<glex::WeakComparison>(x, tree, U, node, leaf_data);
}

Rcpp::NumericMatrix recurseMarginalizeUStrictComparison(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data)
{
  return recurseMarginalizeU<glex::StrictComparison>(x, tree, U, node, leaf_data);
}
