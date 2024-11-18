
#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"

using namespace Rcpp;
LeafData augmentTreeWeakComparison(NumericMatrix &tree, NumericMatrix &dataset);
LeafData augmentTreeStrictComparison(NumericMatrix &tree, NumericMatrix &dataset);
std::vector<std::set<unsigned int>> get_all_subsets_(std::set<unsigned int> &set);

Rcpp::NumericMatrix recurseMarginalizeUWeakComparison(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data);

Rcpp::NumericMatrix recurseMarginalizeUStrictComparison(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data);

double augmentExpectationWeakComparison(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data);
double augmentExpectationStrictComparison(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data);

// [[Rcpp::export]]
double augmentAndTakeExpectation(NumericVector &x, NumericMatrix &dataset, NumericMatrix &tree, NumericVector &to_explain, bool is_weak_inequality)
{
  LeafData leaf_data = is_weak_inequality ? augmentTreeWeakComparison(tree, dataset) : augmentTreeStrictComparison(tree, dataset);
  return is_weak_inequality ? augmentExpectationWeakComparison(x, tree, to_explain, leaf_data) : augmentExpectationStrictComparison(x, tree, to_explain, leaf_data);
}

// [[Rcpp::export]]
XPtr<LeafData> augmentTree(NumericMatrix &tree, NumericMatrix &dataset, bool is_weak_inequality)
{
  LeafData *leaf_data = new LeafData(is_weak_inequality ? augmentTreeWeakComparison(tree, dataset) : augmentTreeStrictComparison(tree, dataset)); // Dynamically allocate
  XPtr<LeafData> ptr(leaf_data, true);                                                                                                            // true enables automatic memory management
  return ptr;
}

// [[Rcpp::export]]
double augmentExpectation(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, SEXP leaf_data_ptr, bool is_weak_inequality)
{
  const Rcpp::XPtr<LeafData> leaf_data(leaf_data_ptr);
  return is_weak_inequality ? augmentExpectationWeakComparison(x, tree, to_explain, *leaf_data) : augmentExpectationStrictComparison(x, tree, to_explain, *leaf_data);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix marginalizeAllSplittedSubsetsinTree(
    Rcpp::NumericMatrix &x,
    NumericMatrix &tree,
    bool is_weak_inequality)
{
  LeafData leaf_data = is_weak_inequality ? augmentTreeWeakComparison(tree, x) : augmentTreeStrictComparison(tree, x);
  std::vector<std::set<unsigned int>> U = get_all_subsets_(leaf_data.all_encountered);
  return is_weak_inequality ? recurseMarginalizeUWeakComparison(x, tree, U, 0, leaf_data) : recurseMarginalizeUStrictComparison(x, tree, U, 0, leaf_data);
}

double augmentExpectationWeakComparison(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data)
{
  std::stack<unsigned int> to_process;
  to_process.push(0);

  double result = 0;
  std::set<unsigned int> to_explain_set(to_explain.begin(), to_explain.end());

  while (!to_process.empty())
  {
    int node_idx = to_process.top();
    to_process.pop();

    NumericMatrix::Row current_node = tree(node_idx, _);
    int current_feature = current_node[Index::FEATURE];
    double split = current_node[Index::SPLIT];

    if (current_feature == -1)
    {
      std::set<unsigned int> to_marginalize = {};
      if (!to_explain_set.empty())
        std::set_intersection(
            to_explain_set.begin(), to_explain_set.end(),
            leaf_data.encountered[node_idx].begin(), leaf_data.encountered[node_idx].end(),
            std::inserter(to_marginalize, to_marginalize.begin()));

      double p = leaf_data.leafProbs[node_idx][to_marginalize];
      result += tree(node_idx, Index::QUALITY) * p;
      continue;
    }

    if (std::find(to_explain.begin(), to_explain.end(), current_feature) != to_explain.end())
    {
      if (x[current_feature] <= split)
      {
        to_process.push(current_node[Index::YES]);
      }
      else
      {
        to_process.push(current_node[Index::NO]);
      }
    }
    else
    {
      to_process.push(current_node[Index::YES]);
      to_process.push(current_node[Index::NO]);
    }
  }

  return result;
}

double augmentExpectationStrictComparison(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data)
{
  std::stack<unsigned int> to_process;
  to_process.push(0);

  double result = 0;
  std::set<unsigned int> to_explain_set(to_explain.begin(), to_explain.end());

  while (!to_process.empty())
  {
    int node_idx = to_process.top();
    to_process.pop();

    NumericMatrix::Row current_node = tree(node_idx, _);
    int current_feature = current_node[Index::FEATURE];
    double split = current_node[Index::SPLIT];

    if (current_feature == -1)
    {
      std::set<unsigned int> to_marginalize = {};
      if (!to_explain_set.empty())
        std::set_intersection(
            to_explain_set.begin(), to_explain_set.end(),
            leaf_data.encountered[node_idx].begin(), leaf_data.encountered[node_idx].end(),
            std::inserter(to_marginalize, to_marginalize.begin()));

      double p = leaf_data.leafProbs[node_idx][to_marginalize];
      result += tree(node_idx, Index::QUALITY) * p;
      continue;
    }

    if (std::find(to_explain.begin(), to_explain.end(), current_feature) != to_explain.end())
    {
      if (x[current_feature] < split)
      {
        to_process.push(current_node[Index::YES]);
      }
      else
      {
        to_process.push(current_node[Index::NO]);
      }
    }
    else
    {
      to_process.push(current_node[Index::YES]);
      to_process.push(current_node[Index::NO]);
    }
  }

  return result;
}
