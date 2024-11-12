
#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"

using namespace Rcpp;

double augmentExpectation_(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data)
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

LeafData augmentTree_(NumericMatrix &tree, NumericMatrix &dataset);
// [[Rcpp::export]]
double augmentAndTakeExpectation(NumericVector &x, NumericMatrix &dataset, NumericMatrix &tree, NumericVector &to_explain)
{
  LeafData leaf_data = augmentTree_(tree, dataset);
  return augmentExpectation_(x, tree, to_explain, leaf_data);
}

// [[Rcpp::export]]
double augmentExpectation(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, SEXP leaf_data_ptr)
{
  const Rcpp::XPtr<LeafData> leaf_data(leaf_data_ptr);
  return augmentExpectation_(x, tree, to_explain, *leaf_data);
}
