#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"

using namespace Rcpp;
std::vector<std::set<unsigned int>> get_all_subsets_(std::set<unsigned int> &set);

void augmentTreeRecurseStepRanger(
    AugmentedData passed_down,
    LeafData &leaf_data,
    NumericMatrix &tree,
    NumericMatrix &dataset,
    unsigned int node)
{
  NumericMatrix::Row current_node = tree(node, _);

  int current_feature = current_node[Index::FEATURE];
  double split = current_node[Index::SPLIT];

  if (current_feature == -1)
  {
    // Leaf node
    leaf_data.encountered[node] = passed_down.encountered;
    leaf_data.leafProbs[node] = ProbsMap();
    for (const auto &[subset, path_dat] : passed_down.pathData)
    {
      leaf_data.leafProbs[node][subset] = (double)path_dat.size() / dataset.nrow();
    }
    return;
  }
  else if (leaf_data.all_encountered.find(current_feature) == leaf_data.all_encountered.end())
  {
    leaf_data.all_encountered.insert(current_feature);
  }

  AugmentedData passed_down_yes = {
      .encountered = passed_down.encountered,
      .pathData = PathData(),
  };
  AugmentedData passed_down_no = {
      .encountered = passed_down.encountered,
      .pathData = PathData(),
  };

  for (const auto &[subset, path_dat] : passed_down.pathData)
  {
    if (subset.find(current_feature) != subset.end())
    {
      // Feature is in the path
      passed_down_yes.pathData[subset] = path_dat;
      passed_down_no.pathData[subset] = path_dat;
    }
    else
    {
      // Feature is not in the path
      passed_down_yes.pathData[subset] = std::vector<unsigned int>();
      passed_down_no.pathData[subset] = std::vector<unsigned int>();
      for (int i : path_dat)
      {
        if (dataset(i, current_feature) <= split)
        {
          passed_down_yes.pathData[subset].push_back(i);
        }
        else
        {
          passed_down_no.pathData[subset].push_back(i);
        }
      }
    }
  }

  if (passed_down.encountered.find(current_feature) == passed_down.encountered.end())
  {
    passed_down_yes.encountered.insert(current_feature);
    passed_down_no.encountered.insert(current_feature);

    for (const auto &[subset, path_dat] : passed_down.pathData)
    {
      std::set to_add = subset;
      to_add.insert(current_feature);

      passed_down_yes.pathData[to_add] = path_dat;
      passed_down_no.pathData[to_add] = path_dat;
    }
  }

  unsigned int yes = current_node[Index::YES];
  unsigned int no = current_node[Index::NO];

  augmentTreeRecurseStepRanger(passed_down_yes, leaf_data, tree, dataset, yes);
  augmentTreeRecurseStepRanger(passed_down_no, leaf_data, tree, dataset, no);
}

void augmentTreeRecurseStepXgboost(
    AugmentedData passed_down,
    LeafData &leaf_data,
    NumericMatrix &tree,
    NumericMatrix &dataset,
    unsigned int node)
{
  NumericMatrix::Row current_node = tree(node, _);

  int current_feature = current_node[Index::FEATURE];
  double split = current_node[Index::SPLIT];

  if (current_feature == -1)
  {
    // Leaf node
    leaf_data.encountered[node] = passed_down.encountered;
    leaf_data.leafProbs[node] = ProbsMap();
    for (const auto &[subset, path_dat] : passed_down.pathData)
    {
      leaf_data.leafProbs[node][subset] = (double)path_dat.size() / dataset.nrow();
    }
    return;
  }
  else if (leaf_data.all_encountered.find(current_feature) == leaf_data.all_encountered.end())
  {
    leaf_data.all_encountered.insert(current_feature);
  }

  AugmentedData passed_down_yes = {
      .encountered = passed_down.encountered,
      .pathData = PathData(),
  };
  AugmentedData passed_down_no = {
      .encountered = passed_down.encountered,
      .pathData = PathData(),
  };

  for (const auto &[subset, path_dat] : passed_down.pathData)
  {
    if (subset.find(current_feature) != subset.end())
    {
      // Feature is in the path
      passed_down_yes.pathData[subset] = path_dat;
      passed_down_no.pathData[subset] = path_dat;
    }
    else
    {
      // Feature is not in the path
      passed_down_yes.pathData[subset] = std::vector<unsigned int>();
      passed_down_no.pathData[subset] = std::vector<unsigned int>();
      for (int i : path_dat)
      {
        if (dataset(i, current_feature) < split)
        {
          passed_down_yes.pathData[subset].push_back(i);
        }
        else
        {
          passed_down_no.pathData[subset].push_back(i);
        }
      }
    }
  }

  if (passed_down.encountered.find(current_feature) == passed_down.encountered.end())
  {
    passed_down_yes.encountered.insert(current_feature);
    passed_down_no.encountered.insert(current_feature);

    for (const auto &[subset, path_dat] : passed_down.pathData)
    {
      std::set to_add = subset;
      to_add.insert(current_feature);

      passed_down_yes.pathData[to_add] = path_dat;
      passed_down_no.pathData[to_add] = path_dat;
    }
  }

  unsigned int yes = current_node[Index::YES];
  unsigned int no = current_node[Index::NO];

  augmentTreeRecurseStepXgboost(passed_down_yes, leaf_data, tree, dataset, yes);
  augmentTreeRecurseStepXgboost(passed_down_no, leaf_data, tree, dataset, no);
}

LeafData augmentTreeRanger(NumericMatrix &tree, NumericMatrix &dataset)
{
  AugmentedData result;

  AugmentedData to_pass_down = {
      .encountered = std::set<unsigned int>(),
      .pathData = PathData(),
  };
  to_pass_down.pathData[{}] = std::vector<unsigned int>(dataset.nrow());
  std::iota(to_pass_down.pathData[{}].begin(), to_pass_down.pathData[{}].end(), 0);
  LeafData leaf_data = LeafData();

  augmentTreeRecurseStepRanger(to_pass_down, leaf_data, tree, dataset, 0);
  return leaf_data;
}

LeafData augmentTreeXgboost(NumericMatrix &tree, NumericMatrix &dataset)
{
  AugmentedData result;

  AugmentedData to_pass_down = {
      .encountered = std::set<unsigned int>(),
      .pathData = PathData(),
  };
  to_pass_down.pathData[{}] = std::vector<unsigned int>(dataset.nrow());
  std::iota(to_pass_down.pathData[{}].begin(), to_pass_down.pathData[{}].end(), 0);
  LeafData leaf_data = LeafData();

  augmentTreeRecurseStepXgboost(to_pass_down, leaf_data, tree, dataset, 0);
  return leaf_data;
}

double augmentExpectationRanger(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data)
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

double augmentExpectationXgboost(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data)
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

// [[Rcpp::export]]
double augmentAndTakeExpectation(NumericVector &x, NumericMatrix &dataset, NumericMatrix &tree, NumericVector &to_explain, bool is_ranger)
{
  LeafData leaf_data = is_ranger ? augmentTreeRanger(tree, dataset) : augmentTreeXgboost(tree, dataset);
  return is_ranger ? augmentExpectationRanger(x, tree, to_explain, leaf_data) : augmentExpectationXgboost(x, tree, to_explain, leaf_data);
}

// [[Rcpp::export]]
XPtr<LeafData> augmentTree(NumericMatrix &tree, NumericMatrix &dataset, bool is_ranger)
{
  LeafData *leaf_data = new LeafData(is_ranger ? augmentTreeRanger(tree, dataset) : augmentTreeXgboost(tree, dataset)); // Dynamically allocate
  XPtr<LeafData> ptr(leaf_data, true);                                                                                  // true enables automatic memory management
  return ptr;
}

// [[Rcpp::export]]
double augmentExpectation(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, SEXP leaf_data_ptr, bool is_ranger)
{
  const Rcpp::XPtr<LeafData> leaf_data(leaf_data_ptr);
  return is_ranger ? augmentExpectationRanger(x, tree, to_explain, *leaf_data) : augmentExpectationXgboost(x, tree, to_explain, *leaf_data);
}

Rcpp::NumericMatrix recurseMarginalizeURanger(
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
    Rcpp::NumericMatrix mat_yes = recurseMarginalizeURanger(x, tree, U, yes, leaf_data);
    Rcpp::NumericMatrix mat_no = recurseMarginalizeURanger(x, tree, U, no, leaf_data);

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
          mat(i, j) += (x(i, current_feature) <= split) ? mat_yes(i, j) : mat_no(i, j);
        }
      }
    }
  }

  // Return combined matrix
  return mat;
}

Rcpp::NumericMatrix recurseMarginalizeUXgboost(
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
    Rcpp::NumericMatrix mat_yes = recurseMarginalizeUXgboost(x, tree, U, yes, leaf_data);
    Rcpp::NumericMatrix mat_no = recurseMarginalizeUXgboost(x, tree, U, no, leaf_data);

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

// [[Rcpp::export]]
Rcpp::NumericMatrix marginalizeAllSplittedSubsetsinTree(
    Rcpp::NumericMatrix &x,
    NumericMatrix &tree,
    bool is_ranger)
{
  LeafData leaf_data = is_ranger ? augmentTreeRanger(tree, x) : augmentTreeXgboost(tree, x);
  std::vector<std::set<unsigned int>> U = get_all_subsets_(leaf_data.all_encountered);
  return is_ranger ? recurseMarginalizeURanger(x, tree, U, 0, leaf_data) : recurseMarginalizeUXgboost(x, tree, U, 0, leaf_data);
}

void contributeFastPD(
    Rcpp::NumericMatrix &mat,
    Rcpp::NumericMatrix &m_all,
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
      m_all(Rcpp::_, colnum) = m_all(Rcpp::_, colnum) + mat(Rcpp::_, i);
    else
      m_all(Rcpp::_, colnum) = m_all(Rcpp::_, colnum) - mat(Rcpp::_, i);
  }
}

// [[Rcpp::export]]
Rcpp::NumericMatrix explainTreeFastPD(
    Rcpp::NumericMatrix &x,
    NumericMatrix &tree,
    Rcpp::List &to_explain_list,
    bool is_ranger)
{
  std::vector<std::set<unsigned int>> to_explain;
  for (int i = 0; i < to_explain_list.size(); i++)
  {
    std::set<unsigned int> to_explain_set = std::set<unsigned int>(as<IntegerVector>(to_explain_list[i]).begin(), as<IntegerVector>(to_explain_list[i]).end());
    to_explain.push_back(to_explain_set);
  }
  LeafData leaf_data = is_ranger ? augmentTreeRanger(tree, x) : augmentTreeXgboost(tree, x);
  std::vector<std::set<unsigned int>> U = get_all_subsets_(leaf_data.all_encountered);
  NumericMatrix mat = is_ranger ? recurseMarginalizeURanger(x, tree, U, 0, leaf_data) : recurseMarginalizeUXgboost(x, tree, U, 0, leaf_data);

  unsigned int to_explain_size = to_explain.size();
  NumericMatrix m_all = NumericMatrix(x.nrow(), to_explain_size);
  CharacterVector m_all_col_names = CharacterVector(to_explain_size);
  CharacterVector x_col_names = colnames(x);

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
