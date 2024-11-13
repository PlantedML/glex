#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"

using namespace Rcpp;

void augmentTreeRecurseStepUptoMaxInteraction(
    AugmentedData passed_down,
    LeafData &leaf_data,
    NumericMatrix &tree,
    NumericMatrix &dataset,
    unsigned int max_interaction,
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
      if (subset.size() >= max_interaction)
        continue;
      std::set to_add = subset;
      to_add.insert(current_feature);

      passed_down_yes.pathData[to_add] = path_dat;
      passed_down_no.pathData[to_add] = path_dat;
    }
  }

  unsigned int yes = current_node[Index::YES];
  unsigned int no = current_node[Index::NO];

  augmentTreeRecurseStepUptoMaxInteraction(passed_down_yes, leaf_data, tree, dataset, max_interaction, yes);
  augmentTreeRecurseStepUptoMaxInteraction(passed_down_no, leaf_data, tree, dataset, max_interaction, no);
}

LeafData augmentTree_(NumericMatrix &tree, NumericMatrix &dataset, unsigned int max_interaction = UINT_MAX)
{
  AugmentedData result;

  AugmentedData to_pass_down = {
      .encountered = std::set<unsigned int>(),
      .pathData = PathData(),
  };
  to_pass_down.pathData[{}] = std::vector<unsigned int>(dataset.nrow());
  std::iota(to_pass_down.pathData[{}].begin(), to_pass_down.pathData[{}].end(), 0);

  // Initialize LeafData with max_interaction
  LeafData leaf_data(max_interaction);

  augmentTreeRecurseStepUptoMaxInteraction(to_pass_down, leaf_data, tree, dataset, max_interaction, 0);

  return leaf_data;
}

// [[Rcpp::export]]
XPtr<LeafData> augmentTree(NumericMatrix &tree, NumericMatrix &dataset)
{
  LeafData *leaf_data = new LeafData(augmentTree_(tree, dataset)); // Dynamically allocate
  XPtr<LeafData> ptr(leaf_data, true);                             // true enables automatic memory management
  return ptr;
}
