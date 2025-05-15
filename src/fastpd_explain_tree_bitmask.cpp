#include <Rcpp.h>
#include "glex.h"
#include "comparison_policies.h"
#include <vector>
#include <algorithm>
#include <functional>

using namespace Rcpp;

template <typename ComparisonPolicy>
void augmentTreeRecurseStepBitmask(
    const AugmentedDataBitMask &passed_down,
    LeafDataBitMask &leaf_data,
    Rcpp::NumericMatrix &tree,
    Rcpp::NumericMatrix &dataset,
    unsigned int node)
{
  // Extract info for this node
  NumericMatrix::Row current_node = tree(node, _);
  const int current_feature = current_node[Index::FEATURE];

  // If it's a leaf node:
  if (current_feature == -1)
  {
    // Record which features were encountered on path to this leaf
    leaf_data.encounteredMask[node] = passed_down.encountered;

    // For each subset in pathData, store the fraction (#rows / totalRows)
    const double denom = static_cast<double>(dataset.nrow());
    for (auto &kv : passed_down.pathData)
    {
      const FeatureMask &subsetMask = kv.first;
      const std::vector<unsigned int> &row_inds = kv.second;
      double fraction = row_inds.empty() ? 0.0 : (row_inds.size() / denom);

      // Store in leafProbs[node]
      leaf_data.leafProbs[node][subsetMask] = fraction;
    }
    return;
  }

  // Not a leaf => we have a splitting feature
  const double split = current_node[Index::SPLIT];
  const unsigned int yes_idx = current_node[Index::YES];
  const unsigned int no_idx = current_node[Index::NO];

  // Mark this feature as encountered in the global sense
  leaf_data.allEncounteredMask = setFeature(leaf_data.allEncounteredMask, current_feature);

  // Prepare child objects (copy encountered, but pathData will be newly built)
  AugmentedDataBitMask passed_down_yes;
  passed_down_yes.encountered = passed_down.encountered;

  AugmentedDataBitMask passed_down_no;
  passed_down_no.encountered = passed_down.encountered;

  // We'll re-populate pathData in yes/no by splitting
  passed_down_yes.pathData.clear();
  passed_down_no.pathData.clear();

  // 1) Split existing subsets if they do NOT contain current_feature.
  //    If they do contain it, we put the same row indices on both sides
  //    (this is how we treat partial-dependence "excluded" features).
  for (auto &kv : passed_down.pathData)
  {
    const FeatureMask &subsetMask = kv.first;
    const std::vector<unsigned int> &row_inds = kv.second;

    // Check membership once
    bool feature_in_subset = hasFeature(subsetMask, current_feature);

    if (feature_in_subset)
    {
      // The subset already includes current_feature => no actual splitting
      passed_down_yes.pathData[subsetMask] = row_inds;
      passed_down_no.pathData[subsetMask] = row_inds;
    }
    else
    {
      // We do threshold-based splitting
      std::vector<unsigned int> yes_vec;
      std::vector<unsigned int> no_vec;
      yes_vec.reserve(row_inds.size());
      no_vec.reserve(row_inds.size());

      for (unsigned int row_id : row_inds)
      {
        double val = dataset(row_id, current_feature);
        if (ComparisonPolicy::compare(val, split))
        {
          yes_vec.push_back(row_id);
        }
        else
        {
          no_vec.push_back(row_id);
        }
      }
      passed_down_yes.pathData[subsetMask] = std::move(yes_vec);
      passed_down_no.pathData[subsetMask] = std::move(no_vec);
    }
  }

  // 2) If we haven't seen this feature in "passed_down.encountered,"
  //    it means we're newly encountering it.
  //    Then we create new subsets that add the feature bit, duplicating rows
  //    (i.e., ignoring threshold for those "excluded" subsets).
  bool is_new_feature = !hasFeature(passed_down.encountered, current_feature);
  if (is_new_feature)
  {
    // Mark it as encountered in these subtrees
    passed_down_yes.encountered = setFeature(passed_down_yes.encountered, current_feature);
    passed_down_no.encountered = setFeature(passed_down_no.encountered, current_feature);

    // Duplicate pathData with the new bit set
    // for each subset in the *original* pathData
    for (auto &kv : passed_down.pathData)
    {
      const FeatureMask &old_subset = kv.first;
      const std::vector<unsigned int> &row_inds = kv.second;

      FeatureMask new_subset = setFeature(old_subset, current_feature);

      passed_down_yes.pathData[new_subset] = row_inds;
      passed_down_no.pathData[new_subset] = row_inds;
    }
  }

  // Recurse
  augmentTreeRecurseStepBitmask<ComparisonPolicy>(passed_down_yes, leaf_data, tree, dataset, yes_idx);
  augmentTreeRecurseStepBitmask<ComparisonPolicy>(passed_down_no, leaf_data, tree, dataset, no_idx);
}

LeafDataBitMask augmentTreeBitmask(
    Rcpp::NumericMatrix &tree,
    Rcpp::NumericMatrix &x,
    bool is_weak_inequality)
{
  int n_nodes = tree.nrow();
  unsigned int n_features = x.ncol(); // Get feature count for proper mask sizing

  // Prepare LeafDataBitMask
  LeafDataBitMask leaf_data;
  leaf_data.encounteredMask.resize(n_nodes, createEmptyMask(n_features));
  leaf_data.leafProbs.resize(n_nodes);
  leaf_data.allEncounteredMask = createEmptyMask(n_features);

  // Build the root "passed_down" data
  AugmentedDataBitMask root;
  root.encountered = createEmptyMask(n_features); // no features encountered at root

  // The entire dataset is active under the empty subset
  {
    std::vector<unsigned int> all_rows;
    all_rows.reserve(x.nrow());
    for (int i = 0; i < x.nrow(); i++)
    {
      all_rows.push_back(i);
    }
    // pathData with key=empty mask, val=all rows
    root.pathData[createEmptyMask(n_features)] = std::move(all_rows);
  }

  // Recurse from node=0 (root)
  if (is_weak_inequality)
  {
    augmentTreeRecurseStepBitmask<glex::WeakComparison>(root, leaf_data, tree, x, /*node=*/0);
  }
  else
  {
    augmentTreeRecurseStepBitmask<glex::StrictComparison>(root, leaf_data, tree, x, /*node=*/0);
  }

  return leaf_data;
}

template <typename ComparisonPolicy>
Rcpp::NumericMatrix recurseMarginalizeSBitmask(
    const Rcpp::NumericMatrix &x,
    const Rcpp::NumericMatrix &tree,
    const std::vector<FeatureMask> &U,
    unsigned int node,
    const LeafDataBitMask &leaf_data)
{
  // Extract row for "node"
  NumericMatrix::ConstRow current_node = tree(node, _);
  const int current_feature = current_node[Index::FEATURE];

  // Dimensions
  const unsigned int n = x.nrow();
  const unsigned int n_subsets = U.size();

  // We'll create an output matrix [n, n_subsets].
  // Using no_init to skip zero fill if we plan to overwrite everything.
  NumericMatrix mat(no_init(n, n_subsets));

  // If leaf node
  if (current_feature == -1)
  {
    // We'll fill each column with (quality * p) for all rows,
    // where p = leafProbs[node][(encounteredMask[node] \ U[j])]
    const double quality = current_node[Index::QUALITY];

    for (unsigned int j = 0; j < n_subsets; ++j)
    {
      FeatureMask intersection = bitmaskAnd(leaf_data.encounteredMask[node], U[j]);

      // Probability from the leafProbs map
      double p = 0.0;
      auto it = leaf_data.leafProbs[node].find(intersection);
      if (it != leaf_data.leafProbs[node].end())
      {
        p = it->second;
      }
      double val = quality * p;

      // Fill column j with val
      double *col_out = &mat(0, j);
      for (unsigned int i = 0; i < n; ++i)
      {
        col_out[i] = val;
      }
    }
    return mat;
  }

  // Internal node => get children
  const unsigned int yes = current_node[Index::YES];
  const unsigned int no = current_node[Index::NO];
  const double split = current_node[Index::SPLIT];

  // Recursively get partial dependence from children
  NumericMatrix mat_yes = recurseMarginalizeSBitmask<ComparisonPolicy>(x, tree, U, yes, leaf_data);
  NumericMatrix mat_no = recurseMarginalizeSBitmask<ComparisonPolicy>(x, tree, U, no, leaf_data);

  // Fill output
  for (unsigned int j = 0; j < n_subsets; ++j)
  {
    const double *col_yes = &mat_yes(0, j);
    const double *col_no = &mat_no(0, j);
    double *col_out = &mat(0, j);

    if (!hasFeature(U[j], current_feature))
    {
      // Feature is "excluded" from the subset => combine yes & no
      for (unsigned int i = 0; i < n; ++i)
      {
        col_out[i] = col_yes[i] + col_no[i];
      }
    }
    else
    {
      // Feature is "included" => row i goes left if x(i,feature)<split, else right
      for (unsigned int i = 0; i < n; ++i)
      {
        double val = x(i, current_feature);
        if (ComparisonPolicy::compare(val, split))
        {
          col_out[i] = col_yes[i];
        }
        else
        {
          col_out[i] = col_no[i];
        }
      }
    }
  }

  return mat;
}

void contributeFastPDBitmask(
    NumericMatrix &mat,
    NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::vector<FeatureMask> &U,
    unsigned int colnum,
    unsigned int t_size)
{
  // Get the maximum size needed for masks based on U's masks
  size_t max_chunks = 0;
  for (const auto &mask : U)
  {
    max_chunks = std::max(max_chunks, mask.size());
  }

  // Create S_bitmask with the same size as U's masks
  FeatureMask S_bitmask = createEmptyMask(max_chunks * BITS_PER_CHUNK);
  for (unsigned int feature : S)
  {
    S_bitmask = setFeature(S_bitmask, feature);
  }

  // Get subsets and ensure they have the same size as U's masks
  std::vector<FeatureMask> Vs = get_all_subsets_of_mask(S_bitmask);
  for (auto &V : Vs)
  {
    if (V.size() < max_chunks)
    {
      V.resize(max_chunks, 0ULL); // Pad with zeros if needed
    }
  }

  for (unsigned int i = 0; i < Vs.size(); ++i)
  {
    const FeatureMask &V = Vs[i];
    auto it = std::find(U.begin(), U.end(), V);
    unsigned int idx = std::distance(U.begin(), it);

    if ((S.size() - countSetBits(V)) % 2 == 0)
    {
      m_all(_, colnum) = m_all(_, colnum) + mat(_, idx);
    }
    else
    {
      m_all(_, colnum) = m_all(_, colnum) - mat(_, idx);
    }
  }
}

// [[Rcpp::export]]
Rcpp::NumericMatrix explainTreeFastPDBitmask(
    Rcpp::NumericMatrix &x,
    Rcpp::NumericMatrix &x_background,
    NumericMatrix &tree,
    Rcpp::List &to_explain_list,
    unsigned int max_interaction,
    bool is_weak_inequality)
{
  // Get feature count for proper mask sizing

  // Augment step using bitmask implementation
  LeafDataBitMask leaf_data = augmentTreeBitmask(tree, x_background, is_weak_inequality);

  // Convert all_encountered to extended bitmask subsets
  FeatureMask all_encountered = leaf_data.allEncounteredMask;
  std::vector<FeatureMask> U = get_all_subsets_of_mask(all_encountered, max_interaction);

  // Explain/expectation/marginalization step
  std::vector<std::set<unsigned int>> to_explain; // List of S'es to explain
  unsigned int to_explain_size = to_explain_list.size();
  NumericMatrix m_all = NumericMatrix(x.nrow(), to_explain_size);
  CharacterVector m_all_col_names = CharacterVector(to_explain_size);
  CharacterVector x_col_names = colnames(x);

  std::set<unsigned int> needToComputePDfunctionsFor; // The set of coords we need to compute PD functions of
  for (unsigned int S_idx = 0; S_idx < to_explain_size; S_idx++)
  {
    std::set<unsigned int> S = std::set<unsigned int>(
        as<IntegerVector>(to_explain_list[S_idx]).begin(),
        as<IntegerVector>(to_explain_list[S_idx]).end());
    to_explain.push_back(S);

    int k = S.size(); // Declare k here
    if (k != 0)
    {
      auto it = S.begin();
      std::ostringstream oss;
      for (int i = 0; i < k - 1; i++, it++)
        oss << x_col_names[*it] << ":";
      oss << x_col_names[*it];
      m_all_col_names[S_idx] = oss.str();
    }

    // Check if S \subset T
    FeatureMask S_mask = setToBitmask(S);
    bool is_subset = false;
    for (const auto &U_mask : U)
    {
      if (isSubset(S_mask, U_mask))
      {
        is_subset = true;
        break;
      }
    }
    if (!is_subset)
      continue;

    needToComputePDfunctionsFor.insert(S_idx);
  }

  // Compute expectation of all necessary subsets using bitmask implementation
  NumericMatrix mat = is_weak_inequality ? recurseMarginalizeSBitmask<glex::WeakComparison>(x, tree, U, 0, leaf_data) : recurseMarginalizeSBitmask<glex::StrictComparison>(x, tree, U, 0, leaf_data);

  unsigned int t_size = countSetBits(all_encountered);

  for (unsigned int S_idx : needToComputePDfunctionsFor)
  {
    std::set<unsigned int> S = to_explain[S_idx];
    contributeFastPDBitmask(mat, m_all, S, U, S_idx, t_size);
  }
  colnames(m_all) = m_all_col_names;

  return m_all;
}
