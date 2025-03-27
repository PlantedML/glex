#include <Rcpp.h>
#include "../inst/include/glex.h"
#include "comparison_policies.h"

using namespace Rcpp;

// Bitmask data structures and utilities
typedef std::unordered_map<uint64_t, std::vector<unsigned int>> PathDataBitmask;

struct AugmentedDataBitMask
{
  uint64_t encountered; // 64-bit mask for encountered features
  PathDataBitmask pathData;
};

using FeatureMask = uint64_t; // up to 64 features

struct LeafDataBitMask
{
  std::vector<FeatureMask> encounteredMask; // for each node
  std::vector<std::unordered_map<FeatureMask, double>> leafProbs;
  FeatureMask allEncounteredMask = 0ULL; // union of all encountered features
};

// Bitmask utility functions
inline bool hasFeature(FeatureMask mask, unsigned int feature)
{
  return (mask & (1ULL << feature)) != 0ULL;
}

inline FeatureMask setFeature(FeatureMask mask, unsigned int feature)
{
  return mask | (1ULL << feature);
}

inline FeatureMask clearFeature(FeatureMask mask, unsigned int feature)
{
  return mask & ~(1ULL << feature);
}

inline FeatureMask bitmaskDifference(FeatureMask big, FeatureMask small)
{
  return big & ~small;
}

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
      uint64_t subsetMask = kv.first;
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
  leaf_data.allEncounteredMask |= (1ULL << current_feature);

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
    const uint64_t subsetMask = kv.first;
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
        if (val < split)
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
    passed_down_yes.encountered |= (1ULL << current_feature);
    passed_down_no.encountered |= (1ULL << current_feature);

    // Duplicate pathData with the new bit set
    // for each subset in the *original* pathData
    for (auto &kv : passed_down.pathData)
    {
      uint64_t old_subset = kv.first;
      const std::vector<unsigned int> &row_inds = kv.second;

      uint64_t new_subset = old_subset | (1ULL << current_feature);

      passed_down_yes.pathData[new_subset] = row_inds;
      passed_down_no.pathData[new_subset] = row_inds;
    }
  }

  // Recurse
  augmentTreeRecurseStepBitmask(passed_down_yes, leaf_data, tree, dataset, yes_idx);
  augmentTreeRecurseStepBitmask(passed_down_no, leaf_data, tree, dataset, no_idx);
}

LeafDataBitMask augmentTreeBitmask(
    Rcpp::NumericMatrix &tree,
    Rcpp::NumericMatrix &x)
{
  int n_nodes = tree.nrow();

  // Prepare LeafDataBitMask
  LeafDataBitMask leaf_data;
  leaf_data.encounteredMask.resize(n_nodes, 0ULL);
  leaf_data.leafProbs.resize(n_nodes);

  // Build the root "passed_down" data
  AugmentedDataBitMask root;
  root.encountered = 0ULL; // no features encountered at root
  // The entire dataset is active under the "subsetMask=0" (empty subset)
  {
    std::vector<unsigned int> all_rows;
    all_rows.reserve(x.nrow());
    for (int i = 0; i < x.nrow(); i++)
    {
      all_rows.push_back(i);
    }
    // pathData with key=0 (empty subset), val=all rows
    root.pathData[0ULL] = std::move(all_rows);
  }

  // Recurse from node=0 (root)
  augmentTreeRecurseStepBitmask(root, leaf_data, tree, x, /*node=*/0);

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
      FeatureMask intersection = leaf_data.encounteredMask[node] & U[j];

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

  // For each subset in U, check if it "contains" current_feature
  // If it does, we sum children; if not, we split rows by threshold.
  std::vector<bool> subsetHasFeature(n_subsets);
  for (unsigned int j = 0; j < n_subsets; ++j)
  {
    subsetHasFeature[j] = hasFeature(U[j], current_feature);
  }

  // Fill output
  for (unsigned int j = 0; j < n_subsets; ++j)
  {
    const double *col_yes = &mat_yes(0, j);
    const double *col_no = &mat_no(0, j);
    double *col_out = &mat(0, j);

    if (!subsetHasFeature[j])
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

// Helper function to convert set to bitmask
FeatureMask setToBitmask(const std::set<unsigned int> &set)
{
  FeatureMask mask = 0ULL;
  for (unsigned int feature : set)
  {
    mask |= (1ULL << feature);
  }
  return mask;
}

// Helper function to convert bitmask to set
std::set<unsigned int> bitmaskToSet(FeatureMask mask)
{
  std::set<unsigned int> set;
  for (unsigned int i = 0; i < 64; i++)
  {
    if (mask & (1ULL << i))
    {
      set.insert(i);
    }
  }
  return set;
}

std::vector<uint64_t> get_all_subsets_of_mask(uint64_t mask, unsigned int max_size = UINT_MAX)
{
  // 1) Gather indices of set bits.
  std::vector<unsigned int> bits;
  bits.reserve(64);
  for (unsigned int i = 0; i < 64; i++)
  {
    if (mask & (1ULL << i))
    {
      bits.push_back(i);
    }
  }
  const size_t k = bits.size();
  const size_t n_subsets = (1ULL << k);

  // 2) Build all subsets
  std::vector<uint64_t> subsets;
  subsets.reserve(n_subsets);
  for (size_t subset_idx = 0; subset_idx < n_subsets; ++subset_idx)
  {
    // Count bits in subset_idx to check size
    if (__builtin_popcountll(subset_idx) > max_size)
      continue; // Skip subsets larger than max_size

    uint64_t subset_mask = 0ULL;
    for (size_t b = 0; b < k; ++b)
    {
      if (subset_idx & (1ULL << b))
      {
        subset_mask |= (1ULL << bits[b]);
      }
    }
    subsets.push_back(subset_mask);
  }
  return subsets;
}

void contributeFastPDBitmask(
    NumericMatrix &mat,
    NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::vector<FeatureMask> &U,
    unsigned int colnum,
    unsigned int t_size)
{
  uint64_t S_bitmask = setToBitmask(S);
  std::vector<uint64_t> Vs = get_all_subsets_of_mask(S_bitmask);
  for (unsigned int i = 0; i < Vs.size(); ++i)
  {
    FeatureMask V = Vs[i];
    auto it = std::find(U.begin(), U.end(), V);
    unsigned int idx = std::distance(U.begin(), it);
    if ((S.size() - __builtin_popcountll(V)) % 2 == 0)
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
  // Augment step using bitmask implementation
  LeafDataBitMask leaf_data = augmentTreeBitmask(tree, x_background);
  // Convert all_encountered to bitmask subsets
  FeatureMask all_encountered = leaf_data.allEncounteredMask;
  std::vector<FeatureMask> U = get_all_subsets_of_mask(all_encountered, max_interaction);

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
    FeatureMask S_mask = setToBitmask(S);
    bool is_subset = false;
    for (const auto &U_mask : U)
    {
      if ((U_mask & S_mask) == S_mask)
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
  uint t_size = __builtin_popcountll(all_encountered);
  for (int S_idx : needToComputePDfunctionsFor)
  {
    std::set<unsigned int> S = to_explain[S_idx];
    contributeFastPDBitmask(mat, m_all, S, U, S_idx, t_size);
  }
  colnames(m_all) = m_all_col_names;

  return m_all;
}
