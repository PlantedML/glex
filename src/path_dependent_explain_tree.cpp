#include <Rcpp.h>
#include <algorithm>
#include "glex.h"
#include "comparison_policies.h"

using namespace Rcpp;

void contributeFastPDBitmask(
    NumericMatrix &mat,
    NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::vector<FeatureMask> &U,
    unsigned int colnum,
    unsigned int t_size);

namespace
{
  std::vector<unsigned int> calculate_data_driven_covers(
      const NumericMatrix &x,
      const NumericMatrix &tree,
      bool is_weak_inequality)
  {
    unsigned int n_nodes = tree.nrow();
    unsigned int n_samples = x.nrow();
    std::vector<unsigned int> covers(n_nodes, 0);

    if (n_samples == 0 || n_nodes == 0)
    {
      return covers;
    }

    for (unsigned int i = 0; i < n_samples; ++i)
    {
      // Cannot take x.row(i) directly if x is pass-by-reference-to-const in some Rcpp contexts
      // but NumericMatrix::ConstRow is fine. Let's assume x is accessible.
      // To be safe with potential non-const x, we can copy or access elements.
      // For now, direct access pattern as in original recursePathDependent implies x is readable.

      unsigned int current_node_idx = 0; // Start at root

      while (current_node_idx < n_nodes)
      { // Ensure current_node_idx is valid
        covers[current_node_idx]++;
        NumericMatrix::ConstRow node_data = tree.row(current_node_idx);
        const int feature_idx = node_data[Index::FEATURE];

        if (feature_idx == -1)
        { // Leaf node
          break;
        }

        const double split_val = node_data[Index::SPLIT];
        // Ensure feature_idx is valid for x.ncol()
        if (static_cast<unsigned int>(feature_idx) >= x.ncol() || feature_idx < 0)
        {
          // This would be an error: tree uses a feature not in x
          // Or handle more gracefully, e.g., Rcpp::stop or break path.
          // For now, assume valid feature_idx as per original code's direct use.
          break;
        }
        const double obs_val = x(i, feature_idx);

        bool goes_yes;
        if (is_weak_inequality)
        {
          goes_yes = glex::WeakComparison::compare(obs_val, split_val);
        }
        else
        {
          goes_yes = glex::StrictComparison::compare(obs_val, split_val);
        }

        if (goes_yes)
        {
          current_node_idx = node_data[Index::YES];
        }
        else
        {
          current_node_idx = node_data[Index::NO];
        }
        // Basic check for child validity, though tree structure should be correct
        if (current_node_idx >= n_nodes && feature_idx != -1)
        {
          // Invalid child index from tree, stop this path.
          // This indicates a malformed tree.
          break;
        }
      }
    }
    return covers;
  }
} // anonymous namespace

template <typename Comparison>
NumericMatrix recursePathDependent(
    NumericMatrix &x,
    const NumericMatrix &tree,
    std::vector<FeatureMask> &U,
    unsigned int node,
    const std::vector<unsigned int> &node_covers) // Added node_covers
{

  NumericMatrix::ConstRow current_node = tree(node, _);
  const int current_feature = current_node[Index::FEATURE];

  const unsigned int n = x.nrow();
  const unsigned int n_subsets = U.size();
  NumericMatrix mat(no_init(n, n_subsets));

  if (current_feature == -1)
  {
    double pred = current_node[Index::QUALITY];
    std::fill(mat.begin(), mat.end(), pred);
  }
  else
  {
    const unsigned int yes = current_node[Index::YES];
    const unsigned int no = current_node[Index::NO];
    const double split = current_node[Index::SPLIT];

    NumericMatrix mat_yes = recursePathDependent<Comparison>(x, tree, U, yes, node_covers);
    NumericMatrix mat_no = recursePathDependent<Comparison>(x, tree, U, no, node_covers);

    for (unsigned int j = 0; j < U.size(); ++j)
    {
      const double *col_yes = &mat_yes(0, j);
      const double *col_no = &mat_no(0, j);
      double *col_out = &mat(0, j);

      if (!hasFeature(U[j], current_feature))
      {
        double cover_at_current_node = static_cast<double>(node_covers[node]);
        double weight_yes = 0.5;
        double weight_no = 0.5;

        if (cover_at_current_node > 0)
        {
          // Ensure child indices 'yes' and 'no' are valid for node_covers
          // This should hold if tree structure and node_covers calculation are correct
          double cover_at_yes_child = (yes < node_covers.size()) ? static_cast<double>(node_covers[yes]) : 0.0;
          double cover_at_no_child = (no < node_covers.size()) ? static_cast<double>(node_covers[no]) : 0.0;

          weight_yes = cover_at_yes_child / cover_at_current_node;
          weight_no = cover_at_no_child / cover_at_current_node;

          // Small correction if sum is not exactly 1 due to all samples going one way and 0 the other
          // for a child that might not be further split (leaf).
          // The covers should reflect actual flow. If node_covers[yes] + node_covers[no] != node_covers[node]
          // it means some samples terminated or tree is structured unexpectedly (e.g. missing values not handled here)
          // For now, simple division is fine.
        }
        // If cover_at_current_node is 0, weights remain 0.5/0.5.
        // This means if no samples from x reach this node, we average predictions from children.

        for (unsigned int i = 0; i < n; ++i)
        {
          col_out[i] = weight_yes * col_yes[i] + weight_no * col_no[i];
        }
      }
      else
      {
        for (unsigned int i = 0; i < n; ++i)
        {
          if (Comparison::compare(x(i, current_feature), split))
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
  }
  return (mat);
}

// [[Rcpp::export]]
NumericMatrix explainTreePathDependent(
    NumericMatrix &x,
    const NumericMatrix &tree,
    List &to_explain_list,
    unsigned int max_interaction,
    bool is_weak_inequality)
{
  unsigned int n_features = x.ncol();
  unsigned int n_nodes = tree.nrow();

  FeatureMask all_encountered = createEmptyMask(n_features);
  for (unsigned int i = 0; i < n_nodes; ++i)
  {
    NumericMatrix::ConstRow current_node_row = tree(i, _);    // Renamed to avoid conflict
    const int feature_val = current_node_row[Index::FEATURE]; // Renamed
    if (feature_val != -1)
    {
      all_encountered = setFeature(all_encountered, feature_val);
    }
  }

  std::vector<FeatureMask> U = get_all_subsets_of_mask(all_encountered, max_interaction);

  // Calculate data-driven covers based on input x and tree structure
  std::vector<unsigned int> node_covers = calculate_data_driven_covers(x, tree, is_weak_inequality);

  NumericMatrix mat;
  if (is_weak_inequality)
  {
    mat = recursePathDependent<glex::WeakComparison>(x, tree, U, 0, node_covers);
  }
  else
  {
    mat = recursePathDependent<glex::StrictComparison>(x, tree, U, 0, node_covers);
  }

  std::vector<std::set<unsigned int>> to_explain;
  unsigned int to_explain_size = to_explain_list.size();
  NumericMatrix m_all = NumericMatrix(x.nrow(), to_explain_size);
  CharacterVector m_all_col_names = CharacterVector(to_explain_size);
  CharacterVector x_col_names = colnames(x);

  std::set<unsigned int> needToComputePDfunctionsFor;
  for (int S_idx = 0; S_idx < to_explain_size; S_idx++)
  {
    std::set<unsigned int> S = std::set<unsigned int>(
        as<IntegerVector>(to_explain_list[S_idx]).begin(),
        as<IntegerVector>(to_explain_list[S_idx]).end());
    to_explain.push_back(S);

    int k = S.size();
    if (k != 0)
    {
      auto it = S.begin();
      std::ostringstream oss;
      for (int i = 0; i < k - 1; i++, it++)
        oss << x_col_names[*it] << ":";
      oss << x_col_names[*it];
      m_all_col_names[S_idx] = oss.str();
    }

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

  unsigned int t_size = countSetBits(all_encountered);

  for (int S_idx : needToComputePDfunctionsFor)
  {
    std::set<unsigned int> S = to_explain[S_idx];
    contributeFastPDBitmask(mat, m_all, S, U, S_idx, t_size);
  }
  colnames(m_all) = m_all_col_names;

  return m_all;
}
