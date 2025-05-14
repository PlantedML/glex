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

template <typename Comparison>
NumericMatrix recursePathDependent(NumericMatrix &x, const NumericMatrix &tree, std::vector<FeatureMask> &U, unsigned int node)
{

  NumericMatrix::ConstRow current_node = tree(node, _);
  const int current_feature = current_node[Index::FEATURE];

  // Dimensions
  const unsigned int n = x.nrow();
  const unsigned int n_subsets = U.size();

  // We'll create an output matrix [n, n_subsets].
  // Using no_init to skip zero fill if we plan to overwrite everything.
  NumericMatrix mat(no_init(n, n_subsets));

  // If leaf, just return value
  if (current_feature == -1)
  {
    double pred = current_node[Index::QUALITY];
    std::fill(mat.begin(), mat.end(), pred);
  }
  else
  {
    // Call both children, they give a matrix each of all obs and subsets
    const unsigned int yes = current_node[Index::YES];
    const unsigned int no = current_node[Index::NO];
    const double split = current_node[Index::SPLIT];
    const double cover_yes = tree(yes, Index::COVER);
    const double cover_no = tree(no, Index::COVER);
    const double cover_node = current_node[Index::COVER];

    NumericMatrix mat_yes = recursePathDependent<Comparison>(x, tree, U, yes);
    NumericMatrix mat_no = recursePathDependent<Comparison>(x, tree, U, no);

    for (unsigned int j = 0; j < U.size(); ++j)
    {
      const double *col_yes = &mat_yes(0, j);
      const double *col_no = &mat_no(0, j);
      double *col_out = &mat(0, j);

      // Use bitmask to check if feature is out (not in subset U[j])
      if (!hasFeature(U[j], current_feature))
      {
        for (unsigned int i = 0; i < n; ++i)
        {
          col_out[i] = cover_yes / cover_node * col_yes[i] + cover_no / cover_node * col_no[i];
        }
      }
      else
      {
        // For subsets where feature is in, split to left/right
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

  // Return combined matrix
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
  // Get feature count for proper mask sizing
  unsigned int n_features = x.ncol();
  unsigned int n_nodes = tree.nrow();

  // Determine all features encountered in the tree nodes
  FeatureMask all_encountered = createEmptyMask(n_features);
  for (unsigned int i = 0; i < n_nodes; ++i)
  {
    NumericMatrix::ConstRow current_node = tree(i, _);
    const int current_feature = current_node[Index::FEATURE];
    if (current_feature != -1)
    {                                                                 // -1 indicates a leaf node
      all_encountered = setFeature(all_encountered, current_feature); // Adjust to 0-based
    }
  }

  // Generate all subsets U of all_encountered features up to max_interaction
  std::vector<FeatureMask> U = get_all_subsets_of_mask(all_encountered, max_interaction);

  // Explain/expectation/marginalization step using the optimized recursePathDependent
  NumericMatrix mat = is_weak_inequality ? recursePathDependent<glex::WeakComparison>(x, tree, U, 0) : recursePathDependent<glex::StrictComparison>(x, tree, U, 0);

  // Process to_explain_list and calculate m_all
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

    // Calculate contribution for S using inclusion-exclusion
    // Iterate over all subsets V of S (using bitmask logic)
    FeatureMask S_mask = setToBitmask(S); // S_set is 0-based
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

  uint t_size = countSetBits(all_encountered);

  for (int S_idx : needToComputePDfunctionsFor)
  {
    std::set<unsigned int> S = to_explain[S_idx];
    contributeFastPDBitmask(mat, m_all, S, U, S_idx, t_size);
  }
  colnames(m_all) = m_all_col_names;

  return m_all;
}
