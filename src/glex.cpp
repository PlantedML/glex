#include <Rcpp.h>
#include <algorithm>

bool containsNumber(Rcpp::IntegerVector &vec, int target)
{
  // Use std::find to check if the target is in the vector
  return std::find(vec.begin(), vec.end(), target) != vec.end();
}

Rcpp::IntegerVector removeValue(Rcpp::IntegerVector &vec, int valueToRemove)
{
  // Use std::remove to move the value to the end
  vec.erase(std::remove(vec.begin(), vec.end(), valueToRemove), vec.end());

  return vec;
}

std::vector<std::set<unsigned int>> get_all_subsets_(std::set<unsigned int> &set)
{
  std::vector<std::set<unsigned int>> result;
  unsigned int n = set.size();
  for (unsigned int i = 0; i < (1 << n); ++i)
  {
    std::set<unsigned int> subset;
    auto it = set.begin();
    for (unsigned int j = 0; j < n; ++j)
    {
      if (i & (1 << j))
      {
        subset.insert(*it);
      }
      it++;
    }
    result.push_back(subset);
  }
  return result;
}

// [[Rcpp::export]]
double empProbFunction(Rcpp::NumericMatrix &x, Rcpp::IntegerVector &coords, Rcpp::NumericVector &lb, Rcpp::NumericVector &ub)
{
  int n = x.nrow();
  int m = coords.size();
  int count = 0;

  for (int i = 0; i < n; ++i)
  { // Loop over rows
    bool withinBounds = true;
    for (int j = 0; j < m; ++j)
    {                          // Loop over selected columns
      int col = coords[j] - 1; // Adjust for 0-based indexing in C++
      if (x(i, col) <= lb[j] || x(i, col) >= ub[j])
      {
        withinBounds = false;
        break; // Exit loop if any variable is out of bounds
      }
    }
    if (withinBounds)
      count++;
  }

  return count / static_cast<double>(n); // Calculate empirical probability
}

// [[Rcpp::export]]
Rcpp::NumericMatrix recurseRcppEmpProbfunction(Rcpp::NumericMatrix &x, Rcpp::IntegerVector &feature, Rcpp::NumericVector &split,
                                               Rcpp::IntegerVector &yes, Rcpp::IntegerVector &no, Rcpp::NumericVector &quality,
                                               Rcpp::NumericMatrix &lb, Rcpp::NumericMatrix &ub, Rcpp::IntegerVector &cover,
                                               std::vector<std::vector<unsigned int>> &U, unsigned int node)
{

  // Start with all 0
  unsigned int n = x.nrow();
  Rcpp::NumericMatrix mat(n, U.size());

  // If leaf, just return value
  if (feature[node] == 0)
  {
    for (unsigned int j = 0; j < U.size(); ++j)
    {
      double p;
      Rcpp::IntegerVector to_integrate = Rcpp::wrap(U[j]);
      for (unsigned int k = 0; k < to_integrate.size(); ++k)
      {
        if (!containsNumber(cover, to_integrate[k]))
          removeValue(to_integrate, to_integrate[k]);
      }
      if (to_integrate.size() == 0)
        p = 1;
      else
      {
        Rcpp::NumericVector leaf_lb = lb(node, Rcpp::_);
        Rcpp::NumericVector leaf_ub = ub(node, Rcpp::_);

        Rcpp::NumericVector to_integrate_lb(to_integrate.size());
        Rcpp::NumericVector to_integrate_ub(to_integrate.size());

        for (unsigned int k = 0; k < to_integrate.size(); ++k)
        {
          to_integrate_lb[k] = leaf_lb[to_integrate[k] - 1];
          to_integrate_ub[k] = leaf_ub[to_integrate[k] - 1];
        }

        p = empProbFunction(x, to_integrate, to_integrate_lb, to_integrate_ub);
      }
      Rcpp::NumericMatrix::Column to_fill = mat(Rcpp::_, j);
      std::fill(to_fill.begin(), to_fill.end(), quality[node] * p);
    }
  }
  else
  {
    // Call both children, they give a matrix each of all obs and subsets
    const int curr_feature = feature[node];
    Rcpp::IntegerVector this_values = Rcpp::clone(cover);
    if (!containsNumber(this_values, curr_feature))
      this_values.push_back(curr_feature);
    Rcpp::NumericMatrix mat_yes = recurseRcppEmpProbfunction(x, feature, split, yes, no, quality, lb, ub, this_values, U, yes[node]);
    Rcpp::NumericMatrix mat_no = recurseRcppEmpProbfunction(x, feature, split, yes, no, quality, lb, ub, this_values, U, no[node]);

    for (unsigned int j = 0; j < U.size(); ++j)
    {
      // Is splitting feature out in this subset?
      bool isout = false;
      for (unsigned int k = 0; k < U[j].size(); ++k)
      {
        if (U[j][k] == feature[node])
        {
          isout = true;
        }
      }

      if (isout)
      {
        // For subsets where feature is out, weighted average of left/right
        for (unsigned int i = 0; i < n; ++i)
        {
          mat(i, j) += mat_yes(i, j) + mat_no(i, j);
        }
      }
      else
      {
        // For subsets where feature is in, split to left/right
        for (unsigned int i = 0; i < n; ++i)
        {
          if (x(i, feature[node] - 1) <= split[node])
          {
            mat(i, j) += mat_yes(i, j);
          }
          else
          {
            mat(i, j) += mat_no(i, j);
          }
        }
      }
    }
  }

  // Return combined matrix
  return (mat);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix recurse(Rcpp::NumericMatrix &x, Rcpp::IntegerVector &feature, Rcpp::NumericVector &split,
                            Rcpp::IntegerVector &yes, Rcpp::IntegerVector &no, Rcpp::NumericVector &quality,
                            Rcpp::NumericMatrix &lb, Rcpp::NumericMatrix &ub, Rcpp::IntegerVector &cover,
                            std::vector<std::vector<unsigned int>> &U, unsigned int node, Rcpp::Function probFunction)
{

  // Start with all 0
  unsigned int n = x.nrow();
  Rcpp::NumericMatrix mat(n, U.size());

  // If leaf, just return value
  if (feature[node] == 0)
  {
    for (unsigned int j = 0; j < U.size(); ++j)
    {
      double p;
      Rcpp::IntegerVector to_integrate = Rcpp::wrap(U[j]);
      for (unsigned int k = 0; k < to_integrate.size(); ++k)
      {
        if (!containsNumber(cover, to_integrate[k]))
          removeValue(to_integrate, to_integrate[k]);
      }
      if (to_integrate.size() == 0)
        p = 1;
      else
      {
        Rcpp::NumericVector leaf_lb = lb(node, Rcpp::_);
        Rcpp::NumericVector leaf_ub = ub(node, Rcpp::_);

        Rcpp::NumericVector to_integrate_lb(to_integrate.size());
        Rcpp::NumericVector to_integrate_ub(to_integrate.size());

        for (unsigned int k = 0; k < to_integrate.size(); ++k)
        {
          to_integrate_lb[k] = leaf_lb[to_integrate[k] - 1];
          to_integrate_ub[k] = leaf_ub[to_integrate[k] - 1];
        }

        p = Rcpp::as<double>(probFunction(to_integrate, to_integrate_lb, to_integrate_ub));
      }
      Rcpp::NumericMatrix::Column to_fill = mat(Rcpp::_, j);
      std::fill(to_fill.begin(), to_fill.end(), quality[node] * p);
    }
  }
  else
  {
    // Call both children, they give a matrix each of all obs and subsets
    const int curr_feature = feature[node];
    Rcpp::IntegerVector this_values = Rcpp::clone(cover);
    if (!containsNumber(this_values, curr_feature))
      this_values.push_back(curr_feature);
    Rcpp::NumericMatrix mat_yes = recurse(x, feature, split, yes, no, quality, lb, ub, this_values, U, yes[node], probFunction);
    Rcpp::NumericMatrix mat_no = recurse(x, feature, split, yes, no, quality, lb, ub, this_values, U, no[node], probFunction);

    for (unsigned int j = 0; j < U.size(); ++j)
    {
      // Is splitting feature out in this subset?
      bool isout = false;
      for (unsigned int k = 0; k < U[j].size(); ++k)
      {
        if (U[j][k] == feature[node])
        {
          isout = true;
        }
      }

      if (isout)
      {
        // For subsets where feature is out, weighted average of left/right
        for (unsigned int i = 0; i < n; ++i)
        {
          mat(i, j) += mat_yes(i, j) + mat_no(i, j);
        }
      }
      else
      {
        // For subsets where feature is in, split to left/right
        for (unsigned int i = 0; i < n; ++i)
        {
          if (x(i, feature[node] - 1) <= split[node])
          {
            mat(i, j) += mat_yes(i, j);
          }
          else
          {
            mat(i, j) += mat_no(i, j);
          }
        }
      }
    }
  }

  // Return combined matrix
  return (mat);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix recurseAlgorithm2(Rcpp::NumericMatrix &x, Rcpp::IntegerVector &feature, Rcpp::NumericVector &split,
                                      Rcpp::IntegerVector &yes, Rcpp::IntegerVector &no, Rcpp::NumericVector &quality,
                                      Rcpp::NumericVector &cover, std::vector<std::vector<unsigned int>> &U, unsigned int node)
{

  // Start with all 0
  unsigned int n = x.nrow();
  Rcpp::NumericMatrix mat(n, U.size());

  // If leaf, just return value
  if (feature[node] == 0)
  {
    double pred = quality[node];
    std::fill(mat.begin(), mat.end(), pred);
  }
  else
  {
    // Call both children, they give a matrix each of all obs and subsets
    Rcpp::NumericMatrix mat_yes = recurseAlgorithm2(x, feature, split, yes, no, quality, cover, U, yes[node]);
    Rcpp::NumericMatrix mat_no = recurseAlgorithm2(x, feature, split, yes, no, quality, cover, U, no[node]);

    for (unsigned int j = 0; j < U.size(); ++j)
    {
      // Is splitting feature out in this subset?
      bool isout = false;
      for (unsigned int k = 0; k < U[j].size(); ++k)
      {
        if (U[j][k] == feature[node])
        {
          isout = true;
        }
      }

      if (isout)
      {
        // For subsets where feature is out, weighted average of left/right
        for (unsigned int i = 0; i < n; ++i)
        {
          mat(i, j) += cover[yes[node]] / cover[node] * mat_yes(i, j) + cover[no[node]] / cover[node] * mat_no(i, j);
        }
      }
      else
      {
        // For subsets where feature is in, split to left/right
        for (unsigned int i = 0; i < n; ++i)
        {
          if (x(i, feature[node] - 1) <= split[node])
          {
            mat(i, j) += mat_yes(i, j);
          }
          else
          {
            mat(i, j) += mat_no(i, j);
          }
        }
      }
    }
  }

  // Return combined matrix
  return (mat);
}

// S and T have to be sorted!
// [[Rcpp::export]]
void contribute(Rcpp::NumericMatrix &mat, Rcpp::NumericMatrix &m_all, Rcpp::IntegerVector &S, Rcpp::IntegerVector &T, std::vector<Rcpp::IntegerVector> &T_subsets, unsigned int colnum)
{

  Rcpp::IntegerVector sTS = Rcpp::setdiff(T, S);
  std::sort(sTS.begin(), sTS.end());

  for (unsigned int i = 0; i < T_subsets.size(); ++i)
  {
    Rcpp::IntegerVector U = T_subsets[i];
    bool contrib = true;
    if (!(sTS.size() == 0))
    {
      Rcpp::IntegerVector ssTSU = Rcpp::setdiff(sTS, U);
      if (!(ssTSU.size() == 0))
      {
        contrib = false;
      }
    }

    if (contrib)
    {
      Rcpp::IntegerVector sTU = Rcpp::setdiff(T, U);

      if (((S.size() - sTU.size()) % 2) == 0)
      {
        // positive sign
        m_all(Rcpp::_, colnum) = m_all(Rcpp::_, colnum) + mat(Rcpp::_, i);
      }
      else
      {
        // negative sign
        m_all(Rcpp::_, colnum) = m_all(Rcpp::_, colnum) - mat(Rcpp::_, i);
      }
    }
  }
}
