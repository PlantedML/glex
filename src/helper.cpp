#include <Rcpp.h>

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

void recurse_subset(
    const std::set<unsigned int> &arr,
    std::set<unsigned int> &currentSubset,
    std::vector<std::set<unsigned int>> &subsets,
    int maxSize,
    int index)
{
  // Add the current subset to the result if it's within the maxSize limit
  if (currentSubset.size() <= maxSize)
  {
    subsets.push_back(currentSubset);
  }

  // Stop recursion if maxSize is reached
  if (currentSubset.size() == maxSize || index >= arr.size())
    return;

  // Recursively generate subsets starting from the current index
  auto it = std::next(arr.begin(), index);
  do
  {
    currentSubset.insert(*it);
    recurse_subset(arr, currentSubset, subsets, maxSize, ++index);
    currentSubset.erase(*it);
    ++it;
  } while (index < arr.size());
}

std::vector<std::set<unsigned int>> get_all_subsets(
    std::set<unsigned int> &arr,
    unsigned int maxSize = UINT_MAX)
{
  std::vector<std::set<unsigned int>> subsets = {};
  std::set<unsigned int> currentSubset = {};
  recurse_subset(arr, currentSubset, subsets, maxSize, 0);
  return subsets;
}

// [[Rcpp::export]]
Rcpp::List get_all_subsets_cpp(Rcpp::IntegerVector x, unsigned int maxSize)
{
  std::set<unsigned int> arr(x.begin(), x.end());
  std::vector<std::set<unsigned int>> subsets = get_all_subsets(arr, maxSize);
  Rcpp::List result(subsets.size());
  for (unsigned int i = 0; i < subsets.size(); ++i)
  {
    result[i] = Rcpp::IntegerVector(subsets[i].begin(), subsets[i].end());
  }
  return result;
}
