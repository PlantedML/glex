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
