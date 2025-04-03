#pragma once

namespace glex
{

  // Comparison policy classes
  struct StrictComparison
  {
    template <typename T>
    static bool compare(const T &a, const T &b)
    {
      return a < b;
    }
  };

  struct WeakComparison
  {
    template <typename T>
    static bool compare(const T &a, const T &b)
    {
      return a <= b;
    }
  };

} // namespace glex
