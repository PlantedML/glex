#pragma once

namespace glex
{

  inline float as_float(double x)
  {
    return static_cast<float>(x);
  }

  // Comparison policy classes
  struct StrictComparison
  {
    template <typename T>
    static bool compare(const T &a, const T &b)
    {
      // xgboost prediction uses float split comparisons, so mirror that precision.
      return as_float(static_cast<double>(a)) < as_float(static_cast<double>(b));
    }
  };

  struct WeakComparison
  {
    template <typename T>
    static bool compare(const T &a, const T &b)
    {
      // Keep weak comparator in the same numeric precision for consistency.
      return as_float(static_cast<double>(a)) <= as_float(static_cast<double>(b));
    }
  };

} // namespace glex
