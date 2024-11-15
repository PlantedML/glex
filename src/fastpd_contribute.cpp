#include <Rcpp.h>
#include "../inst/include/glex.h"

using namespace Rcpp;
std::vector<std::set<unsigned int>> get_all_subsets_(std::set<unsigned int> &set);

void contributeFastPD2(
    NumericMatrix &mat,
    NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::vector<std::set<unsigned int>> &T_subsets,
    unsigned int colnum)
{
  std::vector<std::set<unsigned int>> Vs = get_all_subsets_(S);
  for (unsigned int i = 0; i < Vs.size(); ++i)
  {
    std::set<unsigned int> V = Vs[i];
    auto it = std::find(T_subsets.begin(), T_subsets.end(), V);
    unsigned int idx = std::distance(T_subsets.begin(), it);

    if ((S.size() - V.size()) % 2 == 0)
    {
      m_all(_, colnum) = m_all(_, colnum) + mat(_, idx);
    }
    else
    {
      m_all(_, colnum) = m_all(_, colnum) - mat(_, idx);
    }
  }
}

void contributeFastPD(
    Rcpp::NumericMatrix &mat,
    Rcpp::NumericMatrix &m_all,
    std::set<unsigned int> &S,
    std::set<unsigned int> &T,
    std::vector<std::set<unsigned int>> &T_subsets,
    unsigned int colnum)
{
  std::set<unsigned int> sTS;
  std::set_difference(T.begin(), T.end(), S.begin(), S.end(), std::inserter(sTS, sTS.begin()));

  for (unsigned int i = 0; i < T_subsets.size(); ++i)
  {
    std::set<unsigned int> U = T_subsets[i];
    if (sTS.size() != 0)
    {
      std::set<unsigned int> ssTSU;
      std::set_difference(sTS.begin(), sTS.end(), U.begin(), U.end(), std::inserter(ssTSU, ssTSU.begin()));
      if (ssTSU.size() != 0)
        continue;
    }

    std::set<unsigned int> sTU;
    std::set_difference(T.begin(), T.end(), U.begin(), U.end(), std::inserter(sTU, sTU.begin()));

    if (((S.size() - sTU.size()) % 2) == 0)
      m_all(Rcpp::_, colnum) = m_all(Rcpp::_, colnum) + mat(Rcpp::_, i);
    else
      m_all(Rcpp::_, colnum) = m_all(Rcpp::_, colnum) - mat(Rcpp::_, i);
  }
}
