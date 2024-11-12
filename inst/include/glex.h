
#ifndef GLEX_H_
#define GLEX_H_

#include <set>
#include <map>
#include <vector>

typedef std::map<std::set<unsigned int>, double> ProbsMap;
typedef std::map<std::set<unsigned int>, std::vector<unsigned int>> PathData;

struct AugmentedData
{
  std::set<unsigned int> encountered;
  PathData pathData;
};

struct LeafData
{
  std::set<unsigned int> all_encountered;
  std::map<unsigned int, std::set<unsigned int>> encountered;
  std::map<unsigned int, ProbsMap> leafProbs;
};

enum Index
{
  FEATURE = 0,
  SPLIT = 1,
  YES = 2,
  NO = 3,
  QUALITY = 4,
};

#endif // GLEX_H_
