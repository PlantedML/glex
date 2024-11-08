
#include <set>
#include <map>


typedef std::map<std::set<unsigned int>, double> ProbsMap;
typedef std::map<std::set<unsigned int>, std::vector<unsigned int>> PathData;

struct AugmentedData {
    std::set<unsigned int> encountered;
    PathData pathData;
};

struct LeafData {
    std::map<unsigned int, std::set<unsigned int>> encountered;
    std::map<unsigned int, ProbsMap> leafProbs;
};