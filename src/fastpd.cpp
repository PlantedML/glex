#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"

using namespace Rcpp;

void augmentTreeRecurseStep(AugmentedData passed_down, LeafData &leaf_data, NumericMatrix &tree, NumericMatrix &dataset, unsigned int node)
{
    NumericMatrix::Row current_node = tree(node, _);

    int current_feature = current_node[7];
    double split = current_node[2];

    if (current_feature == -1)
    {
        // Leaf node
        leaf_data.encountered[node] = passed_down.encountered;
        leaf_data.leafProbs[node] = ProbsMap();
        for (const auto &[subset, path_dat] : passed_down.pathData)
        {
            leaf_data.leafProbs[node][subset] = (double)path_dat.size() / dataset.nrow();
        }
        return;
    }
    else if (leaf_data.all_encountered.find(current_feature) == leaf_data.all_encountered.end())
    {
        leaf_data.all_encountered.insert(current_feature);
    }

    AugmentedData passed_down_yes = {
        .encountered = passed_down.encountered,
        .pathData = PathData(),
    };
    AugmentedData passed_down_no = {
        .encountered = passed_down.encountered,
        .pathData = PathData(),
    };

    for (const auto &[subset, path_dat] : passed_down.pathData)
    {
        if (subset.find(current_feature) != subset.end())
        {
            // Feature is in the path
            passed_down_yes.pathData[subset] = path_dat;
            passed_down_no.pathData[subset] = path_dat;
        }
        else
        {
            // Feature is not in the path
            passed_down_yes.pathData[subset] = std::vector<unsigned int>();
            passed_down_no.pathData[subset] = std::vector<unsigned int>();
            for (int i : path_dat)
            {
                if (dataset(i, current_feature) < split)
                {
                    passed_down_yes.pathData[subset].push_back(i);
                }
                else
                {
                    passed_down_no.pathData[subset].push_back(i);
                }
            }
        }
    }

    if (passed_down.encountered.find(current_feature) == passed_down.encountered.end())
    {
        passed_down_yes.encountered.insert(current_feature);
        passed_down_no.encountered.insert(current_feature);

        for (const auto &[subset, path_dat] : passed_down.pathData)
        {
            std::set to_add = subset;
            to_add.insert(current_feature);

            passed_down_yes.pathData[to_add] = path_dat;
            passed_down_no.pathData[to_add] = path_dat;
        }
    }

    unsigned int yes = current_node[3];
    unsigned int no = current_node[4];

    augmentTreeRecurseStep(passed_down_yes, leaf_data, tree, dataset, yes);
    augmentTreeRecurseStep(passed_down_no, leaf_data, tree, dataset, no);
}

LeafData augmentTree_(NumericMatrix &tree, NumericMatrix &dataset)
{
    AugmentedData result;

    AugmentedData to_pass_down = {
        .encountered = std::set<unsigned int>(),
        .pathData = PathData(),
    };
    to_pass_down.pathData[{}] = std::vector<unsigned int>(dataset.nrow());
    std::iota(to_pass_down.pathData[{}].begin(), to_pass_down.pathData[{}].end(), 0);
    LeafData leaf_data = LeafData();

    augmentTreeRecurseStep(to_pass_down, leaf_data, tree, dataset, 0);
    return leaf_data;
}

double augmentExpectation_(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data)
{

    std::stack<unsigned int> to_process;
    to_process.push(0);

    double result = 0;
    std::set<unsigned int> to_explain_set(to_explain.begin(), to_explain.end());

    while (!to_process.empty())
    {
        int node_idx = to_process.top();
        to_process.pop();

        NumericMatrix::Row current_node = tree(node_idx, _);
        int current_feature = current_node[7];
        double split = current_node[2];

        if (current_feature == -1)
        {
            std::set<unsigned int> to_marginalize = {};
            if (!to_explain_set.empty())
                std::set_intersection(
                    to_explain_set.begin(), to_explain_set.end(),
                    leaf_data.encountered[node_idx].begin(), leaf_data.encountered[node_idx].end(),
                    std::inserter(to_marginalize, to_marginalize.begin()));

            double p = leaf_data.leafProbs[node_idx][to_marginalize];
            result += tree(node_idx, 5) * p;
            continue;
        }

        if (std::find(to_explain.begin(), to_explain.end(), current_feature) != to_explain.end())
        {
            if (x[current_feature] < split)
            {
                to_process.push(current_node[3]);
            }
            else
            {
                to_process.push(current_node[4]);
            }
        }
        else
        {
            to_process.push(current_node[3]);
            to_process.push(current_node[4]);
        }
    }

    return result;
}

// [[Rcpp::export]]
double augmentAndTakeExpectation(NumericVector &x, NumericMatrix &dataset, NumericMatrix &tree, NumericVector &to_explain)
{
    LeafData leaf_data = augmentTree_(tree, dataset);
    return augmentExpectation_(x, tree, to_explain, leaf_data);
}

// [[Rcpp::export]]
XPtr<LeafData> augmentTree(NumericMatrix &tree, NumericMatrix &dataset)
{
    LeafData *leaf_data = new LeafData(augmentTree_(tree, dataset)); // Dynamically allocate
    XPtr<LeafData> ptr(leaf_data, true);                             // true enables automatic memory management
    return ptr;
}

// [[Rcpp::export]]
double augmentExpectation(NumericVector &x, NumericMatrix &tree, NumericVector &to_explain, SEXP leaf_data_ptr)
{
    const Rcpp::XPtr<LeafData> leaf_data(leaf_data_ptr);
    return augmentExpectation_(x, tree, to_explain, *leaf_data);
}

bool containsNumber(Rcpp::IntegerVector &vec, int target);
std::vector<std::set<unsigned int>> get_all_subsets_(std::vector<unsigned int> &set)
{
    std::vector<std::set<unsigned int>> result;
    unsigned int n = set.size();
    for (unsigned int i = 0; i < (1 << n); ++i)
    {
        std::set<unsigned int> subset;
        for (unsigned int j = 0; j < n; ++j)
        {
            if (i & (1 << j))
            {
                subset.insert(set[j]);
            }
        }
        result.push_back(subset);
    }
    return result;
}

Rcpp::NumericMatrix recurseMarginalizeU_(
    Rcpp::NumericMatrix &x, NumericMatrix &tree,
    std::vector<std::set<unsigned int>> &U, unsigned int node,
    LeafData &leaf_data)
{
    NumericMatrix::Row current_node = tree(node, _);
    int current_feature = current_node[7];

    // Start with all 0
    unsigned int n = x.nrow();
    Rcpp::NumericMatrix mat(n, U.size());

    // If leaf, just return value
    if (current_feature == -1)
    {
        for (unsigned int j = 0; j < U.size(); ++j)
        {
            std::set<unsigned int> to_explain = {};
            std::set_difference(
                leaf_data.encountered[node].begin(), leaf_data.encountered[node].end(),
                U[j].begin(), U[j].end(),
                std::inserter(to_explain, to_explain.begin()));

            double p = leaf_data.leafProbs[node][to_explain];
            double quality = tree(node, 5);
            double expected_value = quality * p;

            Rcpp::NumericMatrix::Column to_fill = mat(Rcpp::_, j);
            std::fill(to_fill.begin(), to_fill.end(), expected_value);
        }
    }
    else
    {
        unsigned int yes = current_node[3];
        unsigned int no = current_node[4];

        // Call both children, they give a matrix each of all obs and subsets
        Rcpp::NumericMatrix mat_yes = recurseMarginalizeU_(x, tree, U, yes, leaf_data);
        Rcpp::NumericMatrix mat_no = recurseMarginalizeU_(x, tree, U, no, leaf_data);

        for (unsigned int j = 0; j < U.size(); ++j)
        {
            // Is splitting feature out in this subset?

            if (U[j].find(current_feature) != U[j].end())
            {
                // For subsets where feature is out, weighted average of left/right
                for (unsigned int i = 0; i < n; ++i)
                    mat(i, j) += mat_yes(i, j) + mat_no(i, j);
            }
            else
            {
                double split = current_node[2];
                // For subsets where feature is in, split to left/right
                for (unsigned int i = 0; i < n; ++i)
                {
                    mat(i, j) += (x(i, current_feature) < split) ? mat_yes(i, j) : mat_no(i, j);
                }
            }
        }
    }

    // Return combined matrix
    return mat;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix marginalizeAllSplittedSubsetsinTree(
    Rcpp::NumericMatrix &x,
    NumericMatrix &tree)
{
    LeafData leaf_data = augmentTree_(tree, x);
    std::vector<unsigned int> all_encountered(leaf_data.all_encountered.begin(), leaf_data.all_encountered.end());
    std::vector<std::set<unsigned int>> U = get_all_subsets_(all_encountered);
    return recurseMarginalizeU_(x, tree, U, 0, leaf_data);
}