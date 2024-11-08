#include <Rcpp.h>
#include <algorithm>
#include <stack>
#include "../inst/include/glex.h"


using namespace Rcpp;


void augmentTreeRecurseStep(AugmentedData passed_down, LeafData &leaf_data, NumericMatrix &tree, NumericMatrix &dataset, unsigned int node) {
    NumericMatrix::Row current_node = tree(node, _);

    int current_feature = current_node[7];
    double split = current_node[2];

    if (current_feature == -1) {
        // Leaf node
        leaf_data.encountered[node] = passed_down.encountered;
        leaf_data.leafProbs[node] = ProbsMap();
        for (const auto& [subset, path_dat]: passed_down.pathData) {
            leaf_data.leafProbs[node][subset] = (double) path_dat.size() / dataset.nrow();
        }
        return;
    }

    AugmentedData passed_down_yes = {
        .encountered = passed_down.encountered,
        .pathData = PathData(),
    };
    AugmentedData passed_down_no = {
        .encountered = passed_down.encountered,
        .pathData = PathData(),
    };


    for (const auto& [subset, path_dat]: passed_down.pathData) {
        if (subset.find(current_feature) != subset.end()) {
            // Feature is in the path
            passed_down_yes.pathData[subset] = path_dat;
            passed_down_no.pathData[subset] = path_dat;
        } else {
            // Feature is not in the path
            passed_down_yes.pathData[subset] = std::vector<unsigned int>();
            passed_down_no.pathData[subset] = std::vector<unsigned int>();
            for (int i : path_dat) {
                if (dataset(i, current_feature) < split) {
                    passed_down_yes.pathData[subset].push_back(i);
                } else {
                    passed_down_no.pathData[subset].push_back(i);
                }
            }
        }
    }

    if (passed_down.encountered.find(current_feature) == passed_down.encountered.end()) {
        passed_down_yes.encountered.insert(current_feature);
        passed_down_no.encountered.insert(current_feature);

        for (const auto& [subset, path_dat]: passed_down.pathData) {
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

LeafData _augmentTree(NumericMatrix &tree, NumericMatrix &dataset) {
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

double _augmentExpectation(NumericVector &x, NumericMatrix &dataset, NumericMatrix &tree, NumericVector &to_explain, LeafData &leaf_data) {

    std::stack<unsigned int> to_process;
    to_process.push(0);

    double result = 0;
    std::set<unsigned int> to_explain_set(to_explain.begin(), to_explain.end());

    while (!to_process.empty()) {
        int node_idx = to_process.top();
        to_process.pop();

        NumericMatrix::Row current_node = tree(node_idx, _);
        int current_feature = current_node[7];
        double split = current_node[2];

        if (current_feature == -1) {
            std::set<unsigned int> to_marginalize = {};
            if (!to_explain_set.empty())
                std::set_intersection(
                    to_explain_set.begin(), to_explain_set.end(), 
                    leaf_data.encountered[node_idx].begin(), leaf_data.encountered[node_idx].end(),
                    std::inserter(to_marginalize, to_marginalize.begin())
                );

            double p = leaf_data.leafProbs[node_idx][to_marginalize];
            result += tree(node_idx, 5) * p;
            continue;
        }


        if (std::find(to_explain.begin(), to_explain.end(), current_feature) != to_explain.end()) {
            if (x[current_feature] < split) {
                to_process.push(current_node[3]);
            } else {
                to_process.push(current_node[4]);
            }
        } else {
            to_process.push(current_node[3]);
            to_process.push(current_node[4]);
        }
    }

    return result;
}


// [[Rcpp::export]]
double augmentAndTakeExpectation(NumericVector &x, NumericMatrix &dataset, NumericMatrix &tree, NumericVector &to_explain) {
    LeafData leaf_data = _augmentTree(tree, dataset);
    return _augmentExpectation(x, dataset, tree, to_explain, leaf_data);
}



// [[Rcpp::export]]
XPtr<LeafData> augmentTree(NumericMatrix &tree, NumericMatrix &dataset) {
    LeafData* leaf_data = new LeafData(_augmentTree(tree, dataset));  // Dynamically allocate
    XPtr<LeafData> ptr(leaf_data, true);  // true enables automatic memory management
    return ptr;
}

// [[Rcpp::export]]
double augmentExpectation(NumericVector &x, NumericMatrix &dataset, NumericMatrix &tree, NumericVector &to_explain, SEXP leaf_data_ptr) {
    const Rcpp::XPtr<LeafData> leaf_data(leaf_data_ptr);
    return _augmentExpectation(x, dataset, tree, to_explain, *leaf_data);
}