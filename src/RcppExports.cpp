// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/glex.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// augmentAndTakeExpectation
double augmentAndTakeExpectation(NumericVector& x, NumericMatrix& dataset, NumericMatrix& tree, NumericVector& to_explain, bool is_weak_inequality);
RcppExport SEXP _glex_augmentAndTakeExpectation(SEXP xSEXP, SEXP datasetSEXP, SEXP treeSEXP, SEXP to_explainSEXP, SEXP is_weak_inequalitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type dataset(datasetSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type tree(treeSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type to_explain(to_explainSEXP);
    Rcpp::traits::input_parameter< bool >::type is_weak_inequality(is_weak_inequalitySEXP);
    rcpp_result_gen = Rcpp::wrap(augmentAndTakeExpectation(x, dataset, tree, to_explain, is_weak_inequality));
    return rcpp_result_gen;
END_RCPP
}
// augmentTree
XPtr<LeafData> augmentTree(NumericMatrix& tree, NumericMatrix& dataset, bool is_weak_inequality);
RcppExport SEXP _glex_augmentTree(SEXP treeSEXP, SEXP datasetSEXP, SEXP is_weak_inequalitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type tree(treeSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type dataset(datasetSEXP);
    Rcpp::traits::input_parameter< bool >::type is_weak_inequality(is_weak_inequalitySEXP);
    rcpp_result_gen = Rcpp::wrap(augmentTree(tree, dataset, is_weak_inequality));
    return rcpp_result_gen;
END_RCPP
}
// augmentExpectation
double augmentExpectation(NumericVector& x, NumericMatrix& tree, NumericVector& to_explain, SEXP leaf_data_ptr, bool is_weak_inequality);
RcppExport SEXP _glex_augmentExpectation(SEXP xSEXP, SEXP treeSEXP, SEXP to_explainSEXP, SEXP leaf_data_ptrSEXP, SEXP is_weak_inequalitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type tree(treeSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type to_explain(to_explainSEXP);
    Rcpp::traits::input_parameter< SEXP >::type leaf_data_ptr(leaf_data_ptrSEXP);
    Rcpp::traits::input_parameter< bool >::type is_weak_inequality(is_weak_inequalitySEXP);
    rcpp_result_gen = Rcpp::wrap(augmentExpectation(x, tree, to_explain, leaf_data_ptr, is_weak_inequality));
    return rcpp_result_gen;
END_RCPP
}
// marginalizeAllSplittedSubsetsinTree
Rcpp::NumericMatrix marginalizeAllSplittedSubsetsinTree(Rcpp::NumericMatrix& x, NumericMatrix& tree, bool is_weak_inequality);
RcppExport SEXP _glex_marginalizeAllSplittedSubsetsinTree(SEXP xSEXP, SEXP treeSEXP, SEXP is_weak_inequalitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type tree(treeSEXP);
    Rcpp::traits::input_parameter< bool >::type is_weak_inequality(is_weak_inequalitySEXP);
    rcpp_result_gen = Rcpp::wrap(marginalizeAllSplittedSubsetsinTree(x, tree, is_weak_inequality));
    return rcpp_result_gen;
END_RCPP
}
// explainTreeFastPD
Rcpp::NumericMatrix explainTreeFastPD(Rcpp::NumericMatrix& x, NumericMatrix& tree, Rcpp::List& to_explain_list, unsigned int max_interaction, bool is_weak_inequality);
RcppExport SEXP _glex_explainTreeFastPD(SEXP xSEXP, SEXP treeSEXP, SEXP to_explain_listSEXP, SEXP max_interactionSEXP, SEXP is_weak_inequalitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type tree(treeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type to_explain_list(to_explain_listSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type max_interaction(max_interactionSEXP);
    Rcpp::traits::input_parameter< bool >::type is_weak_inequality(is_weak_inequalitySEXP);
    rcpp_result_gen = Rcpp::wrap(explainTreeFastPD(x, tree, to_explain_list, max_interaction, is_weak_inequality));
    return rcpp_result_gen;
END_RCPP
}
// explainTreeFastPDBitmask
Rcpp::NumericMatrix explainTreeFastPDBitmask(Rcpp::NumericMatrix& x, Rcpp::NumericMatrix& x_background, NumericMatrix& tree, Rcpp::List& to_explain_list, unsigned int max_interaction, bool is_weak_inequality);
RcppExport SEXP _glex_explainTreeFastPDBitmask(SEXP xSEXP, SEXP x_backgroundSEXP, SEXP treeSEXP, SEXP to_explain_listSEXP, SEXP max_interactionSEXP, SEXP is_weak_inequalitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x_background(x_backgroundSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type tree(treeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List& >::type to_explain_list(to_explain_listSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type max_interaction(max_interactionSEXP);
    Rcpp::traits::input_parameter< bool >::type is_weak_inequality(is_weak_inequalitySEXP);
    rcpp_result_gen = Rcpp::wrap(explainTreeFastPDBitmask(x, x_background, tree, to_explain_list, max_interaction, is_weak_inequality));
    return rcpp_result_gen;
END_RCPP
}
// find_term_matches
std::vector<int> find_term_matches(std::string main_term, std::vector<std::string> terms);
RcppExport SEXP _glex_find_term_matches(SEXP main_termSEXP, SEXP termsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type main_term(main_termSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type terms(termsSEXP);
    rcpp_result_gen = Rcpp::wrap(find_term_matches(main_term, terms));
    return rcpp_result_gen;
END_RCPP
}
// empProbFunction
double empProbFunction(Rcpp::NumericMatrix& x, Rcpp::IntegerVector& coords, Rcpp::NumericVector& lb, Rcpp::NumericVector& ub);
RcppExport SEXP _glex_empProbFunction(SEXP xSEXP, SEXP coordsSEXP, SEXP lbSEXP, SEXP ubSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type lb(lbSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type ub(ubSEXP);
    rcpp_result_gen = Rcpp::wrap(empProbFunction(x, coords, lb, ub));
    return rcpp_result_gen;
END_RCPP
}
// recurseRcppEmpProbfunction
Rcpp::NumericMatrix recurseRcppEmpProbfunction(Rcpp::NumericMatrix& x, Rcpp::IntegerVector& feature, Rcpp::NumericVector& split, Rcpp::IntegerVector& yes, Rcpp::IntegerVector& no, Rcpp::NumericVector& quality, Rcpp::NumericMatrix& lb, Rcpp::NumericMatrix& ub, Rcpp::IntegerVector& cover, std::vector<std::vector<unsigned int>>& U, unsigned int node);
RcppExport SEXP _glex_recurseRcppEmpProbfunction(SEXP xSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP qualitySEXP, SEXP lbSEXP, SEXP ubSEXP, SEXP coverSEXP, SEXP USEXP, SEXP nodeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type split(splitSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type no(noSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type quality(qualitySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type lb(lbSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type ub(ubSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type cover(coverSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<unsigned int>>& >::type U(USEXP);
    Rcpp::traits::input_parameter< unsigned int >::type node(nodeSEXP);
    rcpp_result_gen = Rcpp::wrap(recurseRcppEmpProbfunction(x, feature, split, yes, no, quality, lb, ub, cover, U, node));
    return rcpp_result_gen;
END_RCPP
}
// recurse
Rcpp::NumericMatrix recurse(Rcpp::NumericMatrix& x, Rcpp::IntegerVector& feature, Rcpp::NumericVector& split, Rcpp::IntegerVector& yes, Rcpp::IntegerVector& no, Rcpp::NumericVector& quality, Rcpp::NumericMatrix& lb, Rcpp::NumericMatrix& ub, Rcpp::IntegerVector& cover, std::vector<std::vector<unsigned int>>& U, unsigned int node, Rcpp::Function probFunction);
RcppExport SEXP _glex_recurse(SEXP xSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP qualitySEXP, SEXP lbSEXP, SEXP ubSEXP, SEXP coverSEXP, SEXP USEXP, SEXP nodeSEXP, SEXP probFunctionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type split(splitSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type no(noSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type quality(qualitySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type lb(lbSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type ub(ubSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type cover(coverSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<unsigned int>>& >::type U(USEXP);
    Rcpp::traits::input_parameter< unsigned int >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function >::type probFunction(probFunctionSEXP);
    rcpp_result_gen = Rcpp::wrap(recurse(x, feature, split, yes, no, quality, lb, ub, cover, U, node, probFunction));
    return rcpp_result_gen;
END_RCPP
}
// recurseAlgorithm2
Rcpp::NumericMatrix recurseAlgorithm2(Rcpp::NumericMatrix& x, Rcpp::IntegerVector& feature, Rcpp::NumericVector& split, Rcpp::IntegerVector& yes, Rcpp::IntegerVector& no, Rcpp::NumericVector& quality, Rcpp::NumericVector& cover, std::vector<std::vector<unsigned int>>& U, unsigned int node);
RcppExport SEXP _glex_recurseAlgorithm2(SEXP xSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP qualitySEXP, SEXP coverSEXP, SEXP USEXP, SEXP nodeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type split(splitSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type no(noSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type quality(qualitySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type cover(coverSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<unsigned int>>& >::type U(USEXP);
    Rcpp::traits::input_parameter< unsigned int >::type node(nodeSEXP);
    rcpp_result_gen = Rcpp::wrap(recurseAlgorithm2(x, feature, split, yes, no, quality, cover, U, node));
    return rcpp_result_gen;
END_RCPP
}
// contribute
void contribute(Rcpp::NumericMatrix& mat, Rcpp::NumericMatrix& m_all, Rcpp::IntegerVector& S, Rcpp::IntegerVector& T, std::vector<Rcpp::IntegerVector>& T_subsets, unsigned int colnum);
RcppExport SEXP _glex_contribute(SEXP matSEXP, SEXP m_allSEXP, SEXP SSEXP, SEXP TSEXP, SEXP T_subsetsSEXP, SEXP colnumSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type m_all(m_allSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type S(SSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type T(TSEXP);
    Rcpp::traits::input_parameter< std::vector<Rcpp::IntegerVector>& >::type T_subsets(T_subsetsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type colnum(colnumSEXP);
    contribute(mat, m_all, S, T, T_subsets, colnum);
    return R_NilValue;
END_RCPP
}
// get_all_subsets_cpp
Rcpp::List get_all_subsets_cpp(Rcpp::IntegerVector x, unsigned int maxSize);
RcppExport SEXP _glex_get_all_subsets_cpp(SEXP xSEXP, SEXP maxSizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type maxSize(maxSizeSEXP);
    rcpp_result_gen = Rcpp::wrap(get_all_subsets_cpp(x, maxSize));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_glex_augmentAndTakeExpectation", (DL_FUNC) &_glex_augmentAndTakeExpectation, 5},
    {"_glex_augmentTree", (DL_FUNC) &_glex_augmentTree, 3},
    {"_glex_augmentExpectation", (DL_FUNC) &_glex_augmentExpectation, 5},
    {"_glex_marginalizeAllSplittedSubsetsinTree", (DL_FUNC) &_glex_marginalizeAllSplittedSubsetsinTree, 3},
    {"_glex_explainTreeFastPD", (DL_FUNC) &_glex_explainTreeFastPD, 5},
    {"_glex_explainTreeFastPDBitmask", (DL_FUNC) &_glex_explainTreeFastPDBitmask, 6},
    {"_glex_find_term_matches", (DL_FUNC) &_glex_find_term_matches, 2},
    {"_glex_empProbFunction", (DL_FUNC) &_glex_empProbFunction, 4},
    {"_glex_recurseRcppEmpProbfunction", (DL_FUNC) &_glex_recurseRcppEmpProbfunction, 11},
    {"_glex_recurse", (DL_FUNC) &_glex_recurse, 12},
    {"_glex_recurseAlgorithm2", (DL_FUNC) &_glex_recurseAlgorithm2, 9},
    {"_glex_contribute", (DL_FUNC) &_glex_contribute, 6},
    {"_glex_get_all_subsets_cpp", (DL_FUNC) &_glex_get_all_subsets_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_glex(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
