// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

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
// recurse
Rcpp::NumericMatrix recurse(Rcpp::NumericMatrix& x, Rcpp::IntegerVector& feature, Rcpp::NumericVector& split, Rcpp::IntegerVector& yes, Rcpp::IntegerVector& no, Rcpp::NumericVector& quality, Rcpp::NumericVector& cover, std::vector<std::vector<unsigned int> >& U, unsigned int node);
RcppExport SEXP _glex_recurse(SEXP xSEXP, SEXP featureSEXP, SEXP splitSEXP, SEXP yesSEXP, SEXP noSEXP, SEXP qualitySEXP, SEXP coverSEXP, SEXP USEXP, SEXP nodeSEXP) {
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
    Rcpp::traits::input_parameter< std::vector<std::vector<unsigned int> >& >::type U(USEXP);
    Rcpp::traits::input_parameter< unsigned int >::type node(nodeSEXP);
    rcpp_result_gen = Rcpp::wrap(recurse(x, feature, split, yes, no, quality, cover, U, node));
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

static const R_CallMethodDef CallEntries[] = {
    {"_glex_find_term_matches", (DL_FUNC) &_glex_find_term_matches, 2},
    {"_glex_recurse", (DL_FUNC) &_glex_recurse, 9},
    {"_glex_contribute", (DL_FUNC) &_glex_contribute, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_glex(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}