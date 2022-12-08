// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// intersect_ellipses
std::vector<double> intersect_ellipses(const std::vector<double>& par, const bool circle, const bool approx);
RcppExport SEXP _eulerr_intersect_ellipses(SEXP parSEXP, SEXP circleSEXP, SEXP approxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type par(parSEXP);
    Rcpp::traits::input_parameter< const bool >::type circle(circleSEXP);
    Rcpp::traits::input_parameter< const bool >::type approx(approxSEXP);
    rcpp_result_gen = Rcpp::wrap(intersect_ellipses(par, circle, approx));
    return rcpp_result_gen;
END_RCPP
}
// optim_final_loss
double optim_final_loss(const std::vector<double>& par, const std::vector<double>& data, const bool circle, const std::string& loss_type, const std::string& loss_aggregator_type);
RcppExport SEXP _eulerr_optim_final_loss(SEXP parSEXP, SEXP dataSEXP, SEXP circleSEXP, SEXP loss_typeSEXP, SEXP loss_aggregator_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type par(parSEXP);
    Rcpp::traits::input_parameter< const std::vector<double>& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const bool >::type circle(circleSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type loss_type(loss_typeSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type loss_aggregator_type(loss_aggregator_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(optim_final_loss(par, data, circle, loss_type, loss_aggregator_type));
    return rcpp_result_gen;
END_RCPP
}
// optim_init
Rcpp::NumericVector optim_init(const Rcpp::NumericVector& par, const Rcpp::NumericMatrix& d, const Rcpp::LogicalMatrix& disjoint, const Rcpp::LogicalMatrix& subset);
RcppExport SEXP _eulerr_optim_init(SEXP parSEXP, SEXP dSEXP, SEXP disjointSEXP, SEXP subsetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type par(parSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type d(dSEXP);
    Rcpp::traits::input_parameter< const Rcpp::LogicalMatrix& >::type disjoint(disjointSEXP);
    Rcpp::traits::input_parameter< const Rcpp::LogicalMatrix& >::type subset(subsetSEXP);
    rcpp_result_gen = Rcpp::wrap(optim_init(par, d, disjoint, subset));
    return rcpp_result_gen;
END_RCPP
}
// choose_two
arma::umat choose_two(const arma::uvec& x);
RcppExport SEXP _eulerr_choose_two(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::uvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(choose_two(x));
    return rcpp_result_gen;
END_RCPP
}
// discdisc
double discdisc(double d, double r1, double r2, double overlap);
RcppExport SEXP _eulerr_discdisc(SEXP dSEXP, SEXP r1SEXP, SEXP r2SEXP, SEXP overlapSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< double >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< double >::type overlap(overlapSEXP);
    rcpp_result_gen = Rcpp::wrap(discdisc(d, r1, r2, overlap));
    return rcpp_result_gen;
END_RCPP
}
// bit_index_cpp
arma::umat bit_index_cpp(arma::uword n);
RcppExport SEXP _eulerr_bit_index_cpp(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::uword >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(bit_index_cpp(n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_eulerr_intersect_ellipses", (DL_FUNC) &_eulerr_intersect_ellipses, 3},
    {"_eulerr_optim_final_loss", (DL_FUNC) &_eulerr_optim_final_loss, 5},
    {"_eulerr_optim_init", (DL_FUNC) &_eulerr_optim_init, 4},
    {"_eulerr_choose_two", (DL_FUNC) &_eulerr_choose_two, 1},
    {"_eulerr_discdisc", (DL_FUNC) &_eulerr_discdisc, 4},
    {"_eulerr_bit_index_cpp", (DL_FUNC) &_eulerr_bit_index_cpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_eulerr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
