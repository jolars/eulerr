// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// intersect_ellipses
arma::vec intersect_ellipses(const arma::vec& par, const bool circles);
RcppExport SEXP _eulerr_intersect_ellipses(SEXP parSEXP, SEXP circlesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type par(parSEXP);
    Rcpp::traits::input_parameter< const bool >::type circles(circlesSEXP);
    rcpp_result_gen = Rcpp::wrap(intersect_ellipses(par, circles));
    return rcpp_result_gen;
END_RCPP
}
// optim_final_loss
double optim_final_loss(const arma::vec& par, const arma::vec& areas, const bool circles);
RcppExport SEXP _eulerr_optim_final_loss(SEXP parSEXP, SEXP areasSEXP, SEXP circlesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type par(parSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type areas(areasSEXP);
    Rcpp::traits::input_parameter< const bool >::type circles(circlesSEXP);
    rcpp_result_gen = Rcpp::wrap(optim_final_loss(par, areas, circles));
    return rcpp_result_gen;
END_RCPP
}
// optim_init
Rcpp::NumericVector optim_init(const arma::rowvec& par, const arma::vec& d, const arma::uvec& disjoint, const arma::uvec& contained);
RcppExport SEXP _eulerr_optim_init(SEXP parSEXP, SEXP dSEXP, SEXP disjointSEXP, SEXP containedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::rowvec& >::type par(parSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type d(dSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type disjoint(disjointSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type contained(containedSEXP);
    rcpp_result_gen = Rcpp::wrap(optim_init(par, d, disjoint, contained));
    return rcpp_result_gen;
END_RCPP
}
// locate_centers
arma::mat locate_centers(const arma::vec& h, const arma::vec& k, const arma::vec& a, const arma::vec& b, const arma::vec& phi, const arma::vec& fitted);
RcppExport SEXP _eulerr_locate_centers(SEXP hSEXP, SEXP kSEXP, SEXP aSEXP, SEXP bSEXP, SEXP phiSEXP, SEXP fittedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type h(hSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type k(kSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type b(bSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type fitted(fittedSEXP);
    rcpp_result_gen = Rcpp::wrap(locate_centers(h, k, a, b, phi, fitted));
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
// bit_indexr
Rcpp::LogicalMatrix bit_indexr(const arma::uword n);
RcppExport SEXP _eulerr_bit_indexr(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::uword >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(bit_indexr(n));
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
// stress
double stress(const arma::vec& areas, const arma::vec& fit);
RcppExport SEXP _eulerr_stress(SEXP areasSEXP, SEXP fitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type areas(areasSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type fit(fitSEXP);
    rcpp_result_gen = Rcpp::wrap(stress(areas, fit));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_eulerr_intersect_ellipses", (DL_FUNC) &_eulerr_intersect_ellipses, 2},
    {"_eulerr_optim_final_loss", (DL_FUNC) &_eulerr_optim_final_loss, 3},
    {"_eulerr_optim_init", (DL_FUNC) &_eulerr_optim_init, 4},
    {"_eulerr_locate_centers", (DL_FUNC) &_eulerr_locate_centers, 6},
    {"_eulerr_choose_two", (DL_FUNC) &_eulerr_choose_two, 1},
    {"_eulerr_bit_indexr", (DL_FUNC) &_eulerr_bit_indexr, 1},
    {"_eulerr_discdisc", (DL_FUNC) &_eulerr_discdisc, 4},
    {"_eulerr_stress", (DL_FUNC) &_eulerr_stress, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_eulerr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
