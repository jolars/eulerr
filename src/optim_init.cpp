// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
double optim_init_loss(
    arma::vec par,
    arma::vec d,
    arma::uvec disjoint,
    arma::uvec contained,
    arma::umat two
) {
  uword n = par.n_elem;
  vec x = par.head(n/2),
      y = par.tail(n/2);
  uvec twoa = two.col(0) - 1,
       twob = two.col(1) - 1;
  vec D = pow(x(twoa) - x(twob), 2) + pow(y(twoa) - y(twob), 2);
  uvec i = (((D >= pow(d, 2)) % disjoint) + ((D <= pow(d, 2)) % contained)) > 0;
  uvec j = find(i == 0);
  vec di = d(j),
      Di = D(j);
  return accu(pow(Di - pow(di, 2), 2));
}

// [[Rcpp::export]]
NumericVector optim_init_grad(
    arma::vec par,
    arma::vec d,
    arma::uvec disjoint,
    arma::uvec contained,
    arma::umat two
) {
  uword n = par.n_elem / 2;
  vec x = par.head(n),
      y = par.tail(n);
  two--;
  uvec twoa = two.col(0),
       twob = two.col(1);
  vec xd = x(twoa) - x(twob),
      yd = y(twoa) - y(twob);
  vec D = pow(xd, 2) + pow(yd, 2);

  uvec i = (((D >= pow(d, 2)) % disjoint) + ((D <= pow(d, 2)) % contained)) > 0;
  uvec j = find(i == 0);

  vec gradx(i.n_elem, fill::zeros),
      grady(i.n_elem, fill::zeros);

  gradx(j) = 4 * (D(j) - pow(d(j), 2)) % xd(j);
  grady(j) = 4 * (D(j) - pow(d(j), 2)) % yd(j);

  mat gradxx = join_rows(gradx, -gradx);
  mat gradyy = join_rows(grady, -grady);

  NumericVector out(2 * n);
  std::fill(out.begin(), out.end(), 0);

  for (uword k = 0; k < n; ++k) {
    out(k)     = accu(gradxx(find(two == k)));
    out(k + n) = accu(gradyy(find(two == k)));
  }
  return out;
}
