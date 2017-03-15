#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec Mahalanobis(arma::mat x, arma::rowvec center, arma::mat cov){
  int n = x.n_rows;
  arma::mat x_cen;
  x_cen.copy_size(x);
  for (int i=0; i < n; i++) {
    x_cen.row(i) = x.row(i) - center;
  }
  return sum((x_cen * cov.i()) % x_cen, 1);    
}
