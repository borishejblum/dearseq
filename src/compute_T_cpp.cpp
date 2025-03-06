#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
Eigen::MatrixXd compute_T_cpp(const Eigen::MatrixXd& sig_eps_inv_T,
                              const Eigen::MatrixXd& phi_sig_xi_sqrt) {
  int n = sig_eps_inv_T.rows();
  int g = sig_eps_inv_T.cols();
  int K = phi_sig_xi_sqrt.cols();

  // Use Kronecker-like behavior to efficiently scale columns
  Eigen::MatrixXd T_fast(n, g * K);

  for (int k = 0; k < K; k++) {
    T_fast.middleCols(k * g, g) = sig_eps_inv_T.array().colwise() * phi_sig_xi_sqrt.col(k).array();
  }

  return T_fast;
}
