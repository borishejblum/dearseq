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


// Asymptotic test needs T_fast indepently of q_fast.
// The vcode below might achieve very marginal speed gains for large dataset in permutation setting,
//b ut would decouple the code between permutations and asymptotic tests too much.
// Eigen::MatrixXd compute_q_cpp(const Eigen::MatrixXd& sig_eps_inv_T,
//                               const Eigen::MatrixXd& phi_sig_xi_sqrt,
//                               const Eigen::MatrixXd& yt_mu_reshaped) {
//   int n = sig_eps_inv_T.rows();
//   int g = sig_eps_inv_T.cols();
//   int K = phi_sig_xi_sqrt.cols();
//
//   // Allocate result matrix
//   Eigen::MatrixXd q_fast(n, g * K);
//
//   // Compute T_fast and directly apply yt_mu_reshaped
//   for (int k = 0; k < K; ++k) {
//     q_fast.middleCols(k * g, g) = yt_mu_reshaped.array() *
//       (sig_eps_inv_T.array().colwise() * phi_sig_xi_sqrt.col(k).array());
//   }
//
//   return q_fast;
// }
