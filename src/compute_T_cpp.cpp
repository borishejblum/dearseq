#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix compute_T_cpp(const NumericMatrix& sig_eps_inv_T,
                            const NumericMatrix& phi_sig_xi_sqrt) {
  int n = sig_eps_inv_T.nrow();    // number of rows
  int g = sig_eps_inv_T.ncol();    // number of columns of sig_eps_inv_T
  int K = phi_sig_xi_sqrt.ncol();   // number of columns of phi_sig_xi_sqrt

  // Create an n x (g*K) result matrix.
  NumericMatrix T_fast(n, g * K);

  // For each row, multiply each element of sig_eps_inv_T by the corresponding replicated phi value.
  // For row i, and for each block k = 0,...,K-1:
  //   for each column r in 0,...,g-1, set T_fast(i, k*g + r) = sig_eps_inv_T(i, r) * phi_sig_xi_sqrt(i, k)
  for (int i = 0; i < n; i++) {
    for (int k = 0; k < K; k++) {
      double phi_val = phi_sig_xi_sqrt(i, k);
      for (int r = 0; r < g; r++) {
        T_fast(i, k * g + r) = sig_eps_inv_T(i, r) * phi_val;
      }
    }
  }

  return T_fast;
}
