#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List perm_list_cpp(const Rcpp::IntegerVector& indiv,
                            const int nb_indiv,
                            const int n,
                            const int n_perm,
                            const Rcpp::IntegerVector& o) {

  // Create a list of nb_indiv empty integer vectors.
  Rcpp::List groups(nb_indiv);
  for (int j = 0; j < nb_indiv; j++) {
    groups[j] = Rcpp::IntegerVector();
  }

  // Fill groups: for each index, append (i+1) to the proper group.
  for (int i = 0; i < n; i++) {
    int grp = indiv[i]; // assuming indiv is 1-indexed
    // Retrieve the group vector, append the new element, then store it back.
    Rcpp::IntegerVector group = groups[grp - 1];
    group.push_back(i + 1);
    groups[grp - 1] = group;
  }


  // Original sequence
  Rcpp::List perm_list(n_perm + 1);
  perm_list[0] = Rcpp::seq_len(n);

  // Generate permutations
  for (int p = 1; p <= n_perm; p++) {
    Rcpp::IntegerVector perm(n);
    int index = 0;

    for (int j = 0; j < nb_indiv; j++) {
      Rcpp::IntegerVector group = groups[j];
      Rcpp::IntegerVector shuffled = Rcpp::sample(group, group.size());

      for (int k = 0; k < shuffled.size(); k++) {
        perm[index++] = shuffled[k];
      }
    }

    perm_list[p] = perm[o-1]; // Reorder using o
  }

  return perm_list;
}
