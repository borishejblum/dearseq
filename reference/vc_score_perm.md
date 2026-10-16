# Computes variance component score test statistics and its permuted distribution

This function computes the variance component score test statistics
along with its permuted values for estimating its distribution under the
null hypothesis.

## Usage

``` r
vc_score_perm(
  y,
  x,
  indiv,
  phi,
  w,
  Sigma_xi = diag(ncol(phi)),
  na_rm = FALSE,
  n_perm = 1000,
  progressbar = TRUE,
  parallel_comp = TRUE,
  nb_cores = parallel::detectCores(logical = FALSE) - 1
)
```

## Arguments

- y:

  a numeric matrix of dim `g x n` containing the raw RNA-seq counts for
  g genes from `n` samples

- x:

  a numeric design matrix of dim `n x p` containing the `p` covariates
  to be adjusted for

- indiv:

  a vector of length `n` containing the information for attributing each
  sample to one of the studied individuals. Coerced to be a `factor`

- phi:

  a numeric design matrix of size `n x K` containing the `K` variables
  to be tested.

- w:

  a vector of length `n` containing the weights for the `n` samples.

- Sigma_xi:

  a matrix of size `K x K` containing the covariance matrix of the `K`
  random effects on `phi`

- na_rm:

  logical: should missing values (including `NA` and `NaN`) be omitted
  from the calculations? Default is `FALSE`.

- n_perm:

  the number of permutation to perform. Default is `1000`.

- progressbar:

  logical indicating wether a progressBar should be displayed when
  computing permutations (only in interactive mode).

- parallel_comp:

  a logical flag indicating whether parallel computation should be
  enabled. Only Linux and MacOS are supported, this is ignored on
  Windows. Default is `TRUE`.

- nb_cores:

  an integer indicating the number of cores to be used when
  `parallel_comp` is `TRUE`. Default is
  `parallel::detectCores(logical=FALSE) - 1`.

## Value

A list with the following elements:

- `score`: an approximation of the observed set score

- `scores_perm`: a vector containing the permuted set scores

- `gene_scores_unscaled`: approximation of the individual gene scores

- `gene_scores_unscaled_perm`: a list of approximationq of the permuted
  individual gene scores

## Examples

``` r
set.seed(123)

##generate some fake data
########################
n <- 100
r <- 12
t <- matrix(rep(1:3), r/3, ncol=1, nrow=r)
sigma <- 0.4
b0 <- 1

#under the null:
b1 <- 0
#under the alternative:
b1 <- 0.7
y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
      matrix(rep(y.tilde, n), ncol=n, nrow=r))
x <- matrix(1, ncol=1, nrow=r)

#run test
scoreTest <- vc_score_perm(y, x, phi=t, w=matrix(1, ncol=ncol(y),
                                                nrow=nrow(y)),
                    Sigma_xi=matrix(1), indiv=rep(1:(r/3), each=3),
                    parallel_comp = FALSE)
scoreTest$score
#> [1] 1487.231
```
