# Permutation-based variance component test statistic and p-value

This function computes an approximation of the Variance Component test
for a mixture of \\\chi^{2}\\s using permutations. This is preferable to
the asymptotic approximation for small sample sizes. We rely on exact
p-values following Phipson and Smyth, 2010 (see References).

## Usage

``` r
vc_test_perm(
  y,
  x,
  indiv = rep(1, nrow(x)),
  phi,
  w,
  Sigma_xi = diag(ncol(phi)),
  n_perm = 1000,
  progressbar = TRUE,
  parallel_comp = TRUE,
  nb_cores = parallel::detectCores(logical = FALSE) - 1,
  genewise_pvals = FALSE,
  adaptive = TRUE,
  max_adaptive = 64000,
  homogen_traj = FALSE,
  na.rm = FALSE
)
```

## Arguments

- y:

  a numeric matrix of dim `G x n` containing the raw RNA-seq counts for
  G genes from `n` samples.

- x:

  a numeric design matrix of dim `n x p` containing the `p` covariates
  to be adjusted for.

- indiv:

  a vector of length `n` containing the information for attributing each
  sample to one of the studied individuals. Coerced to be a `factor`.

- phi:

  a numeric design matrix of size `n x K` containing the `K` variables
  to be tested

- w:

  a vector of length `n` containing the weights for the `n` samples.

- Sigma_xi:

  a matrix of size `K x K` containing the covariance matrix of the `K`
  random effects.

- n_perm:

  the number of perturbations. Default is `1000`.

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

- genewise_pvals:

  a logical flag indicating whether gene-wise p-values should be
  returned. Default is `FALSE` in which case gene-set p-value is
  computed and returned instead.

- adaptive:

  a logical flag indicating whether adaptive permutation should be
  performed. Default is `TRUE`

- max_adaptive:

  The maximum number of permutations considered. Default is `64000`

- homogen_traj:

  a logical flag indicating whether trajectories should be considered
  homogeneous. Default is `FALSE` in which case trajectories are not
  only tested for trend, but also for heterogeneity.

- na.rm:

  logical: should missing values (including `NA` and `NaN`) be omitted
  from the calculations? Default is `FALSE`.

## Value

A list with the following elements when the set p-value is computed:

- `set_score_obs`: the approximation of the observed set score

- `set_pval`: the associated set p-value

or a list with the following elements when gene-wise p-values are
computed:

- `gene_scores_obs`: vector of approximating the observed gene-wise
  scores

- `gene_pvals`: vector of associated gene-wise p-values

- `ds_fdr`: vector of associated gene-wise discrete false discovery
  rates

## References

Phipson B, and Smyth GK (2010). Permutation p-values should never be
zero: calculating exact p-values when permutations are randomly drawn.
*Statistical Applications in Genetics and Molecular Biology*, Volume 9,
Issue 1, Article 39.
<http://www.statsci.org/smyth/pubs/PermPValuesPreprint.pdf>

## Examples

``` r
set.seed(123)

##generate some fake data
########################
n <- 100
r <- 12
t <- matrix(rep(1:3), 4, ncol=1, nrow=r)
sigma <- 0.4
b0 <- 1

#under the null:
b1 <- 0
#under the alternative:
b1 <- 0.5
y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
      matrix(rep(y.tilde, n), ncol=n, nrow=r))
x <- matrix(1, ncol=1, nrow=r)

#run test
permTestRes <- vc_test_perm(y, x, phi=t,
                           w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
                           indiv=rep(1:4, each=3), n_perm=50, #1000,
                           parallel_comp = FALSE)
#> Performing 50 initial permutations for 100 genes
permTestRes$set_pval
#> [1] 0.01960784
```
