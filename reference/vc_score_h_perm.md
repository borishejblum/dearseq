# Computes variance component score test statistics for homogeneous trajectory and its permuted distribution

This function computes the variance component score test statistics for
homogeneous trajectories along with its permuted values for estimating
its distribution under the null hypothesis.

## Usage

``` r
vc_score_h_perm(
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

  a numeric matrix of dim `g x n` containing the raw or normalized
  RNA-seq counts for g genes from `n` samples.

- x:

  a numeric design matrix of dim `n x p` containing the `p` covariates
  to be adjusted for.

- indiv:

  a vector of length `n` containing the information for attributing each
  sample to one of the studied individuals. Coerced to be a `factor`.

- phi:

  a numeric design matrix of size `n x K` containing the `K`
  longitudinal variables to be tested (typically a vector of time points
  or functions of time).

- w:

  a vector of length `n` containing the weights for the `n` samples,
  corresponding to the inverse of the diagonal of the estimated
  covariance matrix of y.

- Sigma_xi:

  a matrix of size `K x K` containing the covariance matrix of the `K`
  random effects corresponding to `phi`.

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

- `gene_scores_unscaled_perm`: a list of approximation of the permuted
  individual gene scores

## Examples

``` r
set.seed(123)

##generate some fake data
########################
ng <- 100
nindiv <- 30
nt <- 5
nsample <- nindiv*nt
tim <- matrix(rep(1:nt), nindiv, ncol=1, nrow=nsample)
tim2 <- tim^2
sigma <- 5
b0 <- 10

#under the null:
beta1 <- rnorm(n=ng, 0, sd=0)
#under the (heterogen) alternative:
beta1 <- rnorm(n=ng, 0, sd=0.1)
#under the (homogen) alternative:
beta1 <- rnorm(n=ng, 0.06, sd=0)

y.tilde <- b0 + rnorm(ng, sd = sigma)
y <- t(matrix(rep(y.tilde, nsample), ncol=ng, nrow=nsample, byrow=TRUE) +
      matrix(rep(beta1, each=nsample), ncol=ng, nrow=nsample, byrow=FALSE) *
           matrix(rep(tim, ng), ncol=ng, nrow=nsample, byrow=FALSE) +
      #matrix(rep(beta1, each=nsample), ncol=ng, nrow=nsample, byrow=FALSE) *
      #     matrix(rep(tim2, ng), ncol=ng, nrow=nsample, byrow=FALSE) +
      matrix(rnorm(ng*nsample, sd = sigma), ncol=ng, nrow=nsample,
             byrow=FALSE)
      )
myindiv <- rep(1:nindiv, each=nt)
x <- cbind(1, myindiv/2==floor(myindiv/2))
myw <- matrix(rnorm(nsample*ng, sd=0.1), ncol=nsample, nrow=ng)

#run test
#We only use few permutations (10) to keep example running time low
#Otherwise one can use n_perm = 1000
score_homogen <- vc_score_h_perm(y, x, phi=tim, indiv=myindiv,
                                w=myw, Sigma_xi=cov(tim), n_perm = 10,
                                parallel_comp = FALSE)
score_homogen$score
#> [1] 1248.097

score_heterogen <- vc_score_perm(y, x, phi=tim, indiv=myindiv,
                           w=myw, Sigma_xi=cov(tim), n_perm = 10,
                           parallel_comp = FALSE)
score_heterogen$score
#> [1] 705.5732

scoreTest_homogen <- vc_test_asym(y, x, phi=tim, indiv=rep(1:nindiv, each=nt),
                                 w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
                                 Sigma_xi=cov(tim), homogen_traj = TRUE)
scoreTest_homogen$set_pval
#> [1] 0
scoreTest_heterogen <- vc_test_asym(y, x, phi=tim, indiv=rep(1:nindiv,
                                                            each=nt),
                                   w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
                                   Sigma_xi=cov(tim), homogen_traj = FALSE)
scoreTest_heterogen$set_pval
#> [1] 0.4607837
```
