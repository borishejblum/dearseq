# Computes variance component score test statistic for homogeneous trajectories

This function computes the variance component score test statistics for
homogeneous trajectories

## Usage

``` r
vc_score_h(y, x, indiv, phi, w, Sigma_xi = diag(ncol(phi)), na_rm = FALSE)
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

## Value

A list with the following elements:

- `score`: approximation of the set observed score

- `q`: observation-level contributions to the score

- `q_ext`: pseudo-observations used to compute covariance taking into
  account the contributions of OLS estimates

- `gene_scores`: approximation of the individual gene scores

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
      #    matrix(rep(tim2, ng), ncol=ng, nrow=nsample, byrow=FALSE) +
      matrix(rnorm(ng*nsample, sd = sigma), ncol=ng, nrow=nsample,
             byrow=FALSE)
      )
myindiv <- rep(1:nindiv, each=nt)
x <- cbind(1, myindiv/2==floor(myindiv/2))
myw <- matrix(rnorm(nsample*ng, sd=0.1), ncol=nsample, nrow=ng)

#run test
score_homogen <- vc_score_h(y, x, phi=tim, indiv=myindiv,
                           w=myw, Sigma_xi=cov(tim))
score_homogen$score
#> [1] 1248.097

score_heterogen <- vc_score(y, x, phi=tim, indiv=myindiv,
                           w=myw, Sigma_xi=cov(tim))
score_heterogen$score
#> [1] 705.5732

scoreTest_homogen <- vc_test_asym(y, x, phi=tim, indiv=rep(1:nindiv, each=nt),
                                 w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
                                 Sigma_xi=cov(tim),
                                 homogen_traj = TRUE)
scoreTest_homogen$set_pval
#> [1] 0
scoreTest_heterogen <- vc_test_asym(y, x, phi=tim, indiv=rep(1:nindiv,
                                                         each=nt),
                                   w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
                                   Sigma_xi=cov(tim),
                                   homogen_traj = FALSE)
scoreTest_heterogen$set_pval
#> [1] 0.4607837
```
