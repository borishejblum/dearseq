# Differential expression analyis of RNA-seq data through a variance component test

Wrapper function for gene-by-gene association testing of RNA-seq data

## Usage

``` r
dear_seq(
  exprmat = NULL,
  object = NULL,
  covariates = NULL,
  variables2test,
  sample_group = NULL,
  weights_var2test_condi = (which_test != "permutation"),
  cov_variables2test_eff = NULL,
  which_test = c("permutation", "asymptotic"),
  which_weights = c("loclin", "voom", "none"),
  n_perm = 1000,
  progressbar = TRUE,
  parallel_comp = TRUE,
  nb_cores = parallel::detectCores(logical = FALSE) - 1,
  preprocessed = FALSE,
  gene_based_weights = FALSE,
  log_scale_gene_weights = TRUE,
  bw = "nrd",
  kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight",
    "tricube", "cosine", "optcosine"),
  transform = TRUE,
  padjust_methods = c("BH", "BY", "holm", "hochberg", "hommel", "bonferroni"),
  lowess_span = 0.5,
  R = NULL,
  adaptive = TRUE,
  max_adaptive = 64000,
  homogen_traj = FALSE,
  na.rm_dearseq = TRUE
)
```

## Arguments

- exprmat:

  a numeric matrix of size `G x n` containing the raw RNA-seq counts or
  preprocessed expressions from `n` samples for `G` genes. Default is
  `NULL`, in which case `object` must not be `NULL`.

- object:

  an object that can be either a
  [`SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/RangedSummarizedExperiment-class.html),
  an `ExpressionSet`, a
  [`DESeqDataSet`](https://rdrr.io/pkg/DESeq2/man/DESeqDataSet.html), or
  a [`DGEList`](https://rdrr.io/pkg/edgeR/man/DGEList.html). Default is
  `NULL`, in which case `exprmat` must not be `NULL`.

- covariates:

  - If `exprmat` is specified as a matrix: then `covariates` must be a
    numeric matrix of size `n x p` containing the model covariates for
    `n` samples (design matrix). Usually, its first column is the
    intercept (full of `1`s).

  - If `object` is specified: then `covariates` must be a character
    vector of length `p` containing the colnames of the design matrix
    given in `object`.

  If `covariates` is `NULL` (the default), then it is just the
  intercept.

- variables2test:

  - If `exprmat` is specified as a matrix: a numeric design matrix of
    size `n x K` containing the `K` variables to be tested.

  - If `object` is specified: then `variables2test` must be a character
    vector of length `K` containing the colnames of the design matrix
    given in `object`.

- sample_group:

  a vector of length `n` indicating whether the samples should be
  grouped (e.g. paired samples or longitudinal data). Coerced to be a
  `factor`. Default is `NULL` in which case no grouping is performed.

- weights_var2test_condi:

  a logical flag indicating whether heteroscedasticity weights
  computation should be conditional on both the variables to be tested
  `variables2test` and on the `covariates`, or on `covariates` alone.
  Default is `TRUE` for the asymptotic test (in which case conditional
  means are estimated conditionally on both `variables2test` and
  `covariates`), and `FALSE` for the permutation test (in which case
  conditional means are estimated conditionally on only the
  `covariates`).

- cov_variables2test_eff:

  a matrix of size `K x K` containing the covariance matrix of the `K`
  random effects. Only used if `homogen_traj` is `FALSE`. Default assume
  diagonal correlation matrix, i.e. independence of random effects.

- which_test:

  a character string indicating which method to use to approximate the
  variance component score test, either `'permutation'` or
  `'asymptotic'`. Default is `'permutation'`.

- which_weights:

  a character string indicating which method to use to estimate the
  mean-variance relationship weights. Possibilities are `'loclin'`,
  `'voom'` or `'none'` (in which case no weighting is performed).
  Default is `'loclin'`. See
  [`sp_weights`](https://borishejblum.github.io/dearseq/reference/sp_weights.md)
  and
  [`voom_weights`](https://borishejblum.github.io/dearseq/reference/voom_weights.md)
  for details.

- n_perm:

  the number of perturbations. Default is `1000`

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

- preprocessed:

  a logical flag indicating whether the expression data have already
  been preprocessed (e.g. log2 transformed). Default is `FALSE`, in
  which case `y` is assumed to contain raw counts and is normalized into
  log(counts) per million.

- gene_based_weights:

  a logical flag used for `'loclin'` weights, indicating whether to
  estimate weights at the gene-level, or rather at the
  observation-level. Default is `FALSE`, which is what it should be for
  gene-wise analysis.

- log_scale_gene_weights:

  a logical flag when `gene_based_weights = TRUE`, whether to smooth the
  variance estimator on the log scale before exponentiation back,
  guaranteeing positive smoothed variances (unlike smoothing on the raw
  scale, which can extrapolate to negative values). Default is `TRUE`.

- bw:

  a character string indicating the smoothing bandwidth selection method
  to use. See `bandwidth` for details. Possible values are `'ucv'`,
  `'SJ'`, `'bcv'`, `'nrd'` or `'nrd0'`.

- kernel:

  a character string indicating which kernel should be used.
  Possibilities are `'gaussian'`, `'epanechnikov'`, `'rectangular'`,
  `'triangular'`, `'biweight'`, `'tricube'`, `'cosine'`, `'optcosine'`.
  Default is `'gaussian'` (NB: `'tricube'` kernel corresponds to the
  loess method).

- transform:

  a logical flag used for `'loclin'` weights, indicating whether values
  should be transformed to uniform for the purpose of local linear
  smoothing. This may be helpful if tail observations are sparse and the
  specified bandwidth gives suboptimal performance there. Default is
  `TRUE`.

- padjust_methods:

  multiple testing correction method used if `genesets` is a list.
  Default is 'BH', i.e. Benjamini-Hochberg procedure for controlling the
  FDR. Other possibilities are: `'holm'`, `'hochberg'`, `'hommel'`,
  `'bonferroni'` or `'BY'` (for Benjamini-Yekutieli procedure).

- lowess_span:

  smoother span for the lowess function, between 0 and 1. This gives the
  proportion of points in the plot which influence the smooth at each
  value. Larger values give more smoothness. Only used if
  `which_weights` is `'voom'`. Default is `0.5`.

- R:

  library.size (optional, important to provide if
  `preprocessed = TRUE`). Default is `NULL`

- adaptive:

  a logical flag indicating whether adaptive permutation should be
  performed. Default is `TRUE`

- max_adaptive:

  The maximum number of permutations considered. Default is `64000`

- homogen_traj:

  a logical flag indicating whether trajectories should be considered
  homogeneous. Default is `FALSE` in which case trajectories are not
  only tested for trend, but also for heterogeneity.

- na.rm_dearseq:

  logical: should missing values in `y` (including `NA` and `NaN`) be
  omitted from the calculations? Default is `TRUE`.

## Value

A list with the following elements:

- `which_test`: a character string carrying forward the value of the
  '`which_test`' argument indicating which test was perform (either
  'asymptotic' or 'permutation').

- `preprocessed`: a logical flag carrying forward the value of the
  '`preprocessed`' argument indicating whether the expression data were
  already preprocessed, or were provided as raw counts and transformed
  into log-counts per million.

- `n_perm`: an integer carrying forward the value of the '`n_perm`'
  argument indicating the number of perturbations performed (`NA` if
  asymptotic test was performed).

- `genesets`: carrying forward the value of the '`genesets`' argument
  defining the gene sets of interest (`NULL` for gene-wise testing).

- `pval`: computed p-values. A `data.frame` with one raw for each each
  gene set, or for each gene if `genesets` argument is `NULL`, and with
  2 columns: the first one '`rawPval`' contains the raw p-values, the
  second one contains the FDR adjusted p-values (according to the
  '`padjust_methods`' argument) and is named '`adjPval`'.

## References

Gauthier M, Agniel D, Thiébaut R & Hejblum BP (2020). dearseq: a
variance component score test for RNA-Seq differential analysis that
effectivelycontrols the false discovery rate, *NAR Genomics and
Bioinformatics*, 2(4):lqaa093.
[doi:10.1093/nargab/lqaa093](https://doi.org/10.1093/nargab/lqaa093)

## See also

[`sp_weights`](https://borishejblum.github.io/dearseq/reference/sp_weights.md)
[`vc_test_perm`](https://borishejblum.github.io/dearseq/reference/vc_test_perm.md)
[`vc_test_asym`](https://borishejblum.github.io/dearseq/reference/vc_test_asym.md)
[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)

## Examples

``` r

#Monte-Carlo estimation of the proportion of DE genes over `nsims`
#simulations under the null

#number of runs
nsims <- 2 #100
res <- numeric(nsims)
for(i in 1:nsims){
 n <- 1000 #number of genes
 nr=5 #number of measurements per subject (grouped data)
 ni=50 #number of subjects
 r <- nr*ni #number of measurements
 t <- matrix(rep(1:nr), ni, ncol=1, nrow=r) # the variable to be tested
 sigma <- 0.5
 b0 <- 1

 #under the null:
 b1 <- 0

 #create the matrix of gene expression
 y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
 y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
          matrix(rep(y.tilde, n), ncol=n, nrow=r))

 #no covariates
 x <- matrix(1, ncol=1, nrow=r)

 #run test
 #asymptotic test with preprocessed grouped data
 res_genes <- dear_seq(exprmat=y, covariates=x, variables2test=t,
                       sample_group=rep(1:ni, each=nr),
                       which_test='asymptotic',
                      which_weights='none', preprocessed=TRUE)

 #proportion of raw p-values>0.05
 mean(res_genes$pvals[, 'rawPval']>0.05)

 #quantiles of raw p-values
 quantile(res_genes$pvals[, 'rawPval'])

 #proportion of raw p-values<0.05 i.e. proportion of DE genes
 res[i] <- mean(res_genes$pvals[, 'rawPval']<0.05)
 message(i)
}
#> 1
#> 2

#results
mean(res)
#> [1] 0.027

if(interactive()){
b0 <- 1
#under the null:
b1 <- 0

#create the matrix of gene expression
y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
      matrix(rep(y.tilde, n), ncol=n, nrow=r))

#run test
#asymptotic test with preprocessed grouped data
res_genes <- dear_seq(exprmat=y, covariates=x, variables2test=t,
                   sample_group=rep(1:ni, each=nr),
                   which_weights='none', preprocessed=TRUE)

#results
summary(res_genes$pvals)
}
```
