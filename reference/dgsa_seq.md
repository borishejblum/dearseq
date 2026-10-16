# Time-course Gene Set Analysis

Wrapper function for performing gene set analysis of (potentially
longitudinal) RNA-seq data

## Usage

``` r
dgsa_seq(
  exprmat = NULL,
  object = NULL,
  covariates = NULL,
  variables2test,
  weights_var2test_condi = (which_test != "permutation"),
  genesets,
  sample_group = NULL,
  cov_variables2test_eff = NULL,
  which_test = c("permutation", "asymptotic"),
  which_weights = c("loclin", "voom", "none"),
  n_perm = 1000,
  progressbar = TRUE,
  parallel_comp = TRUE,
  nb_cores = parallel::detectCores(logical = FALSE) - 1,
  preprocessed = FALSE,
  gene_based_weights = TRUE,
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
  na.rm_gsaseq = TRUE,
  verbose = TRUE
)
```

## Arguments

- exprmat:

  a numeric matrix of size `G x n` containing the raw RNA-seq counts or
  preprocessed expressions from `n` samples for `G` genes. Default is
  `NULL`, in which case `object` must not be `NULL`.

- object:

  an object that can be either an
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

- weights_var2test_condi:

  a logical flag indicating whether heteroscedasticity weights
  computation should be conditional on both the variable(s) to be tested
  `phi` and on covariate(s) `x`, or on `x` alone. Default is `TRUE` for
  the asymptotic test (in which case conditional means are estimated
  conditionally on both `variables2test` and `covariates`), and `FALSE`
  for the permutation test (in which case conditional means are
  estimated conditionally on only the `covariates`).

- genesets:

  Can be either:

  - a `vector`

  - a `list`

  - a `BiocSet` object

  Can be a vector of index or subscripts that defines which rows of `y`
  constitute the investigated gene set (when only 1 gene set is being
  tested).

  Can also be a `list` of index (or `rownames` of `y`) when several gene
  sets are tested at once, such as the first element of a
  [`gmt`](https://rdrr.io/pkg/GSA/man/GSA.read.gmt.html) object.

  Finally, can also be a
  [`BiocSet`](https://rdrr.io/pkg/BiocSet/man/BiocSet-class.html) object

  If `NULL`, then gene-wise p-values are returned.

- sample_group:

  a vector of length `n` indicating whether the samples should be
  grouped (e.g. paired samples or longitudinal data). Coerced to be a
  `factor`. Default is `NULL` in which case no grouping is performed.

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

- preprocessed:

  a logical flag indicating whether the expression data have already
  been preprocessed (e.g. log2 transformed). Default is `FALSE`, in
  which case `y` is assumed to contain raw counts and is normalized into
  log(counts) per million.

- gene_based_weights:

  a logical flag used for `'loclin'` weights, indicating whether to
  estimate weights at the gene-level, or rather at the
  observation-level. Default is `TRUE`, and weights are then estimated
  at the gene-level.

- log_scale_gene_weights:

  a logical flag when `gene_based_weights = TRUE`, whether to smooth the
  variance estimator on the log scale before exponentiation back,
  guaranteeing positive smoothed variances (unlike smoothing on the raw
  scale, which can extrapolate to negative values). Default is `TRUE`.

- bw:

  a character string indicating the smoothing bandwidth selection method
  to use. See `bandwidth` for details. Possible values are `'ucv'`,
  `'SJ'`, `'bcv'`, `'nrd'` or `'nrd0'`

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

  library size (optional, important to provide if
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

- na.rm_gsaseq:

  logical: should missing values in `y` (including `NA` and `NaN`) be
  omitted from the calculations? Default is `TRUE`.

- verbose:

  logical: should informative messages be printed during the
  computation? Default is `TRUE`.

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
  defining the gene sets of interest (`NULL` for gene-wise t esting).

- `pval`: computed p-values. A `data.frame` with one raw for each each
  gene set, or for each gene if `genesets` argument is `NULL`, and with
  2 columns: the first one '`rawPval`' contains the raw p-values, the
  second one contains the FDR adjusted p-values (according to the
  '`padjust_methods`' argument) and is named '`adjPval`'.

## References

Agniel D & Hejblum BP (2017). Variance component score test for
time-course gene set analysis of longitudinal RNA-seq data,
*Biostatistics*, 18(4):589-604.
[doi:10.1093/biostatistics/kxx005](https://doi.org/10.1093/biostatistics/kxx005)
. [arXiv:1605.02351](https://arxiv.org/abs/1605.02351).

Law, C. W., Chen, Y., Shi, W., & Smyth, G. K. (2014). voom: Precision
weights unlock linear model analysis tools for RNA-seq read counts.
*Genome Biology*, 15(2), R29.
[doi:10.1186/gb-2014-15-2-r29](https://doi.org/10.1186/gb-2014-15-2-r29)
.

## See also

[`sp_weights`](https://borishejblum.github.io/dearseq/reference/sp_weights.md)
[`vc_test_perm`](https://borishejblum.github.io/dearseq/reference/vc_test_perm.md)
[`vc_test_asym`](https://borishejblum.github.io/dearseq/reference/vc_test_asym.md)
[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)

## Examples

``` r

nsims <- 2 #500
res_quant <- list()
for(i in 1:nsims){
 n <- 3000
 na <- 2
 arm_size <- 25
 nr <- 2
 r <- na*arm_size*nr
 t <- matrix(rep(1:nr), r/nr, ncol=1, nrow=r)
 a <- matrix(rep(1:na), r/nr, ncol=1, nrow=r)
 sigma <- 0.4
 b0 <- 1

 #under the null:
 b1 <- 0

 #under the alternative
 #b1 <- 1

 y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
 y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
        matrix(rep(y.tilde, n), ncol=n, nrow=r))
 x <- matrix(1, ncol=1, nrow=r)

 #run test
 res <- dgsa_seq(exprmat = y, covariates = x, variables2test = t,
                genesets=lapply(0:99, function(x){x*30+(1:30)}),
                cov_variables2test_eff = matrix(1),
                sample_group = rep(1:(r/nr), each=nr),
                which_test='asymptotic',
                which_weights='none', preprocessed=TRUE)
 length(res$pvals[, 'rawPval'])
 quantile(res$pvals[, 'rawPval'])

 res_quant[[i]] <- res$pvals[, 'rawPval'] #res$pvals[, 'adjPval']

 cat(i, "/", nsims, "\n", sep="")
}
#> 1/2
#> 2/2
mean(unlist(res_quant)<0.05)
#> [1] 0.22
qqplot(y = unlist(res_quant), x=runif(500), ylab="Raw p-values",
      xlab = "Uniform distribution (expected under H0)")
qqline(unlist(res_quant), distribution = qunif, col="red")

#hist(unlist(res_quant)) #plot(density(unlist(res_quant)))
#round(rowMeans(vapply(res_quant, FUN = quantile, FUN.VALUE = rep(1.1, 5))), 3)


if(interactive()){
res_genes <- dgsa_seq(exprmat = y, covariates = x, variables2test = t,
                    genesets = NULL,
                    cov_variables2test_eff = matrix(1),
                    sample_group = rep(1:(r/nr), each=nr),
                    which_test = 'permutation',
                    which_weights = 'none', preprocessed = TRUE,
                    n_perm = 1000, parallel_comp = FALSE)

mean(res_genes$pvals$rawPval < 0.05)
summary(res_genes$pvals$rawPval)
}
```
