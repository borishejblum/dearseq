# Non parametric local heteroscedasticity weights

Computes precision weights that account for heteroscedasticity in
RNA-seq count data based on non-parametric local linear regression
estimates.

## Usage

``` r
sp_weights(
  y,
  x,
  phi = NULL,
  use_phi = TRUE,
  preprocessed = FALSE,
  gene_based = FALSE,
  log_scale_gene_weights = TRUE,
  bw = c("nrd", "ucv", "SJ", "nrd0", "bcv"),
  kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight",
    "tricube", "cosine", "optcosine"),
  transform = TRUE,
  verbose = TRUE,
  na.rm = FALSE
)
```

## Arguments

- y:

  a numeric matrix of size `G x n` containing the raw RNA-seq counts or
  preprocessed expression from `n` samples for `G` genes.

- x:

  a numeric matrix of size `n x p` containing the model covariate(s)
  from `n` samples (design matrix).

- phi:

  a numeric design matrix of size `n x K` containing the K variable(s)
  of interest( e.g. bases of time).

- use_phi:

  a logical flag indicating whether conditional means should be
  conditioned on `phi` and on covariate(s) `x`, or on `x` alone. Default
  is `TRUE` in which case conditional means are estimated conditionally
  on both `x` and `phi`.

- preprocessed:

  a logical flag indicating whether the expression data have already
  been preprocessed (e.g. log2 transformed). Default is `FALSE`, in
  which case `y` is assumed to contain raw counts and is normalized into
  log(counts) per million.

- gene_based:

  a logical flag indicating whether to estimate weights at the
  gene-level. Default is `FALSE`, when weights will be estimated at the
  observation-level.

- log_scale_gene_weights:

  a logical flag when `gene_based = TRUE`, whether to smooth the
  variance estimator on the log scale before exponentiation back,
  guaranteeing positive smoothed variances (unlike smoothing on the raw
  scale, which can extrapolate to negative values). Default is `TRUE`.

- bw:

  a character string indicating the smoothing bandwidth selection method
  to use. See `bandwidth` for details. Possible values are `'ucv'`,
  `'SJ'`, `'bcv'`, `'nrd'` or `'nrd0'`. Default is `'nrd'`.

- kernel:

  a character string indicating which kernel should be used.
  Possibilities are `'gaussian'`, `'epanechnikov'`, `'rectangular'`,
  `'triangular'`, `'biweight'`, `'tricube'`, `'cosine'`, `'optcosine'`.
  Default is `'gaussian'` (NB: `'tricube'` kernel corresponds to the
  loess method).

- transform:

  a logical flag indicating whether values should be transformed to
  uniform for the purpose of local linear smoothing. This may be helpful
  if tail observations are sparse and the specified bandwidth gives
  suboptimal performance there. Default is `TRUE`.

- verbose:

  a logical flag indicating whether informative messages are printed
  during the computation. Default is `TRUE`.

- na.rm:

  logical: should missing values (including `NA` and `NaN`) be omitted
  from the calculations? Default is `FALSE`.

## Value

a list containing the following components:

- `weights`: a matrix `n x G` containing the computed precision weights

- `plot_utilities`: a list containing the following elements:

  - `reverse_trans`: a function encoding the reverse function used for
    smoothing the observations before computing the weights

  - `method`: the weight computation method (`"loclin"`)

  - `smth`: the vector of the smoothed values computed

  - `gene_based`: a logical indicating whether the computed weights are
    based on average at the gene level or on individual observations

  - `mu`: the transformed observed counts or averages

  - `v`: the observed variability estimates

## See also

`bandwidth` [`density`](https://rdrr.io/r/stats/density.html)

## Author

Boris Hejblum

## Examples

``` r
set.seed(123)

G <- 10000
n <- 12
p <- 2
y <- sapply(1:n, FUN = function(x) {
  rnbinom(n = G, size = 0.07, mu = 200)
})

x <- sapply(1:p, FUN = function(x) {
  rnorm(n = n, mean = n, sd = 1)
})

w <- sp_weights(y, x, use_phi = FALSE, na.rm = TRUE)
#> 
#> Bandwith computed.
```
