# Precision weights accounting for heteroscedasticity in RNA-seq count data

Implementation of the procedure described in Law *et al.* for estimating
precision weights from RNA-seq data.

## Usage

``` r
voom_weights(y, x, preprocessed = FALSE, lowess_span = 0.5, R = NULL)
```

## Arguments

- y:

  a matrix of size `G x n` containing the raw RNA-seq counts or
  preprocessed expressions from `n` samples for `G` genes.

- x:

  a matrix of size `n x p` containing the model covariates from `n`
  samples (design matrix).

- preprocessed:

  a logical flag indicating whether the expression data have already
  been preprocessed (e.g. log2 transformed). Default is `FALSE`, in
  which case `y` is assumed to contain raw counts and is normalized into
  log(counts) per million.

- lowess_span:

  smoother span for the lowess function, between 0 and 1. This gives the
  proportion of points in the plot which influence the smooth at each
  value. Larger values give more smoothness. Default is `0.5`.

- R:

  library.size (optional, important to provide if
  `preprocessed = TRUE`). Default is `NULL`

## Value

a vector of length `n` containing the computed precision weights

## References

Law, C. W., Chen, Y., Shi, W., & Smyth, G. K. (2014). voom: Precision
weights unlock linear model analysis tools for RNA-seq read counts.
*Genome Biology*, 15(2), R29.

## See also

[`lowess`](https://rdrr.io/r/stats/lowess.html)
[`approxfun`](https://rdrr.io/r/stats/approxfun.html)
[`voom`](https://rdrr.io/pkg/limma/man/voom.html)

## Author

Boris Hejblum

## Examples

``` r
set.seed(123)

G <- 10000
n <- 12
p <- 2

y <- sapply(1:n, FUN=function(x){rnbinom(n=G, size=0.07, mu=200)})
x <- sapply(1:p, FUN=function(x){rnorm(n=n, mean=n, sd=1)})

my_w <-  voom_weights(y, x)
plot_weights(my_w)

if (requireNamespace('limma', quietly = TRUE)) {
 w_voom <- limma::voom(counts=y, design=x, plot=TRUE)
 #slightly faster, same results
 all.equal(my_w$weights, w_voom$weights)
}

#> [1] "Mean relative difference: 0.05814798"

if(interactive()){
#microbenchmark::microbenchmark(limma::voom(counts=t(y), design=x,
#                               plot=FALSE), voom_weights(x, y),
#                               times=30)
}
```
