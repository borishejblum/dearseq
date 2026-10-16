# Plotting mean-variance fit for precision weights estimation

Display the variability with respect to the level of expression and the
associated smoothed estimation of precision weights accounting for
heteroscedasticity.

## Usage

``` r
plot_weights(x)
```

## Arguments

- x:

  a list (such as outputed by the functions
  [`sp_weights`](https://borishejblum.github.io/dearseq/reference/sp_weights.md)
  or
  [`voom_weights`](https://borishejblum.github.io/dearseq/reference/voom_weights.md))
  containing the following components:

  - `weights`: a matrix `n x G` containing the estimated precision
    weights

  - `plot_utilities`: a list containing the following elements:

    - `reverse_trans`: a function encoding the reverse function used for
      smoothing the observations before computing the weights

    - `method`: the weight computation method (either `"voom"` or
      `"loclin"`)

    - `smth`: the vector of the smoothed values computed

    - `gene_based`: a logical indicating whether the computed weights
      are based on average at the gene level or on individual
      observations

    - `mu`: the transformed observed counts or averages

    - `v`: the observed variability estimates

## Value

a [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object

## Author

Boris Hejblum

## Examples

``` r
G <- 10000
n <- 12
p <- 2
y <- sapply(1:n, FUN = function(x){rnbinom(n = G, size = 0.07, mu = 200)})
x <- sapply(1:p, FUN = function(x){rnorm(n = n, mean = n, sd = 1)})

if(interactive()){
 w <- sp_weights(y, x, use_phi=FALSE, na.rm = TRUE, gene_based=TRUE)
 plot_weights(w)

 vw <-  voom_weights(y, x)
 plot_weights(vw)
}
```
