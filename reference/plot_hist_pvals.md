# Plotting raw p-values histogram

Display the histogram of raw p-values for diagnostic plots

## Usage

``` r
plot_hist_pvals(pvals, binwidth = 0.02)
```

## Arguments

- pvals:

  a vector of raw p-values

- binwidth:

  a value specifying the width of the histogram bins. Default is `0.02`.

## Value

a [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object

## Author

Boris Hejblum

## Examples

``` r
#generate fake data
n <- 1000 #number of genes
nr=5 #number of measurements per subject (grouped data)
ni=50 #number of subjects
r <- nr*ni #number of measurements
t <- matrix(rep(1:nr), ni, ncol=1, nrow=r) # the variable to be tested 
sigma <- 0.5
x <- matrix(1, ncol=1, nrow=r) #no covariates only intercept
y.tilde <- rnorm(r, sd = sigma)
y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
      matrix(rep(y.tilde, n), ncol=n, nrow=r))

#Run dear_seq()
res_genes <- dear_seq(exprmat=y, covariates=x, variables2test=t,
                   sample_group=rep(1:ni, each=nr),
                   which_test = "asymptotic",
                   which_weights='none', preprocessed=TRUE)

#Plot
plot_hist_pvals(res_genes$pvals$rawPval)
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the dearseq package.
#>   Please report the issue at <https://github.com/borishejblum/dearseq/issues>.

```
