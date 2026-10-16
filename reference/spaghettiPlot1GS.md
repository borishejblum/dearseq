# Spaghetti plot for Specific Gene Set

Spaghetti plot for Specific Gene Set

## Usage

``` r
spaghettiPlot1GS(
  gs_index,
  gmt,
  expr_mat,
  design,
  var_time,
  var_indiv,
  sampleIdColname,
  var_group = NULL,
  var_subgroup = NULL,
  plotChoice = c("Medians", "Individual"),
  loess_span = 0.75
)
```

## Arguments

- gs_index:

  index of the specific gene set in `gmt`.

- gmt:

  a `list` of elements: `geneset`, `geneset.name` and
  `geneset.description` (see
  [`GSA.read.gmt`](https://rdrr.io/pkg/GSA/man/GSA.read.gmt.html)).

- expr_mat:

  a `data.frame` with numerics of size `G x n` contraining the raw
  RNA-seq counts from `n` samples for `G` genes.

- design:

  a `data.frame` or `DFrame` containing the information of each sample
  (SampleID).

- var_time:

  the `time` or `visit` variable contained in `design`.

- var_indiv:

  the patient variable contained in `design` data.

- sampleIdColname:

  a character string indicating the name of the sample ID variable in
  `design` to be matched with the `colnames` of `expr_mat`

- var_group:

  a group variable in `design` data to divide into two facets. Default
  is `NULL`.

- var_subgroup:

  a subgroup variable in `design` data to add 2 curves on plot for each
  subgroup. Default is `NULL`.

- plotChoice:

  to choose which type of plot (either `"Medians"`, `"Individual"` or
  both). Default is `c("Medians", "Individual")`.

- loess_span:

  smoothing span. Default is `0.75`.

## Value

a ggplot2 plot object

## Examples

``` r
data(baduel_5gs) 
design$Indiv <- design$Population:design$Replicate
design$Vern <- ifelse(design$Vernalized, "Vernalized", "Non-vernalized")

library(ggplot2)
spaghettiPlot1GS(gs_index = 3, gmt = baduel_gmt, expr_mat = log2(expr_norm_corr+1), 
  design = design, var_time = AgeWeeks, var_indiv = Indiv, 
  sampleIdColname = "sample", var_group=Vern, var_subgroup=Population, 
  plotChoice = "Medians", loess_span= 1.5) +
  xlab("Age (weeks)") + guides(color= "none")
```
