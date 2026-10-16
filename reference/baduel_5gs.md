# Small portion of RNA-seq data from plant physiology study.

A subsample of the RNA-seq data from Baduel *et al.* studying
Arabidopsis Arenosa physiology.

## Usage

``` r
data(baduel_5gs)
```

## Format

3 objects

- `design`: a design matrix for the 48 measured samples, containing the
  following variables:

  - `SampleName` corresponding column names from `expr_norm_corr`

  - `Intercept` an intercept variable

  - `Population` a factor identifying the plant population

  - `Age_weeks` numeric age of the plant at sampling time (in weeks)

  - `Replicate` a purely technical variable as replicates are not from
    the same individual over weeks. Should not be used in analysis.

  - `Vernalized` a logical variable indicating whether the plant had
    undergone vernalization (exposition to cold and short day
    photoperiods)

  - `Vernalized` a binary variable indicating whether the plant belonged
    to the KA population

  - `AgeWeeks_Population` interaction variable between the `AgeWeeks`
    and `Population` variables

  - `AgeWeeks_Vernalized` interaction variable between the `AgeWeeks`
    and `Vernalized` variables

  - `Vernalized_Population` interaction variable between the
    `Vernalized` and `Population` variables

  - `AgeWeeks_Vernalized_Population` interaction variable between the
    `AgeWeeks`, `Vernalized` and `Population` variables

- `baduel_gmt`: a `gmt` object containing 5 gene sets of interest (see
  [`GSA.read.gmt`](https://rdrr.io/pkg/GSA/man/GSA.read.gmt.html)),
  which is simply a `list` with the 3 following components:

  - `genesets`: a `list` of `n` gene identifiers vectors composing
    eachgene set (each gene set is represented as the vector of the gene
    identifiers composing it)

  - `geneset.names`: a vector of length `n` containing the gene set
    names (i.e. gene sets identifiers)

  - geneset.descriptions: a vector of length `n` containing gene set
    descriptions (e.g. textual information on their biological function)

- `expr_norm_corr`: a numeric matrix containing the normalized batch
  corrected expression for the 2454 genes included in either of the 5
  gene sets of interests

## Source

<https://www.ncbi.nlm.nih.gov/bioproject/PRJNA312410>

## References

Baduel P, Arnold B, Weisman CM, Hunter B & Bomblies K (2016).
Habitat-Associated Life History and Stress-Tolerance Variation in
Arabidopsis Arenosa. *Plant Physiology*, 171(1):437-51.
[doi:10.1104/pp.15.01875](https://doi.org/10.1104/pp.15.01875)

Agniel D & Hejblum BP (2017). Variance component score test for
time-course gene set analysis of longitudinal RNA-seq data,
*Biostatistics*, 18(4):589-604.
[doi:10.1093/biostatistics/kxx005](https://doi.org/10.1093/biostatistics/kxx005)
[arXiv:1605.02351](https://arxiv.org/abs/1605.02351).

## Examples

``` r
if(interactive()){
data('baduel_5gs')

set.seed(54321)
KAvsTBG <- dgsa_seq(exprmat=log2(expr_norm_corr+1),
                    covariates=apply(as.matrix(design[,
  c('Intercept', 'Vernalized', 'AgeWeeks', 'Vernalized_Population',
  'AgeWeeks_Population'), drop=FALSE]), 2, as.numeric),
                     variables2test =
                         as.matrix(design[, c('PopulationKA'), drop=FALSE]),
                     genesets=baduel_gmt$genesets[c(3,5)],
                     which_test = 'permutation', which_weights = 'loclin',
                     n_perm=1000, preprocessed = TRUE)

set.seed(54321)
Cold <- dgsa_seq(exprmat=log2(expr_norm_corr+1),
                 covariates=apply(as.matrix(design[,
   c('Intercept', 'AgeWeeks', 'PopulationKA', 'AgeWeeks_Population'),
   drop=FALSE]), 2, as.numeric),
                variables2test=as.matrix(design[, c('Vernalized',
                 'Vernalized_Population')]),
                 genesets=baduel_gmt$genesets[c(3,5)],
                 which_test = 'permutation', which_weights = 'loclin',
                 n_perm=1000, preprocessed = TRUE)
}

```
