# Exact permutation p-values

Calculates exact p-values for permutation tests when permutations are
randomly drawn with replacement. This implementation is based on
(slightly adapted) the implementation by Belinda Phipson and Gordon
Smyth from the R package `statmod`

## Usage

``` r
permPvals(nPermSupObs, nPermEff, totalPossibleNPerm)
```

## Arguments

- nPermSupObs:

  number of permutations that yielded test statistics at least as
  extreme as the observed data. Can be a vector or an array of values.

- nPermEff:

  number of permutations effectively computed.

- totalPossibleNPerm:

  total number of permutations possible.

## Value

a vector (or an array, similar to `nperm_supobs`) of exact p-values

## References

Phipson B, and Smyth GK (2010). Permutation p-values should never be
zero: calculating exact p-values when permutations are randomly drawn.
*Statistical Applications in Genetics and Molecular Biology*, Volume 9,
Issue 1, Article 39.
<http://www.statsci.org/smyth/pubs/PermPValuesPreprint.pdf>

## See also

statmod::permp

## Author

Belinda Phipson and Gordon Smyth (adapted by Boris Hejblum)

## Examples

``` r
permPvals(10, 100, 1000)
#> [1] 0.1084109
```
