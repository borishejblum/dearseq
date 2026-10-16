# dearseq: Differential Expression Analysis for RNA-seq data through a robust variance component test

Differential Expression Analysis RNA-seq data with variance component
score test accounting for data heteroscedasticity through precision
weights. Perform both gene-wise and gene set analyses, and can deal with
repeated or longitudinal data. Methods are detailed in: i) Agniel D &
Hejblum BP (2017) Variance component score test for time-course gene set
analysis of longitudinal RNA-seq data, Biostatistics, 18(4):589-604 ;
and ii) Gauthier M, Agniel D, Thiébaut R & Hejblum BP (2020) dearseq: a
variance component score test for RNA-Seq differential analysis that
effectively controls the false discovery rate, NAR Genomics and
Bioinformatics, 2(4):lqaa093. [doi:
10.1093/biostatistics/kxx005](https://doi.org/%2010.1093/biostatistics/kxx005)

## Details

Analysis of RNA-seq data with variance component score test accounting
for data heteroscedasticity through precision weights. Performs
gene-wise analysis as well as gene set analysis, including for complex
experimental designs such as longitudinal data.

|          |                                                  |
|----------|--------------------------------------------------|
| Package: | dearseq                                          |
| Type:    | Package                                          |
| Version: | 1.20.2                                           |
| Date:    | 2025-08-02                                       |
| License: | [GPL-2](http://www.gnu.org/licenses/gpl-2.0.txt) |

The two main functions of the `dearseq` package are
[`dear_seq`](https://borishejblum.github.io/dearseq/reference/dear_seq.md)
and
[`dgsa_seq`](https://borishejblum.github.io/dearseq/reference/dgsa_seq.md).

## References

Agniel D & Hejblum BP (2017). Variance component score test for
time-course gene set analysis of longitudinal RNA-seq data,
*Biostatistics*, 18(4):589-604.
[doi:10.1093/biostatistics/kxx005](https://doi.org/10.1093/biostatistics/kxx005)
[arXiv:1605.02351](https://arxiv.org/abs/1605.02351).

Gauthier M, Agniel D, Thiébaut R & Hejblum BP (2020). dearseq: a
variance component score test for RNA-Seq differential analysis that
effectively controls the false discovery rate, *NAR Genomics and
Bioinformatics*, 2(4):lqaa093.
[doi:10.1093/nargab/lqaa093](https://doi.org/10.1093/nargab/lqaa093) .

## See also

Useful links:

- <https://borishejblum.github.io/dearseq/>

- <https://bioconductor.org/packages/dearseq/>

- Report bugs at <https://github.com/borishejblum/dearseq/issues>

## Author

**Maintainer**: Boris P. Hejblum <boris.hejblum@u-bordeaux.fr>
([ORCID](https://orcid.org/0000-0003-0646-452X))

Authors:

- Denis Agniel <denis.agniel@gmail.com>

- Marine Gauthier <marine.gauthier@u-bordeaux.fr>

Other contributors:

- Mélanie Huchon <melanie.huchon@u-bordeaux.fr> \[contributor\]

- Iban Larrouture <iban.larrouture@u-bordeaux.fr> \[contributor\]
