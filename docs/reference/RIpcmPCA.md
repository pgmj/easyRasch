# Fits a Rasch PCM using eRm, and conducts a PCA of residuals to get eigenvalues using `psych::pca()` and reports the top 5 values.

Proportion of explained variance is calculated using
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html).

## Usage

``` r
RIpcmPCA(
  dfin,
  output = "table",
  fontsize = 15,
  maxiter = 5000,
  rotation = "oblimin"
)
```

## Arguments

- dfin:

  Dataframe with item data only

- output:

  Defaults to "table", optional "dataframe"

- fontsize:

  Set font size for table

- maxiter:

  Maximum number of iterations. Increase if convergence not obtained.

- rotation:

  Defaults to "oblimin"

## Details

Note from [`?psych::pca`](https://rdrr.io/pkg/psych/man/principal.html):
The eigenvectors are rescaled by the sqrt of the eigenvalues to produce
the component loadings more typical in factor analysis.

Possible rotations are: "none", "varimax", "quartimax", "promax",
"oblimin", "simplimax", and "cluster".

See Chou & Wang (2010, DOI: 10.1177/0013164410379322) for a simulation
study testing PCA eigenvalues across multiple conditions.
