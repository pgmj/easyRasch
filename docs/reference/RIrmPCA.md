# Fits the Rasch model for dichotomous data using `eRm::RM()`, and conducts a PCA of residuals to get eigenvalues.

See
[`?RIpcmPCA`](https://pgmj.github.io/easyRasch/reference/RIpcmPCA.md)
for more details.

## Usage

``` r
RIrmPCA(
  dfin,
  output = "table",
  fontsize = 15,
  maxiter = 5000,
  rotation = "oblimin"
)
```

## Arguments

- output:

  Optional "dataframe" or "quarto"

- fontsize:

  Set font size

- maxiter:

  Maximum number of iterations. Increase if convergence not obtained.

- rotation:

  Defaults to "oblimin"

- data:

  Dataframe with item data only

## Details

See Chou & Wang (2010, DOI: 10.1177/0013164410379322) for a simulation
study testing PCA eigenvalues across multiple conditions.
