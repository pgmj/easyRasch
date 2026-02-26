# Parametric bootstrap procedure for PCA of Rasch model residuals

Estimates item and person parameters from data and simulates data
fitting the RM or PCM, then estimates the largest eigenvalue from
residuals.

## Usage

``` r
RIbootPCA(
  data,
  iterations = 200,
  cpu = 4,
  rotation = "oblimin",
  maxiter = 5000
)
```

## Arguments

- data:

  Dataframe with item responses

- iterations:

  Number of bootstrap iterations

- cpu:

  Number of CPU cores to use

- rotation:

  Defaults to "oblimin"

- maxiter:

  Maximum number of iterations

## Details

Outputs an object with complete results under `$results` and percentile
values at 95%, 99%, 99.5%, and 99.9%.
