# Person location estimation with parallel processing

Yields about 2-3x speed increase when using 4-8 CPU cores. Requires
[`library(furrr)`](https://github.com/DavisVaughan/furrr)

## Usage

``` r
RIestThetasCATr(
  data,
  itemParams,
  method = "WL",
  cpu = 4,
  theta_range = c(-10, 10)
)
```

## Arguments

- data:

  Dataframe with response data only (no demographics etc), items as
  columns

- itemParams:

  Optional item (threshold) location matrix

- method:

  Estimation method (defaults to `"WL"`)

- cpu:

  Number of CPUs/cores to utilize (default is 4)

- theta_range:

  Range of theta (person location) values

## Details

Outputs a vector of person locations, one for each row in the dataframe.

Uses thetaEst function from catR package to estimate person locations
(thetas) for a dataframe with item data as columns and persons as rows.
Defaults to use WL estimation (lower bias than ML, see Warm, 1989).
