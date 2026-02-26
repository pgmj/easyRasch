# Person location estimation (`catR` version)

Outputs a vector of person locations, one for each row in the dataframe.

## Usage

``` r
RIestThetasOLD(data, itemParams, method = "WL", theta_range = c(-10, 10))
```

## Arguments

- data:

  Dataframe with response data only (no demographics etc), items as
  columns

- itemParams:

  Optional item (threshold) location matrix

- method:

  Estimation method (defaults to "WL")

- theta_range:

  Range of theta (person location) values

## Details

Uses `thetaEst()` function from `catR` package to estimate person
locations (thetas) for a dataframe with item data as columns and persons
as rows. Defaults to use WL estimation (lower bias than ML, see Warm,
1989).

A version for multi-core processing is available as
[`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md).
