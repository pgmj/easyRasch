# Plot k-fold infit based on raw output

NOTE: currently this function seems to not work on Windows OS.

## Usage

``` r
RIinfitKfoldPlot(kfold)
```

## Arguments

- kfold:

  Object with "raw" output from
  [`RIinfitKfold()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfold.md)
  (default)

## Details

Outputs a figure showing highest and lowest expected values based on all
simulations and cross-validation folds from an object created with
`RIinfitKfold(data)`.
