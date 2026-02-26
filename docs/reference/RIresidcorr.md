# Correlation matrix of Rasch residuals using Yen's Q^3^

Uses package `mirt` (Chalmers, 2012).

## Usage

``` r
RIresidcorr(data, cutoff, output = "table", ...)
```

## Arguments

- data:

  Dataframe with item data only

- cutoff:

  Relative value above the average of all item residual correlations

- output:

  Default HTML table, optional "quarto" or "dataframe"

- ...:

  Options sent to
  [`kbl_rise()`](https://pgmj.github.io/easyRasch/reference/kbl_rise.md)
  for table output

## Details

Mandatory option to set relative cutoff-value over the average of all
item residual correlations. It is strongly recommended to use the
function
[`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
to retrieve an appropriate cutoff value for your data.
