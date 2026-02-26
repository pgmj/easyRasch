# Correlation matrix of Rasch residuals using G^2^

Based on Chen & Thissen (2007, DOI: 10.2307/1165285), uses package
`mirt` (Chalmers, 2012).

## Usage

``` r
RIresidcorrG2(data, cutoff, output = "table", ...)
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
[`RIgetResidCorG2()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCorG2.md)
to retrieve an appropriate cutoff value for your data.
