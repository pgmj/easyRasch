# Partial gamma analysis of Differential Item Functioning

A simple wrapper for
[`iarm::partgam_DIF()`](https://rdrr.io/pkg/iarm/man/partgam_DIF.html).
Filters results to only show statistically significant relationships and
sorts the table on the absolute value of partial gamma.

## Usage

``` r
RIpartgamDIF(data, dif.var, output = "table")
```

## Arguments

- data:

  A dataframe with response data

- dif.var:

  A vector with a DIF variable

- output:

  Defaults to a HTML table, optional "quarto" and "dataframe"

## Details

Conditional highlighting in HTML table output set to partial gamma \>
+/-0.21.
