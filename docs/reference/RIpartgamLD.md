# Partial gamma analysis of local dependence

A simple wrapper for
[`iarm::partgam_LD()`](https://rdrr.io/pkg/iarm/man/partgam_LD.html).
Filters results to only show statistically significant relationships and
sorts the table on the absolute value of partial gamma.

## Usage

``` r
RIpartgamLD(data, output = "table")
```

## Arguments

- data:

  A dataframe with response data

- output:

  Defaults to a HTML table, optional "quarto" and "dataframe"

## Details

Conditional highlighting in HTML table output set to partial gamma \>
0.21.
