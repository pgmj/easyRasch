# Item-restscore

A simple wrapper for
[`iarm::item_restscore()`](https://rdrr.io/pkg/iarm/man/item_restscore.html),
adding information about absolute difference in expected and observed
values, and item (average) location.

## Usage

``` r
RIrestscore(data, output = "table", sort, p.adj = "BH")
```

## Arguments

- data:

  A dataframe with response data

- output:

  Defaults to a HTML table, optional "quarto" and "dataframe"

- sort:

  Optional sorting on absolute difference (descending)

- p.adj:

  See `?item_restscore()` for options (BH is default)

## Details

Please note that item-restscore is likely to produce false positives
when sample size is \> 600. It is recommended to use
[`RIbootRestscore()`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md)
with large samples.
