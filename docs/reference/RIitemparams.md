# Item parameters summary

Displays a table with item threshold locations. Can also output a
dataframe or a CSV file.

## Usage

``` r
RIitemparams(
  dfin,
  fontsize = 15,
  output = "table",
  detail = "thresholds",
  filename = "item_params.csv",
  tbl_width = 90
)
```

## Arguments

- dfin:

  Dataframe with item data only

- fontsize:

  Option to set font size for table

- output:

  Defaults to "table, can be set to "dataframe" or "file"

- detail:

  Set to "all" to get more detailed summary output

- filename:

  Name of file to save output to

- tbl_width:

  Width of table

## Details

Currently only works with the Partial Credit Model (polytomous data).
