# A kableExtra function to simplify table creation

A kableExtra function to simplify table creation

## Usage

``` r
kbl_rise(
  data,
  tbl_width = 65,
  fontsize = 14,
  fontfamily = "Arial",
  options = c("striped", "hover"),
  ...
)
```

## Arguments

- data:

  Dataframe/tibble to create table from

- tbl_width:

  Width of table (0-100)

- fontsize:

  Font size

- fontfamily:

  Font family

- ...:

  Passes options to kbl()
