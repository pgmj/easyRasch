# Raw sum score to logit score transformation table & figure

By default displays a table with raw sum scores and their corresponding
logit score and logit standard error. Depends on functions from package
`iarm`.

## Usage

``` r
RIscoreSE(
  data,
  output = "table",
  point_size = 3,
  error_width = 0.5,
  error_multiplier = 1.96,
  ...
)
```

## Arguments

- data:

  Dataframe with item data only

- output:

  Options: "table" (default), "figure", or "dataframe"

- point_size:

  Point size for figure

- error_width:

  Width of error bar ends for figure

- error_multiplier:

  Range of error bars to multiply with SEM

- ...:

  Options for
  [`kbl_rise()`](https://pgmj.github.io/easyRasch/reference/kbl_rise.md)
  for table creation

## Details

Automatically chooses PCM or RM depending on data structure.

Optional figure or dataframe output.

NOTE: the figure uses
[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html),
take this into account if you wish to add theming.
