# Check dataframe

This functions checks for items with low number of responses in any
category.

## Usage

``` r
RIcheckdata(data, n = 3)
```

## Arguments

- data:

  Font family for all plot text

- n:

  Lowest number of responses in a cell

## Value

Whether there are issues with the data or not (TRUE or FALSE)

## Details

For now, low number is set to \< 3.
