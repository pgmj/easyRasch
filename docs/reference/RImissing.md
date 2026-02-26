# Creates a figure with item missing data descriptives for items

Sample use: `RImissing(df, itemStart = "PSS")`

## Usage

``` r
RImissing(data, itemStart)
```

## Arguments

- data:

  Dataframe/tibble to create table from

- itemStart:

  What your variable names start with, in quotes

## Details

If `itemStart` is missing, the whole dataframe will be used.
