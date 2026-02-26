# Check response distribution prior to DIF analysis

Outputs a patchwork plot with one
[`RItileplot()`](https://pgmj.github.io/easyRasch/reference/RItileplot.md)
for each level of the DIF variable. Note that continuous DIF variables,
such as age in years, will not work. A limit has been set at 12 levels
of DIF.

## Usage

``` r
RIdifTileplot(data, dif_var)
```

## Arguments

- data:

  Dataframe with item responses

- dif_var:

  DIF variables, ideally a labelled factor

## Details

Plot labels are automatically set based on the DIF variable being a
factor with labels. You can change the patchwork object title/etc as
usual by adding for instance
`+ plot_annotation(title = "Whatever you like")`.
