# DIF PCM analysis with panel figure output for items' average locations

Makes use of the eRm package function
[`LRtest()`](https://rdrr.io/pkg/eRm/man/LRtest.html). Outputs a panel
of figures with item average locations and 95% confidence intervals.

## Usage

``` r
RIdifFigureLR(dfin, dif.var)
```

## Arguments

- dfin:

  Dataframe with item data only

- dif.var:

  DIF variable

## Details

DIF variables need to be factors with the same length as the number of
rows in the dataset.

sample usage: RIdifTableE(df, dif.age)
