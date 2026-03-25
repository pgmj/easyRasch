# DIF PCM analysis with table output for item thresholds

Makes use of the eRm package function
[`LRtest()`](https://rdrr.io/pkg/eRm/man/LRtest.html). Outputs a table
with item average locations, group differences, and standard errors.

## Usage

``` r
RIdifThreshTblLR(dfin, dif.var, fontfamily = "sans-serif", cutoff = 0.5)
```

## Arguments

- dfin:

  Dataframe with item data only

- dif.var:

  DIF variable

- fontfamily:

  Set table font

- cutoff:

  Cutoff in item location logit difference for table highlighting

## Details

DIF variables need to be factors with the same length as the number of
rows in the dataset.

sample usage: RIdifTableThreshE(df, dif.age, fontfamily = "Arial")
