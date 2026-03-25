# DIF PCM analysis with table output for item locations

Makes use of the eRm package function
[`LRtest()`](https://rdrr.io/pkg/eRm/man/LRtest.html). Outputs a table
with item average locations, group differences, and standard errors.

## Usage

``` r
RIdifTableLR(
  dfin,
  dif.var,
  model = "PCM",
  sort = FALSE,
  fontfamily = "sans-serif",
  cutoff = 0.5
)
```

## Arguments

- dfin:

  Dataframe with item data only

- dif.var:

  DIF variable

- model:

  Defaults to "PCM", optional "RM" under development

- sort:

  Set to TRUE to sort the table based on DIF size

- fontfamily:

  Set table font

- cutoff:

  Cutoff in item location logit difference for table highlighting

## Details

DIF variables need to be factors with the same length as the number of
rows in the dataset.

sample usage: RIdifTableE(df, dif.age)
