# DIF PCM interaction analysis

Makes use of the psychotree package. This function is for interaction
between two DIF variables

## Usage

``` r
RIdifTable2(dfin, dif.var1, dif.var2, cutoff = 0.5)
```

## Arguments

- dfin:

  Dataframe with item data only

- cutoff:

  Cutoff in item location logit difference for table highlighting

- dif.var:

  DIF variable

## Details

DIF variables need to be vectors with the same length as the number of
rows in the dataset.

sample usage: `RIdifTable2(df, dif.age, dif.gender)`
