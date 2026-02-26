# DIF analysis dichotomous - requires having set up dif.variables

Makes use of the psychotree package, which also allows for interactions
between DIF variables, which is not implemented in this function (yet).

## Usage

``` r
RIdifTableRM(dfin, dif.var, cutoff = 0.5)
```

## Arguments

- dfin:

  Dataframe with item data only

- dif.var:

  DIF variable

- cutoff:

  Cutoff in item location logit difference for table highlighting

## Details

DIF variables need to be vectors with the same length as the number of
rows in the dataset.

sample usage: RIdifTable(df, dif.age)
