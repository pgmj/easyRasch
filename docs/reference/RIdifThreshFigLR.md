# DIF PCM analysis with panel figure output for item thresholds

Makes use of the eRm package function `LRtest()`. Outputs a panel of
figures with item threshold locations and 95% confidence intervals.

## Usage

``` r
RIdifThreshFigLR(dfin, dif.var)
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
