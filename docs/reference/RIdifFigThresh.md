# Create a DIF line graph for item PCM thresholds

Produces a panel of linegraphs showing item thresholds over DIF nodes.

## Usage

``` r
RIdifFigThresh(dfin, dif.var)
```

## Arguments

- dfin:

  Dataframe with item data only

- dif.var:

  DIF variable

## Details

NOTE: only works with PCM data where all variables have multiple
thresholds, since the `threshpar()` function has problems when
dichotomous data are included.
