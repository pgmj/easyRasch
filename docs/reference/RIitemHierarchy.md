# Create a figure showing items and thresholds (with CI)

Items are sorted by item average location. Confidence intervals are 84%
by default to enable visual interpretation of statistically significant
differences (Payton et al., 2003). The CI can be changed using the
`sem_multiplier` option (ie. use 1.96 for 95% CI).

## Usage

``` r
RIitemHierarchy(dfin, numbers = TRUE, sem_multiplier = 1.405)
```

## Arguments

- dfin:

  Dataframe with item data only

- numbers:

  Display text in figure with item threshold locations

- sem_multiplier:

  For confidence intervals displayed in figure

## Details

Only works with partial credit models currently. For dichotomous data,
use `df.erm <- RM(data)` followed by `plotPImap(df.erm, sorted = T)`
