# Creates a plot with distribution of simulation based item fit values

Uses the output from
[`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
as input. Uses and `.width = c(.66,.99)` with
[`ggdist::stat_dots()`](https://mjskay.github.io/ggdist/reference/stat_dots.html).

## Usage

``` r
RIgetfitPlot(simcut, data, cutoff = c(0.001, 0.999), output = "infit")
```

## Arguments

- simcut:

  Output object from
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)

- data:

  Optional response dataframe for plotting observed item fit

- cutoff:

  Defaults to quantile(.001) and .999 to match
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)

- output:

  Optional setting "both" for infit and outfit
