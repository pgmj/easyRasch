# Person fit

Outputs a histogram of person fit ZSTD and a plot with person fit ZSTD
and person location/score. Defaults to output a histogram and a hex
heatmap.

## Usage

``` r
RIpfit(
  dfin,
  model = "PCM",
  pointsize = 2.5,
  alpha = 0.5,
  bins = 30,
  group,
  output = c("hist", "heatmap"),
  infit_lim = c(-1.96, 1.96)
)
```

## Arguments

- dfin:

  Dataframe with item data only

- model:

  Rasch model to use, "PCM" or "RM"

- pointsize:

  Size of datapoints for grouped view

- alpha:

  Transparency of points (0-1 where 1 = not transparent)

- bins:

  Number of bins for hexplot

- group:

  Optional grouping variable

- output:

  Can also be "rowid" for a dataframe with rownumbers

- infit_lim:

  Lower/upper limit for person infit ZSTD

## Details

Optional grouped output with colorized points.

You can also get a vector with row numbers for persons with infit ZSTD
over/under +/- 1.96 by using `output = "rowid"`. Or the full dataframe
with all respondents infit ZSTD and estimated theta values with
`output = "dataframe`.

If you desire another cutoff than +/- 1.96, it can be set with
`infit_lim`.

Note: theta estimation is done using ML, which is not optimal but should
be sufficient for this analysis.
