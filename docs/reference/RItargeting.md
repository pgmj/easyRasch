# Targeting, Wright map derivative.

Outputs a figure consisting of three figures with the same scale on top
of each other. At the top is a histogram of Person Locations, with a
dotted line and gray field indicating mean/SD. In the middle is a
similar histogram with Item Thresholds. At the bottom is a figure
showing the individual item thresholds as dots.

## Usage

``` r
RItargeting(
  dfin,
  model = "PCM",
  xlim = c(-4, 4),
  output = "figure",
  bins = 30,
  fast_thetas = FALSE
)
```

## Arguments

- dfin:

  Dataframe with item data only

- model:

  Defaults to "PCM", use "RM" for dichotomous data

- xlim:

  Optionally, set lower/upper limits for x axis

- output:

  Default "figure", or "list" to output 3 figures to a list object

- bins:

  Optionally, set number of bins for histograms

- fast_thetas:

  If you want fast output, but slightly less accurate person locations

## Details

The figure is made up from three figures using library(patchwork). If
desired, you can output the three figures to a list object instead of a
single figure. This allows you to modify each figure (change theming,
colors, etc). You can put together the three figures into one using
patchwork:

`list$p1 / list$p2 / list$p3 + plot_layout(heights = c(1, 1, 1.4))`
