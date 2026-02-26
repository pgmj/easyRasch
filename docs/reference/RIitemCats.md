# Individual item category probability plots

Uses [`eRm::PCM()`](https://rdrr.io/pkg/eRm/man/PCM.html) and
[`eRm::plotICC()`](https://rdrr.io/pkg/eRm/man/plotICC.html).

## Usage

``` r
RIitemCats(data, items = "all", xlims = c(-6, 6), legend = FALSE)
```

## Arguments

- data:

  Dataframe with item data only

- items:

  Optionally a single item `"q4"`, or a vector `c("q4","q2")`

- xlims:

  Start/end point for x-axis

- legend:

  Set to a position such as "left" if desired
