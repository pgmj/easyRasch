# Calculate conditional infit MSQ statistics

Automatically uses RM (dichotomous data) or PCM (polytomous data)
depending on data structure.

## Usage

``` r
RIitemfit(
  data,
  simcut,
  output = "table",
  sort = "items",
  cutoff = c(0.001, 0.999),
  ...
)
```

## Arguments

- data:

  Dataframe with response data

- simcut:

  Object output from
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)

- output:

  Optional "dataframe" or "quarto"

- sort:

  Optional "infit"

- cutoff:

  Default `c(.001,.999)`

- ...:

  Options passed on to
  [`kbl_rise()`](https://pgmj.github.io/easyRasch/reference/kbl_rise.md)
  for table creation

## Details

Uses [`iarm::out_infit()`](https://rdrr.io/pkg/iarm/man/out_infit.html)
to calculate conditional mean square fit statistics for all items. See
Müller (2020, DOI: 10.1186/s40488-020-00108-7) for details. Note: only
uses complete cases! This is explicitly mentioned in the automatic table
caption text.

Cutoff threshold values from simulation data (using option `simcut`) are
used with the [`quantile()`](https://rdrr.io/r/stats/quantile.html)
function with .001 and .999 values to filter out extremes. Actual cutoff
values are shown in the output.

Simulated datasets that have zero responses in any response category
that should have data will automatically be removed/skipped from
analysis, which means that final set of iterations may be lower than
specified by user.

Optional sorting (only) for table output with conditional highlighting
based on simulation cutoff values, `sort = "infit"`.
