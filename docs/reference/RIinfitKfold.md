# Cross-validation of conditional item infit

Creates k random folds using
[`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html);
"V-fold cross-validation (also known as k-fold cross-validation)
randomly splits the data into V groups of roughly equal size (called
"folds"). A resample of the analysis data consists of V-1 of the folds",
see <https://rsample.tidymodels.org/reference/vfold_cv.html>

## Usage

``` r
RIinfitKfold(
  data,
  k = 5,
  output = "raw",
  sim_iter = 100,
  sim_cpu = 4,
  cutoff = c(0.001, 0.999)
)
```

## Arguments

- data:

  Dataframe with item responses

- k:

  Number of folds to use (default is 5)

- output:

  Default `raw`, options `dataframe`, `table`

- sim_iter:

  Number of iterations (depends on sample size)

- sim_cpu:

  Number of CPU cores to use

- cutoff:

  Truncation at percentile values (see
  [`?RIitemfit`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md))

## Details

Each V-1 dataset is used both for calculating item fit and expected item
fit critical values (using
[`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)).
If `output = "table"` (default), results are summarized indicating upper
and lower bounds for each item's calculated infit and simulated expected
range. This is based on all V-1 fold combinations.
