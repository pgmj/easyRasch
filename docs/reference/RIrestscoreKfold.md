# Cross-validation of item-restscore

Creates k random folds using
[`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html);
"V-fold cross-validation (also known as k-fold cross-validation)
randomly splits the data into V groups of roughly equal size (called
"folds"). A resample of the analysis data consists of V-1 of the folds",
see <https://rsample.tidymodels.org/reference/vfold_cv.html>

## Usage

``` r
RIrestscoreKfold(data, k = 5, output = "table")
```

## Arguments

- data:

  Dataframe with item responses

- k:

  Number of folds to use (default is 5)

- output:

  Default `table`, option `dataframe`.

## Details

Each V-1 dataset is used both for calculating item-restscore and
summarises results based on BH adjusted p-values.
