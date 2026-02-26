# Person location estimation

Outputs a dataframe of person locations (theta) and measurement error
(SEM) for each person

## Usage

``` r
RIestThetas(data, method = "WLE")
```

## Arguments

- data:

  Dataframe with response data only (no demographics etc), items as
  columns

- method:

  Estimation method (defaults to "WLE")

## Details

IMPORTANT: only use with complete response data. If you have missing
item responses
[`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md)
or
[`RIestThetasOLD()`](https://pgmj.github.io/easyRasch/reference/RIestThetasOLD.md)
is recommended instead.

Uses
[`iarm::person_estimates()`](https://rdrr.io/pkg/iarm/man/person_estimates.html)
to estimate person locations (thetas) for a dataframe with item data as
columns and persons as rows.

Defaults to use WLE estimation (lower bias than MLE, see Warm, 1989;
Kreiner, 2025) and PCM.

Note: If you want to use a pre-specified set of item parameters, please
use
[`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md)
or
[`RIestThetasOLD()`](https://pgmj.github.io/easyRasch/reference/RIestThetasOLD.md).
