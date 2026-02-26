# Person fit with U3 for polytomous data

This function will estimate person parameters with WLE and item
parameters with CML and use these as input to the
[`PerFit::U3poly()`](https://rdrr.io/pkg/PerFit/man/U3poly.html)
function. Since the results from
[`PerFit::U3poly()`](https://rdrr.io/pkg/PerFit/man/U3poly.html) are
inconsistent, the same analysis is iterated 100 times, and the median
proportion of flagged respondents is returned.

## Usage

``` r
RIu3poly(data)
```

## Arguments

- data:

  Dataframe with item responses
