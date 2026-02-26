# Temporary fix for upstream bug in `iarm::person_estimates()`

To get
[`RIscoreSE()`](https://pgmj.github.io/easyRasch/reference/RIscoreSE.md)
working properly for cases with theta range up til c(-10,10).

## Usage

``` r
RI_iarm_persons_mle(
  respm,
  thresh,
  model = c("RM", "PCM"),
  theta = rep(0, dim(respm)[1]),
  type = c("MLE", "WLE"),
  extreme = TRUE,
  maxit = 20,
  maxdelta = 3,
  tol = 1e-04,
  maxval = 9
)
```

## Arguments

- respm:

  temp

- thresh:

  temp

- model:

  temp

- theta:

  temp

- type:

  temp

- extreme:

  temp

- maxit:

  temp

- maxdelta:

  temp

- tol:

  temp

- maxval:

  temp

## Details

code from package `iarm`
<https://github.com/cran/iarm/blob/master/R/Person-Fit.R>
