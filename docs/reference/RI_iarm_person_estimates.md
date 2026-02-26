# Temporary fix for upstream bug in `iarm::person_estimates()`

To get
[`RIscoreSE()`](https://pgmj.github.io/easyRasch/reference/RIscoreSE.md)
working properly for cases with theta range up til c(-10,10).

## Usage

``` r
RI_iarm_person_estimates(
  object,
  properties = F,
  allperson = F,
  sthetarange = c(-10, 10)
)
```

## Arguments

- object:

  Output from PCM() or RM()

- properties:

  All properties or not

- allperson:

  All respondents or not

## Details

code from package `iarm`
<https://github.com/cran/iarm/blob/master/R/Person-Fit.R>
