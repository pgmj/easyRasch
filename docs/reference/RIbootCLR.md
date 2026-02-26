# Bootstrapped Likelihood Ratio Test

Non-parametric bootstrap use of
[`iarm::clr_tests()`](https://rdrr.io/pkg/iarm/man/clr_tests.html).

## Usage

``` r
RIbootCLR(dat, iterations = 250, samplesize = 500, cpu = 4)
```

## Arguments

- dat:

  A dataframe with response data

- iterations:

  How many bootstrap samples to run

- samplesize:

  How large sample to use in each bootstrap

- cpu:

  How many CPU's to use
