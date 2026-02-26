# Conditional reliability using RMU

Currently experimental function that creates a list object containing
one table and two plots showing the range of plausible values at each
ordinal sum score level (corresponds to possible theta values). Means
and 95% HDCIs are shown in figures and in table. One plot makes mean
adjustments for WL estimate thetas, since the plausible values are based
on EAP estimates. This is similar to the bias adjustment made when
bootstrapping.

## Usage

``` r
RIcrel(
  data,
  draws = 500,
  n = 10,
  conf_level = 0.95,
  estim = "WLE",
  theta_range = c(-6, 6)
)
```

## Arguments

- data:

  Dataframe with item responses

- draws:

  Number of plausible values to generate

- n:

  Number of persons to sample from each sum score level

- conf_level:

  Desired confidence interval (HDCI)

- estim:

  Estimation method for theta (latent scores)

- theta_range:

  The range of possible theta values
