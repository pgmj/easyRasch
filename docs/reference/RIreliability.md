# Reliability metrics

Several metrics are reported, RMU, PSI, and 'empirical'. It is
recommended to also use the function
[`RIrelRep()`](https://pgmj.github.io/easyRasch/reference/RelRep.md) to
evaluate conditional reliability. RMU seems like the main metric to
report.

## Usage

``` r
RIreliability(
  data,
  conf_int = 0.95,
  draws = 1000,
  estim = "WLE",
  boot = FALSE,
  cpu = 4,
  pv = "mirt",
  iter = 50,
  verbose = TRUE,
  theta_range = c(-10, 10)
)
```

## Arguments

- data:

  Dataframe/tibble with only item response data coded as integers

- conf_int:

  Desired confidence interval (HDCI)

- draws:

  Number of plausible values to generate

- estim:

  Estimation method for theta (latent scores)

- boot:

  Optional non-parametric bootstrap for empirical reliability

- cpu:

  Number of cpu cores to use for bootstrap method

- pv:

  Choice of R package. Optional "TAM", requires that you have TAM
  installed

- iter:

  Number of times the RMU estimation is done on the draws

- verbose:

  Set to `FALSE` to avoid the messages

- theta_range:

  The range of possible theta values

## Details

RMU, Relative Measurement Uncertainty: This function uses the `mirt`
library to estimate the Rasch model using Marginal Maximum Likelihood
and then generates plausible values (PVs; Mislevy, 1991). The function
uses borrowed code, see
[`?RMUreliability`](https://pgmj.github.io/easyRasch/reference/RMUreliability.md).

The PVs are then used with the RMU method described by Bignardi et al.
(2025) to estimate a mean and confidence interval. The mean is similar
to the expected a posteriori (EAP) reliability point estimate (Adams,
2005). The confidence interval uses the 95% highest continuous density
interval (HDCI) based on the distribution of correlations.

Default setting is to generate 1000 PVs. More are recommended for stable
estimates/CIs. How many more has not been systematically evaluated, but
4000 might be a good starting point. For smaller samples, more PVs is
not very demanding computationally, but be wary of the time it takes to
create thousands of PVs for each respondent in large samples.

PSI, Person Separation Index: Estimated using functions in the `eRm`
package, see [`?eRm::SepRel`](https://rdrr.io/pkg/eRm/man/SepRel.html).
Note that this excludes min/max scoring individuals, which may result in
unexpected results, especially compared to other methods.

Empirical: Estimated using
[`mirt::empirical_rxx()`](https://philchalmers.github.io/mirt/reference/empirical_rxx.html),
see
<https://stats.stackexchange.com/questions/427631/difference-between-empirical-and-marginal-reliability-of-an-irt-model>

## References

- Bignardi, G., Kievit, R., & Bürkner, P. C. (2025). A general method
  for estimating reliability using Bayesian Measurement Uncertainty.
  PsyArXiv.
  [doi:10.31234/osf.io/h54k8](https://osf.io/preprints/psyarxiv/h54k8)

- Mislevy, R. J. (1991). Randomization-Based Inference about Latent
  Variables from Complex Samples. Psychometrika, 56(2), 177–196.
  [doi:10.1007/BF02294457](https://doi.org/10.1007/BF02294457)

- Adams, R. J. (2005). Reliability as a measurement design effect.
  Studies in Educational Evaluation, 31(2), 162–172.
  [doi:10.1016/j.stueduc.2005.05.008](https://doi.org/10.1016/j.stueduc.2005.05.008)

## Examples

``` r
if (FALSE) { # \dontrun{
# comparison of a fully Bayesian Rasch model and PVs
df <- eRm::raschdat1[,1:20] %>%
  rownames_to_column("id") %>%
  pivot_longer(!id, names_to = "item")

library(brms)
brms_model <- brm(
  value ~ 1 + (1 | item) + (1 | id),
  data    = df,
  chains  = 4,
  cores   = 4,
  family = "bernoulli"
)

posterior_draws <- brms_model %>%
  as_draws_df() %>%
  dplyr::select(starts_with("r_id")) %>%
  t()

RMUreliability(posterior_draws)
RIreliability(eRm::raschdat1[,1:20], draws = 4000)
} # }
```
