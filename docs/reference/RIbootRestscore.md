# Item-restscore bootstrapped

See this simulation study preprint:
https://pgmj.github.io/rasch_itemfit/

## Usage

``` r
RIbootRestscore(
  dat,
  iterations = 200,
  samplesize = 600,
  cpu = 4,
  output = "table",
  cutoff = 5
)
```

## Arguments

- iterations:

  How many bootstrap samples to run

- samplesize:

  How large sample to use in each bootstrap

- cpu:

  How many CPU's to use

- output:

  Optional "dataframe", or "quarto" for
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) output

- cutoff:

  Percentage values below this are not shown in table/quarto output

- data:

  Dataframe with only response data, with 0 as lowest response

## Details

Item-restscore will often indicate false positives (item misfit when it
is not misfitting) if the sample size is above 400 and there is one
truly misfitting item in the data. If there is more than one misfitting
item, false positives can occur at such small sample sizes as n =
150-250 with increasing rates as n goes up.

Conversely, when sample size is below n = 800, the detection rate of
truly misfitting items is below 90%, particularly if misfitting items
have location \> 1.5 logits from the sample mean.

Thus, if one has a large dataset it may be useful to be able to use
non-parametric bootstrapping with replacement to get a more nuanced view
of the probability of items actually being misfit.
