# Create table with Rasch PCM model item fit values for each item.

**\[deprecated\]**

## Usage

``` r
RIitemfitPCM(
  dfin,
  samplesize,
  nsamples,
  zstd_min = -1.96,
  zstd_max = 1.96,
  msq_min = 0.7,
  msq_max = 1.3,
  fontsize = 15,
  fontfamily = "Lato",
  output = "table",
  tbl_width = 65,
  method = "conditional",
  simcut = FALSE,
  gf
)
```

## Arguments

- dfin:

  Dataframe with item data only

- samplesize:

  Desired sample size in multisampling (recommended range 200-500)

- nsamples:

  Desired number of samples (recommended range 10-50)

- zstd_min:

  Lower cutoff level for ZSTD

- zstd_max:

  Upper cutoff level for ZSTD

- msq_min:

  Lower cutoff level for MSQ

- msq_max:

  Upper cutoff level for MSQ

- fontsize:

  Set fontsize for table

- fontfamily:

  Set font family for table

- output:

  Defaults to output a table. Optional "dataframe" or "quarto"

- tbl_width:

  Set table width in percent

- method:

  Defaults to "conditional". Optional "unconditional"

- simcut:

  Set to TRUE if you want to use simulation based cutoff values

- gf:

  The output object from
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  is needed when `simcut = TRUE`

## Details

Defaults to using conditional estimates for MSQ values (Müller, 2020)
estimated using the `iarm` package. Use `method = "unconditional"` for
the "old" unconditional MSQ values (using `eRm`).

Since version 0.2.0 (2024-08-15), it is highly recommended to replace
rule-of-thumb cutoff values with simulation based cutoffs. See details
in `?RIgetfit()` for an easy way to get and set appropriate cutoff
values.

ZSTD is inflated with large samples (N \> 500). There is an optional
function to use a reduced sample size and run analysis using multiple
random samples to get the average ZSTD for each item over all runs.

If you are using Quarto, the YAML execute setting "cache: yes" will be a
useful chunk option to speed things up if you render often. 30-50
samples seems to produce stable output, but 4-8 is probably sufficient
for a quick look at the approximate ZSTD statistics. It is recommended
to use sample size 200-500, based on Hagell & Westergren (2016) & Müller
(2020).
