# Create table with Rasch dichotomous model item fit values for each item.

**\[deprecated\]**

## Usage

``` r
RIitemfitRM(
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
  method = "conditional"
)
```

## Arguments

- dfin:

  Dataframe with item data only

- samplesize:

  Desired sample size in multisampling (recommended range 250-500)

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

  Set font size for table

- fontfamily:

  Set font family for table

- output:

  Defaults to output a table. Optional "dataframe" or "quarto"

- tbl_width:

  Set table width in percent

- method:

  Defaults to "conditional". Optional "unconditional"

## Details

Defaults to using conditional estimates for MSQ values (Müller, 2020)
estimated using the `iarm` package. Use `method = "unconditional"` for
the "old" unconditional MSQ values (using `eRm`).

ZSTD is inflated with large samples (N \> 500). Optional function to
reduce sample size and run analysis using multiple random samples to get
average ZSTD If you are using Quarto/Rmarkdown, "cache: yes" will be a
useful chunk option to speed things up. 50 samples seems to give stable
output, but 4-8 is probably sufficient for a quick look at the
approximate ZSTD statistics. It is recommended to use sample size
200-500, based on Hagell & Westergren, 2016.
