# Create table with Rasch PCM model item fit values for each item.

**\[deprecated\]**

## Usage

``` r
RIitemfitPCM2(
  dfin,
  samplesize = 200,
  nsamples = 8,
  cpu = 4,
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

  Desired sample size in multisampling (recommended range 200-500)

- nsamples:

  Desired number of samples (recommended range 8-50)

- cpu:

  Number of CPU cores to utilize (default = 4)

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

## Details

Special version of
[`RIitemfitPCM()`](https://pgmj.github.io/easyRasch/reference/RIitemfitPCM.md)
that utilizes multiple CPU cores to improve performance. Requires
[`library(doParallel)`](https://github.com/RevolutionAnalytics/doparallel).
To find how many cores you have on your computer, use
[`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html),
but remember to keep some cores free.

See documentation for
[`RIitemfitPCM()`](https://pgmj.github.io/easyRasch/reference/RIitemfitPCM.md)
for more complete information.
