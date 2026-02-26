# Reliability of test

**\[deprecated\]**

## Usage

``` r
RItif(dfin, lo = -5, hi = 5, samplePSI = FALSE, cutoff = 3.33, model = "PCM")
```

## Arguments

- dfin:

  Dataframe with item data only

- lo:

  Lower limit of x axis (default = -5)

- hi:

  Upper limit of x axis (default = 5)

- samplePSI:

  Adds information about sample characteristics

- cutoff:

  Caption text will generate information relative to this TIF value

- model:

  Defaults to "PCM", use "RM" for dichotomous data

## Details

NOTE: TIF is not reliable with small numbers of items, and should not be
used as your primary indication of reliability (see doi:
10.1111/bmsp.12033). Please use
[`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
and [`RIrelRep()`](https://pgmj.github.io/easyRasch/reference/RelRep.md)
instead.

Test information shows the reliability curve of the test (not the
sample).

Use option `samplePSI = TRUE` to add graphical and written
representation of the current sample's theta mean/SD, test information
(TIF) mean/SD, and Person Separation Index (PSI). According to Wright &
Stone (1999), PSI is calculated as
\\\frac{\mathrm{SSD}-\mathrm{MSE}}{\mathrm{SSD}}\\, see
[`?eRm::SepRel`](https://rdrr.io/pkg/eRm/man/SepRel.html) for details.
According to Embretson & Reise (2000), PSI = 1 - SEM^2, and TIF =
1/SEM^2, and the values reported in this function are based on sample
average SEM.

For reference: TIF 2.5 corresponds to PSI 0.6 TIF 3.33 -\> PSI 0.7 TIF 5
-\> PSI 0.8 TIF 10 -\> PSI 0.9

## References

Milanzi, et al. (2015). Reliability measures in item response theory:
Manifest versus latent correlation functions.
[doi:10.1111/bmsp.12033](https://doi.org/10.1111/bmsp.12033)
