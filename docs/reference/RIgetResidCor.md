# Get simulation based cutoff values for Yen's Q^3^ residual correlations

Based on Christensen et al. (2017, DOI: 10.1177/0146621616677520), uses
package `mirt` (Chalmers, 2012) to assess local dependence.

## Usage

``` r
RIgetResidCor(data, iterations = 500, cpu = 4, seed = 123)
```

## Arguments

- data:

  Dataframe with response data

- iterations:

  Number of simulation iterations (needed)

- cpu:

  Number of CPU cores to use (4 is default)

- seed:

  For random number generation and reproducibility

## Details

Uses a dataframe with response data to simulate residual correlation
values across n simulations based on estimated item & person locations.

Results include mean, max and difference between the mean and max for
each iteration. Also, 95th, 99th, 99.5th and 99.9th percentile values
are calculated for use with
[`RIresidcorr()`](https://pgmj.github.io/easyRasch/reference/RIresidcorr.md)
as cutoff value, since the max value may be spurious and dependent on
number of iterations.

Uses multi-core processing. To find how many cores you have on your
computer, use
[`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
Remember to keep 1-2 cores free.
