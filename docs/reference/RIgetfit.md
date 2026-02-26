# Get simulation based cutoff values for item fit values

This function uses your response data to simulate datasets that fit the
Rasch model to find a credible range of item fit values. The function
outputs an object that is strongly recommended to save to an object,
since it takes some time to run this function when using many
iterations/simulations.

## Usage

``` r
RIgetfit(data, iterations = 250, cpu = 4, na.omit = TRUE, seed = 123)
```

## Arguments

- data:

  Dataframe with response data

- iterations:

  Number of simulation iterations (use 200-400)

- cpu:

  Number of CPU cores to use

- na.omit:

  Defaults to TRUE to produce conditional fit comparable values

- seed:

  For random number generation and reproducibility

## Details

The output is a list object, which can in turn be used with two
different functions. Most importantly, you can use it with
[`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
to get conditional highlighting of cutoff values based on your sample
size and item parameters. Each item gets its own cutoff thresholds.

The function
[`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md)
uses the package `ggdist` to plot the distribution of fit values from
the simulation results.

Uses multi-core processing. To find how many cores you have on your
computer, use
[`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
Remember to keep 1-2 cores free.

Since version 0.2.4.2, the default is to only use complete cases in the
simulations, since this is what the conditional item fit function uses
and numbers should be more comparable using this method.
