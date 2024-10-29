# easyRasch 0.3

- package name changed from `RISEkbmRasch` to `easyRasch`.

# easyRasch 0.2.4.6

- `RIpartgamDIF()` added for convenient use of `iarm::partgam_DIF()` to assess DIF.
- `RIpartgamLD()` added for convenient use of `iarm::partgam_LD()` to assess local dependence.
- `RImissing()` and `RImissingP()` now return a message (not a plot) if no data is missing.

# easyRasch 0.2.4.5

- Changed caption text in `RIresidcorr()` to be more grammatically correct.
- Bug fix for `RItargeting()` and `RIitemparams()` to use `max(na.rm = TRUE)`.

# easyRasch 0.2.4.4

- New data pre-analysis check `RIcheckdata()` to determine whether there are at least 3 responses in each cell (item response category) 
  - implemented for `RItargeting()` and `RIitemparams()`
  - if there are fewer than 3 responses in any cell `mirt` will be used to estimate item threshold locations, since it is less prone to extreme values under these conditions than `eRm`.
  - if there are fewer than 3 responses in any cell a warning message will appear
  
# easyRasch 0.2.4.3

- New function `RIrestscore()`, a wrapper function to simplify output from `iarm::item_restscore()`.
- Fix for `RIitemhierarchy()`, where `na.rm = TRUE` was omitted from `rowMeans()` leading to no mean location for items with less thresholds than others.
- Fix for `RIresidcorr()` to make conditional highlighting work for any values. 
  - Added option `output = "quarto"` for output of a `knitr::kable()` table (without conditional highlighting).

# easyRasch 0.2.4.2

- `RIitemfit()` now consistently states that conditional item fit is based on complete cases only in the automatic caption text.
- `RIitemfit()` has a new option for conditional highlighting of misfit based on rule-of-thumb values for infit MSQ according to Smith et al. (1998), since Müller (2020) showed that these can be fairly accurate for conditional infit and thus useful for a quick look at item fit.
- `RIgetfit()` now defaults to use the same sample size that the conditional item fit function uses, which means only complete cases. There is an option to change this behavior in the simulation function.

# easyRasch 0.2.4.1

* New function - `RIpboot()` generates datasets using parametric bootstrapping.

## Bug fix:

- `RIgetfitPlot()` fix for when/if the first iteration of simulations has missing data

## Known issue:

- `RIgetfit()` and `RIgetResidCor()` gets issues upstream with eRm::RM() when badly skewed dichotomous data is used as input.
  - A temporary fix has been implemented, discarding simulated datasets with less than 8 positive responses for any item.

# easyRasch 0.2.4

- `RIgetResidCor()` and `RIgetfit()` now replicate the sample theta distribution accurately using resampling with replacement (parametric bootstrapping based on estimated sample thetas/item thresholds).
- `RIgetResidCor()` and `RIgetfit()` will now omit simulated datasets with empty cells (zero responses in response categories that should have responses).
- `RIgetResidCor()` now automatically chooses PCM or RM depending on data (`model` option removed)
- `RIscoreSE()` has support for dichotomomous data and automatically chooses PCM or RM depending on data 
- `RItileplot()` has a new option for setting `text_color`.

# easyRasch 0.2.3.1

- `RIitemfit()` should finally calculate misfit correctly so that the sorting works
- Changed `RIgetfit()` model estimation function to use `psychotools::PCModel.fit()` to speed up simulations slightly for polytomous models. Like the `eRm::PCM()` function, this also uses Conditional Maximum Likelihood, and produces identical results with `iarm::out_infit()`.

# easyRasch 0.2.3

- `RIgetfit()` now retains the variable/item names from the data.
- `RIitemfit()` now uses conditional highlighting with individual cutoff values for each item.
- `RIitemfit()` outputs two new variables indicating the differences between observed infit/outfit and cutoff threshold values.
- `RIitemfit()` optionally sorts the table output based on misfit per item, using either "infit" or "outfit".
- `RIgetfitPlot()` optionally shows observed item fit in the plot. Ideally, 95% CI would be shown, but the SE output from iarm::out_infit is not reliable according to the author (Müller, 2020), and iarm::boot_fit() does not output SE, only p-values.

# easyRasch 0.2.2.1

- Changed simulation based cutoff thresholds used by `RIitemfit()` to be `quantile(fitmetric, .995)` and .005 instead of .99 and .01 in previous version, to be consistent with `RIgetResidCor()` which uses the one-sided `quantile(fitmetric, .99)`.

# easyRasch 0.2.2

- New `RIitemfit()` function, which replaces both `RIitemfitPCM()` and `RIitemfitRM()`
  - Only outputs conditional MSQ (ZSTD irrelevant for conditional item fit)
  - Automatically uses RM or PCM depending on data structure.
  - Optional conditional highlighting of simulation based cutoff values, and includes the cutoff intervals when using `output = "table"` (default).
- Modified `RIgetfit()` to only use conditional MSQ when running simulations.
  - Automatically uses RM or PCM depending on data structure.
- Modified `RIgetfitPlot()` accordingly.
- Removed `RIgetfitTable()` and `RIgetfitLoHi()` since this information now is included in the output from the new function `RIitemfit()`

# easyRasch 0.2.1.1

- Added 1st and 99th percentiles (upper/lower limits) for simulation based item fit metrics from `RIgetfitTable()`
- `RIitemfitPCM()` and `RIgetfitLoHi()` now use 1st/99th percentile values from simulation as cutoffs.
- `RIgetfitPlot()` now uses these options (see `?ggdist::stat_dotsinterval` for details) for rendering the distribution of simulated+estimated item fit metrics

```
stat_dotsinterval(quantiles = iterations, point_interval = median_qi,
                        layout = "weave", slab_color = NA,
                        .width = c(0.66, 0.99)
```

# easyRasch 0.2.1

- Dichotomous data now working with `RIgetfit()` and `RIgetResidCor()`
  - no integration with `RIitemfitRM()` yet.
- Renamed option `method` to `model` for `RIgetfit()` and `RIgetResidCor()` for consistency across functions.

# easyRasch 0.2.0

## Major update

Implemented two simulation functions to get cutoff values for item fit and 
residual correlations (Yen's Q3). For now, these only work with polytomous (PCM)
data.

As always, documentation is available by using `?function` (without the parentheses otherwise usually included).

## New functions, and brief descriptions:

- `RIgetfit()` - Get simulation based cutoff values for MSQ and ZSTD.
  - `RIgetfitTable()` - Summarises simulation based cutoff values for each item.
  - `RIgetfitPlot()` - Plot (one at a time)
- `RIgetResidCor()` - Get simulation based cutoff values for Yen's Q3 residual correlations
  - Based on Christensen et al. (2017, DOI: 10.1177/0146621616677520).
  - Uses your dataset to get appropriate cutoff values for use with `RIresidcorr()`
  
## Changes:

- `RIitemfitPCM()` now has two new options:
  - `simcut` Set to TRUE if you want to use simulation based cutoff values
  - `gf` The output object from `RIgetfit()` is needed when `simcut = TRUE`
  - example command: `RIitemfitPCMtest(df, simcut = TRUE, gf = getfit)`