# easyRasch 0.4.2.3 (2026-01-13)

- There is a fix for MacOS Tahoe 26.1 removing support for OpenGL, see <https://stackoverflow.com/questions/66011929/package-rgl-in-r-not-loading-in-mac-os/66127391>
  - MacOS users need to add `options(rgl.useNULL = TRUE)` to their scripts before loading `easyRasch`.
- New function `RIu3poly()` since `PerFit::U3poly()` has inconsistent results. The new function runs 100 iterations of `PerFit::U3poly()` and returns the median proportion of flagged respondents.

# easyRasch 0.4.2.2 (2025-12-19)

- `RIdifTable()` and `RIdifTable2()` now both identify RM or PCM from data automatically and uses the correct model.

# easyRasch 0.4.2.1 (2025-12-10)

- `RIitemcols()` now also shows variable name in facet labels, and respects the order of variables in data
- `RIinfitKfoldPlot()` gets some bug fixes for text display.

# easyRasch 0.4.2 (2025-12-01)

- Most simulation functions now produce reproducible results based on your chosen seed.
  - Use `set.seed()` at the start of your analysis document, for instance with `set.seed(473662)` (use a random number). See `?set.seed` for details.
  - Updated functions are: `RIgetfit()`, `RIgetResidcorr()`, `RIbootrestscore()`, `RIbootLRT()`, `RIbootPCA()`,
    `RIgetResidCorG2()`, `RIpboot()`
- `RIbootPCA()` should now be more reliable, quite a bit faster, and also output the number of simulated datasets that contained missing cells and could not be estimated.

# easyRasch 0.4.1.4 (2025-11-19)

- New experimental function `RIcrel()` for conditional reliability estimates based on the Relative Measurement Uncertainty (RMU).
  - Outputs a list object with a table showing mean and 95% HDCI at each ordinal sum score (corresponds to possible thetas)
  - Outputs two figures, one of which is bias-corrected using WLE estimates.
  - *Note that this is an experimental function that has yet to be evaluated.*
- PCA of residuals has added options for max iterations and rotation.
  - If you get a warning message "convergence not obtained in GPFoblq", increase max iterations.
- Minor bug fixes

# easyRasch 0.4.1.3 (2025-10-28)

- New option for `RIreliability()` to set `theta_range`.

# easyRasch 0.4.1.2 (2025-10-24)

- New functionality in `RIreliability()` with setting `iter` that defaults to estimating the RMU 50 times from the same set of draws to achieve better stability in estimates.
  - see <https://pgmj.github.io/reliability.html> for examples and comparisons with other metrics.
  - New option for `RIreliability()` is `pv` to enable users to specify `"TAM"` if desired.
  - `RIreliability()` also has new option `verbose = TRUE`, which can be set to `FALSE` to disable message output.
- New option for `RItargeting()` - `fast_thetas = TRUE` is handy when you have large datasets but don't need exact results. It uses `RIestThetas()`, which is super fast but cannot handle missing data properly (it uses sum scores).

# easyRasch 0.4.1.1 (2025-10-15)

- Changed method for `RIreliability()` plausible values to use package `mirt` instead of `TAM`, leading to 2-5x speedup.
  - New option for `RIreliability()` is `estim` which defaults to use `WLE`, Warm's weighted likelihood estimation for low bias (Warm, 1989). This option is passed to `mirt::fscores()`, see `?mirt::fscores` for options.
  - `mirt::fscores()` uses setting 'MH' to obtain Metropolis-Hastings samples from the posterior.
  - Removed EAP reliability in favor of `mirt::empirical_rxx()` (partly since RMU is very similar to EAP), and added optional non-parametric bootstrap method to get confidence interval for empirical.

# easyRasch 0.4.1 (2025-10-10)

- Added `RIrelRep()` to estimate conditional reliability with alpha or omega
  - Code borrowed from R package `dynamic`, <https://github.com/melissagwolf/dynamic/blob/master/R/RelRep.R>
  - Paper describing the method: McNeish, D., & Dumas, D. (2025). Reliability representativeness: How well does coefficient alpha summarize reliability across the score distribution? Behavior Research Methods, 57(3), 93. https://doi.org/10.3758/s13428-025-02611-8

# easyRasch 0.4.0 (2025-10-10)

- Added `RIreliability()` as a recommended function to estimate several forms of reliability, including RMU with a confidence interval.
  - This function is modified to use plausible values instead of requiring a Bayesian model to be fit. The package `TAM` is used to estimated the Rasch model using MML and to generate plausible values.
  - Code and idea borrowed from: Bignardi, G., Kievit, R., & Bürkner, P.-C. (2025). A general method for estimating reliability using Bayesian Measurement Uncertainty. OSF Preprints. <https://doi.org/10.31234/osf.io/h54k8_v1>
  - NOTE: The `RItif()` function is no longer recommended for use, since TIF is unreliable, particularly with small numbers of items (Milanzi et al., 2015, doi: 10.1111/bmsp.12033)

# easyRasch 0.3.9 (2025-09-22)

- Removed output of outfit MSQ statistics since these are not useful in determining item fit/misfit to the Rasch model (Müller, 2020; Johansson, 2025).
- Added `RIresidcorrG2()` and the corresponding parametric bootstrap to determine a critical value, `RIgetResidCorG2()`, for the G2 metric to evaluate local independence (Chen & Thissen, 1997).
  - Using G^2^ is recommended in addition to Yen's Q^3^ to evaluate local dependence as both have strengths and weaknesses.
- Namespace fix for explicit use of `kableExtra::footnote()` in tables, to avoid issues with `flextable` and potentially other packages.
 
# easyRasch 0.3.8 (2025-09-18)

- Major speedup for `RIbootRestscore()` for PCM (due to moving to `psychotools::PCModel.fit(hessian = FALSE)` instead of `eRm::PCM()`), and minor speed up for RM (due to not calculating SE).
  - quick tests indicate PCM (polytomous data) is now more than twice as fast, and we get ~10% for RM.
- `RIgetfit()` speedup at 10-20% due to not calculating SE/hessian, which is not needed for conditional infit.
- `RIresidcorr()` fix so that the residual correlation matrix is not printed to the console (re-implementing `sink()`).

# easyRasch 0.3.7.1 (2025-09-04)

- `RIinfitKfold()` now uses `output = "raw"` as default, intended for use with `RIinfitKfoldPlot()`. The `output = "figure"` option was removed.
- `RIinfitKfoldPlot()` now has improved interpretability and added information in caption text.

# easyRasch 0.3.7 (2025-08-26)

- `RIrestscoreKfold()` added for cross-validating item-restscore using `rsample::vfold_cv()`. This is experimental, it may well be more informative to use `RIbootrestscore()` instead, even with smaller samples.
- `RIinfitKfold()` now has an option for "figure" output.
- `RIinfitKfoldPlot()` added to make a plot from the "raw" output from `RIinfitKfold()`.
- Another minor bug fix for `RImissingP()`... it should now correctly deal with complete data.


# easyRasch 0.3.6.4 (2025-08-08)

- `RIdifTileplot(data, dif_var)` - new convenience function for checking response distributions split by DIF categories prior to DIF analysis.
- `RIresidcorr()` now also uses accelerator SQUAREM for (very slightly) improved speed.

# easyRasch 0.3.6.3 (2025-08-06)

- Bug fix for `RImissingP()`, which has been broken a while.
- Experimental new function `RIinfitKfold()` for cross-validating item fit using `rsample::vfold_cv()`; "V-fold cross-validation (also known as k-fold cross-validation) randomly splits the data into V groups of roughly equal size (called "folds"). A resample of the analysis data consists of V-1 of the folds", see <https://rsample.tidymodels.org/reference/vfold_cv.html>
  - this has not yet been tested so there are no recommendations for how or when to use it.
- Slight speedup (~20%) for `RIgetResidCor()` by changing the accelerator used by `mirt()` to SQUAREM and avoiding the use of `sink()`.

# easyRasch 0.3.6.2 (2025-04-30)

- Added an automatic creation of the itemlabels object for `RIitemHierarchy()` and `RIitemcols()` if it is missing.
- Specified the use of `slice()` as `dplyr::slice()` to avoid namespace issues with package `ordinal`.
- Bug fix in `RIbootRestscore()` where `count()` needed to be specified as `dplyr::count()`.

# easyRasch 0.3.6.1 (2025-03-20)

- Accidentally introduced a bug into `RItargeting()` which is now corrected. Additionally, minor breaks were removed from the inverted histogram of item threshold locations and only integers shown on the y axis.
- Changed the default number of iterations for `RIbootPCA()` to 200.

# easyRasch 0.3.6 (2025-03-19)

- New function `RIbootPCA()` to determine critical value for PCA of residuals' largest expected eigenvalue that supports unidimensionality.
  - default setting is 1000 iterations and 4 cpu cores. Have not yet done a simulation study to know which percentile of the results is recommended as a critical value, but 99% is probably a reasonable starting point.
- While `RIgetfit()` defaults to remove respondents with missing data to ensure comparability with the conditional item fit that is estimated only by respondents with complete responses, it is possible to not remove missing respondents. In order to make this work for dichotomous data, item estimation and theta estimation with WLE is now implemented using functions from `mirt`. This means that MML is used for item parameters rather than CML. If respondents with missing data are removed (default), CML and WLE is used.


# easyRasch 0.3.5 (2025-02-28)

- New function `RIciccPlot()` which is a wrapper for `iarm::ICCplot()`. The function simplifies getting output from all items in the dataframe.
  - NOTE: this can also be used for DIF analysis, using two options, `dif = "yes"` and `dif_var = your$difvariable`.
  - If you use Quarto, you may need to play with code chunk settings for `fig-height` and `fig-width` to get the output right.
- New function `RIitemcols()` for a figure showing each item's response distribution and n + % of responses in each category.
  - If you use Quarto, you will likely need to increase `fig-height` to something above the default setting of 5.
- Added the number of items analyzed to the `RIbootRestscore()` caption text.

# easyRasch 0.3.4.1 (2025-01-23)

- `RIgetResidCor()` bug fix for estimation of theta values with dichotomous data, and for estimation of thetas with missing data.

# easyRasch 0.3.4 (2025-01-20)

- **New function:** `RIbootLRT()` since there may be issues with large samplesizes causing type-1 errors for the global fit conditional likelihood ratio test.
- `RIbootRestscore()` has been reworked a bit:
  - Results are filtered to show only misfit (unless output is `dataframe`).
  - Results are sorted on underfit/overfit and percent of misfit.
- Cleaned and updated the `RIrmPCA()` function.
- **NOTE:** a simulation paper evaluating bootstrap functions for conditional item fit and item-restscore has been made available as a preprint: <https://pgmj.github.io/rasch_itemfit/>

# easyRasch 0.3.3.2

- `RIitemfit()` now shows item location relative to sample mean theta.
- `RIgetfitPlot()` now uses `quantile()` to calculate values for the black horizontal lines to align with `RIitemfit()` cutoff values.
- Since I still haven't had time to fix the issues with `RIestThetasCATr()`, `RIrestscore()` and `RIitemfit()` both use `eRm::person.parameter()` to estimate person locations for the relative item locations variable. This change was made since `RIestThetas()` does not handle missing item responses properly.

# easyRasch 0.3.3.1

- `RIestThetasCATr()` still has bugs with dichotomous data (RM models). It will now stop if used with such data.
  - functions using `RIestThetasCATr()` have been temporarily modified to use `RIestThetas()` for RM data. These functions are `RIrestscore()` and `RIitemfit()`

# easyRasch 0.3.3

- `RIbootRestscore()` now also displays conditional MSQ infit and relative item location in "table" output.
  - also, a new option `cutoff = 5`, which filters to only include rows with percentage of results above the cutoff.
- Since `RIestThetas()` does not handle incomplete responses as expected, an error message will now be produced if one has incomplete responses in data and uses the `RIestThetas()` function, recommending the use of the `catR::thetaEst()`-based function `RIestThetasCATr()` instead.
- `RIestThetas()` now automatically chooses model between RM and PCM.
- The "old" functions for estimating person locations/thetas have been renamed from `RIestThetasOLD()` and `RIestThetasOLD2()` to only `RIestThetasCATr()`.
  - The updated `RIestThetasCATr()` function now works with both polytomous (PCM) and dichotomous data, and automatically chooses between PCM and RM.
  - `RIestThetasCATr()` defaults to use multiple cores, `cpu = 4`. Set this value to something appropriate for your computer, such as `parallel::detectCores() -1`.

# easyRasch 0.3.2

- New function `RIbootRestscore()`, particularly intended for those working with sample sizes of ~ 500 and up.
  - more details in [documentation on this site](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.html) or by using `?RIbootRestscore` in your console after installing the new version.

# easyRasch 0.3.1.2

- Bug fix for `RIgetfit()` and `RIgetResidCor()` when using PCM that could result in all simulations returning empty.
- `RIrestscore()` has an additional output column indicating the item location relative to person mean location to better reflect targeting properties.

# easyRasch 0.3.1.1

- `RIgetfitPlot()` now uses the same `c(.001,.999)` intervals as `RIitemfit()`.

# easyRasch 0.3.1

- `RIpartgamLD()` no longer shows negative gamma values.
- `RIitemfit()`: new option to set upper/lower cutoff values when calculating quantile cutoff values for conditional highlighting based on simulations from `RIgetfit()`
  - new default setting for `cutoff` is `c(.001,.999)`. The old default was `c(.005,.995)`, which according to preliminary simulation studies resulted in increased rates of false positives.
- `RIestThetas()`: added note in documentation that it is not advisable to use this function with incomplete response data.
  - `RIestThetasOLD()` or `RIestThetasOLD2()` are recommended when there are missing responses for some items for some respondents.
- `RItargeting(model = "RM")` now sorts items according to order in data.
- `RIrestscore()` and `RIitemfit()` now also output a column with item (average for polytomous items) location. This is due to preliminary simulation studies indicating that misfitting items > 1.5 logits from person mean require a larger sample to identify reliably.

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
