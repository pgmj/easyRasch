# Changelog

## easyRasch 0.5.1.1 (2026-04-01)

- Restructuring R/ folder to prepare for some structural changes.

## easyRasch 0.5.1 (2026-03-25)

- Bayesian/`brms`-related functions added in previous version are
  removed and now reside in a new separate package,
  [`easyRaschBayes`](https://pgmj.github.io/easyRaschBayes/index.html),
  which is available on CRAN:
  <https://cloud.r-project.org/web/packages/easyRaschBayes/index.html>
- Minor fix for `RIciccplot()` that would surface if package `iarm` was
  not loaded.

## easyRasch 0.5 (2026-02-26)

- Added two functions for assessing item (and person) fit with Bayesian
  Rasch models (RM and PCM) output by the R package `brms`.
  - `fit_statistic_pcm()` and `fit_statistic_rm()`.
  - Code based on [Bürkner,
    2020](http://doi.org/10.3390/jintelligence8010005).
- `infit_statistic()` is a Bayesian version of conditional item infit
  for `brms` models.
- `q3_statistic()` is a Bayesian version of Yen’s Q3 residual
  correlations to evaluate local independence of item pairs for `brms`
  models.
- All new functions for this release are written with Claude Opus 4.6

## easyRasch 0.4.3.2 (2026-02-17)

- Reset multicore/parallel processing to sequential processing before
  returning results from functions with multicore/parallel processing.
  - Functions modified: `RIestThetasCATr`, `RIgetfit`,
    `RIbootRestscore`, `RIbootLRT`, `RIbootPCA`
- Attempt to make an “invisible” workaround for the issues with iarm/rgl
  for MacOS users, which includes temporarily modifying
  `options(rgl.useNULL = TRUE)`
  - Functions modified: `RIitemfit`, `RIgetfit`, `RIrestscore`,
    `RIpartgamLD`, `RIbootRestscore`, `RIinfitKfold`, `RIrestscoreKfold`

## easyRasch 0.4.3.1 (2026-02-05)

- Changed back DESCRIPTION (from previous change), since the Imports
  functionality requires some code rewriting that I don’t have time to
  do right now.
  - `iarm` is under Imports, to enable loading the `easyRasch` package
    without errors.
- Changed the license to GPL-3, since I think this is required due to
  some of the code included.

## easyRasch 0.4.3 (2026-02-03)

- Changes in
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  and
  [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  to use of random number generation to be more in line with how it
  should be done.
  - Added package `doRNG` to package imports/dependencies.
- Changed DESCRIPTION file to list package dependencies under Imports,
  in line with package development guidelines.

## easyRasch 0.4.2.3 (2026-01-13)

- There is a fix for MacOS Tahoe 26.1 removing support for OpenGL, see
  <https://stackoverflow.com/questions/66011929/package-rgl-in-r-not-loading-in-mac-os/66127391>
  - MacOS users need to add `options(rgl.useNULL = TRUE)` to their
    scripts before loading `easyRasch`.
- New function
  [`RIu3poly()`](https://pgmj.github.io/easyRasch/reference/RIu3poly.md)
  since [`PerFit::U3poly()`](https://rdrr.io/pkg/PerFit/man/U3poly.html)
  has inconsistent results. The new function runs 100 iterations of
  [`PerFit::U3poly()`](https://rdrr.io/pkg/PerFit/man/U3poly.html) and
  returns the median proportion of flagged respondents.

## easyRasch 0.4.2.2 (2025-12-19)

- [`RIdifTable()`](https://pgmj.github.io/easyRasch/reference/RIdifTable.md)
  and
  [`RIdifTable2()`](https://pgmj.github.io/easyRasch/reference/RIdifTable2.md)
  now both identify RM or PCM from data automatically and uses the
  correct model.

## easyRasch 0.4.2.1 (2025-12-10)

- [`RIitemcols()`](https://pgmj.github.io/easyRasch/reference/RIitemcols.md)
  now also shows variable name in facet labels, and respects the order
  of variables in data
- [`RIinfitKfoldPlot()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfoldPlot.md)
  gets some bug fixes for text display.

## easyRasch 0.4.2 (2025-12-01)

- Most simulation functions now produce reproducible results based on
  your chosen seed.
  - Use [`set.seed()`](https://rdrr.io/r/base/Random.html) at the start
    of your analysis document, for instance with `set.seed(473662)` (use
    a random number). See
    [`?set.seed`](https://rdrr.io/r/base/Random.html) for details.
  - Updated functions are:
    [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md),
    `RIgetResidcorr()`, `RIbootrestscore()`,
    [`RIbootLRT()`](https://pgmj.github.io/easyRasch/reference/RIbootLRT.md),
    [`RIbootPCA()`](https://pgmj.github.io/easyRasch/reference/RIbootPCA.md),
    [`RIgetResidCorG2()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCorG2.md),
    [`RIpboot()`](https://pgmj.github.io/easyRasch/reference/RIpboot.md)
- [`RIbootPCA()`](https://pgmj.github.io/easyRasch/reference/RIbootPCA.md)
  should now be more reliable, quite a bit faster, and also output the
  number of simulated datasets that contained missing cells and could
  not be estimated.

## easyRasch 0.4.1.4 (2025-11-19)

- New experimental function
  [`RIcrel()`](https://pgmj.github.io/easyRasch/reference/RIcrel.md) for
  conditional reliability estimates based on the Relative Measurement
  Uncertainty (RMU).
  - Outputs a list object with a table showing mean and 95% HDCI at each
    ordinal sum score (corresponds to possible thetas)
  - Outputs two figures, one of which is bias-corrected using WLE
    estimates.
  - *Note that this is an experimental function that has yet to be
    evaluated.*
- PCA of residuals has added options for max iterations and rotation.
  - If you get a warning message “convergence not obtained in GPFoblq”,
    increase max iterations.
- Minor bug fixes

## easyRasch 0.4.1.3 (2025-10-28)

- New option for
  [`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
  to set `theta_range`.

## easyRasch 0.4.1.2 (2025-10-24)

- New functionality in
  [`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
  with setting `iter` that defaults to estimating the RMU 50 times from
  the same set of draws to achieve better stability in estimates.
  - see <https://pgmj.github.io/reliability.html> for examples and
    comparisons with other metrics.
  - New option for
    [`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
    is `pv` to enable users to specify `"TAM"` if desired.
  - [`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
    also has new option `verbose = TRUE`, which can be set to `FALSE` to
    disable message output.
- New option for
  [`RItargeting()`](https://pgmj.github.io/easyRasch/reference/RItargeting.md) -
  `fast_thetas = TRUE` is handy when you have large datasets but don’t
  need exact results. It uses
  [`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md),
  which is super fast but cannot handle missing data properly (it uses
  sum scores).

## easyRasch 0.4.1.1 (2025-10-15)

- Changed method for
  [`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
  plausible values to use package `mirt` instead of `TAM`, leading to
  2-5x speedup.
  - New option for
    [`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
    is `estim` which defaults to use `WLE`, Warm’s weighted likelihood
    estimation for low bias (Warm, 1989). This option is passed to
    [`mirt::fscores()`](https://philchalmers.github.io/mirt/reference/fscores.html),
    see
    [`?mirt::fscores`](https://philchalmers.github.io/mirt/reference/fscores.html)
    for options.
  - [`mirt::fscores()`](https://philchalmers.github.io/mirt/reference/fscores.html)
    uses setting ‘MH’ to obtain Metropolis-Hastings samples from the
    posterior.
  - Removed EAP reliability in favor of
    [`mirt::empirical_rxx()`](https://philchalmers.github.io/mirt/reference/empirical_rxx.html)
    (partly since RMU is very similar to EAP), and added optional
    non-parametric bootstrap method to get confidence interval for
    empirical.

## easyRasch 0.4.1 (2025-10-10)

- Added
  [`RIrelRep()`](https://pgmj.github.io/easyRasch/reference/RelRep.md)
  to estimate conditional reliability with alpha or omega
  - Code borrowed from R package `dynamic`,
    <https://github.com/melissagwolf/dynamic/blob/master/R/RelRep.R>
  - Paper describing the method: McNeish, D., & Dumas, D. (2025).
    Reliability representativeness: How well does coefficient alpha
    summarize reliability across the score distribution? Behavior
    Research Methods, 57(3), 93.
    <https://doi.org/10.3758/s13428-025-02611-8>

## easyRasch 0.4.0 (2025-10-10)

- Added
  [`RIreliability()`](https://pgmj.github.io/easyRasch/reference/RIreliability.md)
  as a recommended function to estimate several forms of reliability,
  including RMU with a confidence interval.
  - This function is modified to use plausible values instead of
    requiring a Bayesian model to be fit. The package `TAM` is used to
    estimated the Rasch model using MML and to generate plausible
    values.
  - Code and idea borrowed from: Bignardi, G., Kievit, R., & Bürkner,
    P.-C. (2025). A general method for estimating reliability using
    Bayesian Measurement Uncertainty. OSF Preprints.
    <https://doi.org/10.31234/osf.io/h54k8_v1>
  - NOTE: The
    [`RItif()`](https://pgmj.github.io/easyRasch/reference/RItif.md)
    function is no longer recommended for use, since TIF is unreliable,
    particularly with small numbers of items (Milanzi et al., 2015, doi:
    10.1111/bmsp.12033)

## easyRasch 0.3.9 (2025-09-22)

- Removed output of outfit MSQ statistics since these are not useful in
  determining item fit/misfit to the Rasch model (Müller, 2020;
  Johansson, 2025).
- Added
  [`RIresidcorrG2()`](https://pgmj.github.io/easyRasch/reference/RIresidcorrG2.md)
  and the corresponding parametric bootstrap to determine a critical
  value,
  [`RIgetResidCorG2()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCorG2.md),
  for the G2 metric to evaluate local independence (Chen & Thissen,
  1997).
  - Using G² is recommended in addition to Yen’s Q³ to evaluate local
    dependence as both have strengths and weaknesses.
- Namespace fix for explicit use of
  [`kableExtra::footnote()`](https://rdrr.io/pkg/kableExtra/man/footnote.html)
  in tables, to avoid issues with `flextable` and potentially other
  packages.

## easyRasch 0.3.8 (2025-09-18)

- Major speedup for
  [`RIbootRestscore()`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md)
  for PCM (due to moving to `psychotools::PCModel.fit(hessian = FALSE)`
  instead of [`eRm::PCM()`](https://rdrr.io/pkg/eRm/man/PCM.html)), and
  minor speed up for RM (due to not calculating SE).
  - quick tests indicate PCM (polytomous data) is now more than twice as
    fast, and we get ~10% for RM.
- [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  speedup at 10-20% due to not calculating SE/hessian, which is not
  needed for conditional infit.
- [`RIresidcorr()`](https://pgmj.github.io/easyRasch/reference/RIresidcorr.md)
  fix so that the residual correlation matrix is not printed to the
  console (re-implementing
  [`sink()`](https://rdrr.io/r/base/sink.html)).

## easyRasch 0.3.7.1 (2025-09-04)

- [`RIinfitKfold()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfold.md)
  now uses `output = "raw"` as default, intended for use with
  [`RIinfitKfoldPlot()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfoldPlot.md).
  The `output = "figure"` option was removed.
- [`RIinfitKfoldPlot()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfoldPlot.md)
  now has improved interpretability and added information in caption
  text.

## easyRasch 0.3.7 (2025-08-26)

- [`RIrestscoreKfold()`](https://pgmj.github.io/easyRasch/reference/RIrestscoreKfold.md)
  added for cross-validating item-restscore using
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html).
  This is experimental, it may well be more informative to use
  `RIbootrestscore()` instead, even with smaller samples.
- [`RIinfitKfold()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfold.md)
  now has an option for “figure” output.
- [`RIinfitKfoldPlot()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfoldPlot.md)
  added to make a plot from the “raw” output from
  [`RIinfitKfold()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfold.md).
- Another minor bug fix for
  [`RImissingP()`](https://pgmj.github.io/easyRasch/reference/RImissingP.md)…
  it should now correctly deal with complete data.

## easyRasch 0.3.6.4 (2025-08-08)

- `RIdifTileplot(data, dif_var)` - new convenience function for checking
  response distributions split by DIF categories prior to DIF analysis.
- [`RIresidcorr()`](https://pgmj.github.io/easyRasch/reference/RIresidcorr.md)
  now also uses accelerator SQUAREM for (very slightly) improved speed.

## easyRasch 0.3.6.3 (2025-08-06)

- Bug fix for
  [`RImissingP()`](https://pgmj.github.io/easyRasch/reference/RImissingP.md),
  which has been broken a while.
- Experimental new function
  [`RIinfitKfold()`](https://pgmj.github.io/easyRasch/reference/RIinfitKfold.md)
  for cross-validating item fit using
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html);
  “V-fold cross-validation (also known as k-fold cross-validation)
  randomly splits the data into V groups of roughly equal size
  (called”folds”). A resample of the analysis data consists of V-1 of
  the folds”, see
  <https://rsample.tidymodels.org/reference/vfold_cv.html>
  - this has not yet been tested so there are no recommendations for how
    or when to use it.
- Slight speedup (~20%) for
  [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  by changing the accelerator used by
  [`mirt()`](https://philchalmers.github.io/mirt/reference/mirt.html) to
  SQUAREM and avoiding the use of
  [`sink()`](https://rdrr.io/r/base/sink.html).

## easyRasch 0.3.6.2 (2025-04-30)

- Added an automatic creation of the itemlabels object for
  [`RIitemHierarchy()`](https://pgmj.github.io/easyRasch/reference/RIitemHierarchy.md)
  and
  [`RIitemcols()`](https://pgmj.github.io/easyRasch/reference/RIitemcols.md)
  if it is missing.
- Specified the use of `slice()` as
  [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html)
  to avoid namespace issues with package `ordinal`.
- Bug fix in
  [`RIbootRestscore()`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md)
  where [`count()`](https://rdrr.io/pkg/matrixStats/man/rowCounts.html)
  needed to be specified as
  [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html).

## easyRasch 0.3.6.1 (2025-03-20)

- Accidentally introduced a bug into
  [`RItargeting()`](https://pgmj.github.io/easyRasch/reference/RItargeting.md)
  which is now corrected. Additionally, minor breaks were removed from
  the inverted histogram of item threshold locations and only integers
  shown on the y axis.
- Changed the default number of iterations for
  [`RIbootPCA()`](https://pgmj.github.io/easyRasch/reference/RIbootPCA.md)
  to 200.

## easyRasch 0.3.6 (2025-03-19)

- New function
  [`RIbootPCA()`](https://pgmj.github.io/easyRasch/reference/RIbootPCA.md)
  to determine critical value for PCA of residuals’ largest expected
  eigenvalue that supports unidimensionality.
  - default setting is 1000 iterations and 4 cpu cores. Have not yet
    done a simulation study to know which percentile of the results is
    recommended as a critical value, but 99% is probably a reasonable
    starting point.
- While
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  defaults to remove respondents with missing data to ensure
  comparability with the conditional item fit that is estimated only by
  respondents with complete responses, it is possible to not remove
  missing respondents. In order to make this work for dichotomous data,
  item estimation and theta estimation with WLE is now implemented using
  functions from `mirt`. This means that MML is used for item parameters
  rather than CML. If respondents with missing data are removed
  (default), CML and WLE is used.

## easyRasch 0.3.5 (2025-02-28)

- New function
  [`RIciccPlot()`](https://pgmj.github.io/easyRasch/reference/RIciccPlot.md)
  which is a wrapper for
  [`iarm::ICCplot()`](https://rdrr.io/pkg/iarm/man/ICCplot.html). The
  function simplifies getting output from all items in the dataframe.
  - NOTE: this can also be used for DIF analysis, using two options,
    `dif = "yes"` and `dif_var = your$difvariable`.
  - If you use Quarto, you may need to play with code chunk settings for
    `fig-height` and `fig-width` to get the output right.
- New function
  [`RIitemcols()`](https://pgmj.github.io/easyRasch/reference/RIitemcols.md)
  for a figure showing each item’s response distribution and n + % of
  responses in each category.
  - If you use Quarto, you will likely need to increase `fig-height` to
    something above the default setting of 5.
- Added the number of items analyzed to the
  [`RIbootRestscore()`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md)
  caption text.

## easyRasch 0.3.4.1 (2025-01-23)

- [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  bug fix for estimation of theta values with dichotomous data, and for
  estimation of thetas with missing data.

## easyRasch 0.3.4 (2025-01-20)

- **New function:**
  [`RIbootLRT()`](https://pgmj.github.io/easyRasch/reference/RIbootLRT.md)
  since there may be issues with large samplesizes causing type-1 errors
  for the global fit conditional likelihood ratio test.
- [`RIbootRestscore()`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md)
  has been reworked a bit:
  - Results are filtered to show only misfit (unless output is
    `dataframe`).
  - Results are sorted on underfit/overfit and percent of misfit.
- Cleaned and updated the
  [`RIrmPCA()`](https://pgmj.github.io/easyRasch/reference/RIrmPCA.md)
  function.
- **NOTE:** a simulation paper evaluating bootstrap functions for
  conditional item fit and item-restscore has been made available as a
  preprint: <https://pgmj.github.io/rasch_itemfit/>

## easyRasch 0.3.3.2

- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  now shows item location relative to sample mean theta.
- [`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md)
  now uses [`quantile()`](https://rdrr.io/r/stats/quantile.html) to
  calculate values for the black horizontal lines to align with
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  cutoff values.
- Since I still haven’t had time to fix the issues with
  [`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md),
  [`RIrestscore()`](https://pgmj.github.io/easyRasch/reference/RIrestscore.md)
  and
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  both use
  [`eRm::person.parameter()`](https://rdrr.io/pkg/eRm/man/person.parameter.html)
  to estimate person locations for the relative item locations variable.
  This change was made since
  [`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md)
  does not handle missing item responses properly.

## easyRasch 0.3.3.1

- [`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md)
  still has bugs with dichotomous data (RM models). It will now stop if
  used with such data.
  - functions using
    [`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md)
    have been temporarily modified to use
    [`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md)
    for RM data. These functions are
    [`RIrestscore()`](https://pgmj.github.io/easyRasch/reference/RIrestscore.md)
    and
    [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)

## easyRasch 0.3.3

- [`RIbootRestscore()`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md)
  now also displays conditional MSQ infit and relative item location in
  “table” output.
  - also, a new option `cutoff = 5`, which filters to only include rows
    with percentage of results above the cutoff.
- Since
  [`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md)
  does not handle incomplete responses as expected, an error message
  will now be produced if one has incomplete responses in data and uses
  the
  [`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md)
  function, recommending the use of the
  [`catR::thetaEst()`](https://rdrr.io/pkg/catR/man/thetaEst.html)-based
  function
  [`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md)
  instead.
- [`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md)
  now automatically chooses model between RM and PCM.
- The “old” functions for estimating person locations/thetas have been
  renamed from
  [`RIestThetasOLD()`](https://pgmj.github.io/easyRasch/reference/RIestThetasOLD.md)
  and `RIestThetasOLD2()` to only
  [`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md).
  - The updated
    [`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md)
    function now works with both polytomous (PCM) and dichotomous data,
    and automatically chooses between PCM and RM.
  - [`RIestThetasCATr()`](https://pgmj.github.io/easyRasch/reference/RIestThetasCATr.md)
    defaults to use multiple cores, `cpu = 4`. Set this value to
    something appropriate for your computer, such as
    `parallel::detectCores() -1`.

## easyRasch 0.3.2

- New function
  [`RIbootRestscore()`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md),
  particularly intended for those working with sample sizes of ~ 500 and
  up.
  - more details in [documentation on this
    site](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.html)
    or by using
    [`?RIbootRestscore`](https://pgmj.github.io/easyRasch/reference/RIbootRestscore.md)
    in your console after installing the new version.

## easyRasch 0.3.1.2

- Bug fix for
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  and
  [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  when using PCM that could result in all simulations returning empty.
- [`RIrestscore()`](https://pgmj.github.io/easyRasch/reference/RIrestscore.md)
  has an additional output column indicating the item location relative
  to person mean location to better reflect targeting properties.

## easyRasch 0.3.1.1

- [`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md)
  now uses the same `c(.001,.999)` intervals as
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md).

## easyRasch 0.3.1

- [`RIpartgamLD()`](https://pgmj.github.io/easyRasch/reference/RIpartgamLD.md)
  no longer shows negative gamma values.
- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md):
  new option to set upper/lower cutoff values when calculating quantile
  cutoff values for conditional highlighting based on simulations from
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  - new default setting for `cutoff` is `c(.001,.999)`. The old default
    was `c(.005,.995)`, which according to preliminary simulation
    studies resulted in increased rates of false positives.
- [`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md):
  added note in documentation that it is not advisable to use this
  function with incomplete response data.
  - [`RIestThetasOLD()`](https://pgmj.github.io/easyRasch/reference/RIestThetasOLD.md)
    or `RIestThetasOLD2()` are recommended when there are missing
    responses for some items for some respondents.
- `RItargeting(model = "RM")` now sorts items according to order in
  data.
- [`RIrestscore()`](https://pgmj.github.io/easyRasch/reference/RIrestscore.md)
  and
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  now also output a column with item (average for polytomous items)
  location. This is due to preliminary simulation studies indicating
  that misfitting items \> 1.5 logits from person mean require a larger
  sample to identify reliably.

## easyRasch 0.3

- package name changed from `RISEkbmRasch` to `easyRasch`.

## easyRasch 0.2.4.6

- [`RIpartgamDIF()`](https://pgmj.github.io/easyRasch/reference/RIpartgamDIF.md)
  added for convenient use of
  [`iarm::partgam_DIF()`](https://rdrr.io/pkg/iarm/man/partgam_DIF.html)
  to assess DIF.
- [`RIpartgamLD()`](https://pgmj.github.io/easyRasch/reference/RIpartgamLD.md)
  added for convenient use of
  [`iarm::partgam_LD()`](https://rdrr.io/pkg/iarm/man/partgam_LD.html)
  to assess local dependence.
- [`RImissing()`](https://pgmj.github.io/easyRasch/reference/RImissing.md)
  and
  [`RImissingP()`](https://pgmj.github.io/easyRasch/reference/RImissingP.md)
  now return a message (not a plot) if no data is missing.

## easyRasch 0.2.4.5

- Changed caption text in
  [`RIresidcorr()`](https://pgmj.github.io/easyRasch/reference/RIresidcorr.md)
  to be more grammatically correct.
- Bug fix for
  [`RItargeting()`](https://pgmj.github.io/easyRasch/reference/RItargeting.md)
  and
  [`RIitemparams()`](https://pgmj.github.io/easyRasch/reference/RIitemparams.md)
  to use `max(na.rm = TRUE)`.

## easyRasch 0.2.4.4

- New data pre-analysis check
  [`RIcheckdata()`](https://pgmj.github.io/easyRasch/reference/RIcheckdata.md)
  to determine whether there are at least 3 responses in each cell (item
  response category)
  - implemented for
    [`RItargeting()`](https://pgmj.github.io/easyRasch/reference/RItargeting.md)
    and
    [`RIitemparams()`](https://pgmj.github.io/easyRasch/reference/RIitemparams.md)
  - if there are fewer than 3 responses in any cell `mirt` will be used
    to estimate item threshold locations, since it is less prone to
    extreme values under these conditions than `eRm`.
  - if there are fewer than 3 responses in any cell a warning message
    will appear

## easyRasch 0.2.4.3

- New function
  [`RIrestscore()`](https://pgmj.github.io/easyRasch/reference/RIrestscore.md),
  a wrapper function to simplify output from
  [`iarm::item_restscore()`](https://rdrr.io/pkg/iarm/man/item_restscore.html).
- Fix for `RIitemhierarchy()`, where `na.rm = TRUE` was omitted from
  [`rowMeans()`](https://rdrr.io/r/base/colSums.html) leading to no mean
  location for items with less thresholds than others.
- Fix for
  [`RIresidcorr()`](https://pgmj.github.io/easyRasch/reference/RIresidcorr.md)
  to make conditional highlighting work for any values.
  - Added option `output = "quarto"` for output of a
    [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) table
    (without conditional highlighting).

## easyRasch 0.2.4.2

- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  now consistently states that conditional item fit is based on complete
  cases only in the automatic caption text.
- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  has a new option for conditional highlighting of misfit based on
  rule-of-thumb values for infit MSQ according to Smith et al. (1998),
  since Müller (2020) showed that these can be fairly accurate for
  conditional infit and thus useful for a quick look at item fit.
- [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  now defaults to use the same sample size that the conditional item fit
  function uses, which means only complete cases. There is an option to
  change this behavior in the simulation function.

## easyRasch 0.2.4.1

- New function -
  [`RIpboot()`](https://pgmj.github.io/easyRasch/reference/RIpboot.md)
  generates datasets using parametric bootstrapping.

### Bug fix:

- [`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md)
  fix for when/if the first iteration of simulations has missing data

### Known issue:

- [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  and
  [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  gets issues upstream with eRm::RM() when badly skewed dichotomous data
  is used as input.
  - A temporary fix has been implemented, discarding simulated datasets
    with less than 8 positive responses for any item.

## easyRasch 0.2.4

- [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  and
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  now replicate the sample theta distribution accurately using
  resampling with replacement (parametric bootstrapping based on
  estimated sample thetas/item thresholds).
- [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  and
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  will now omit simulated datasets with empty cells (zero responses in
  response categories that should have responses).
- [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  now automatically chooses PCM or RM depending on data (`model` option
  removed)
- [`RIscoreSE()`](https://pgmj.github.io/easyRasch/reference/RIscoreSE.md)
  has support for dichotomomous data and automatically chooses PCM or RM
  depending on data
- [`RItileplot()`](https://pgmj.github.io/easyRasch/reference/RItileplot.md)
  has a new option for setting `text_color`.

## easyRasch 0.2.3.1

- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  should finally calculate misfit correctly so that the sorting works
- Changed
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  model estimation function to use
  [`psychotools::PCModel.fit()`](https://rdrr.io/pkg/psychotools/man/pcmodel.html)
  to speed up simulations slightly for polytomous models. Like the
  [`eRm::PCM()`](https://rdrr.io/pkg/eRm/man/PCM.html) function, this
  also uses Conditional Maximum Likelihood, and produces identical
  results with
  [`iarm::out_infit()`](https://rdrr.io/pkg/iarm/man/out_infit.html).

## easyRasch 0.2.3

- [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  now retains the variable/item names from the data.
- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  now uses conditional highlighting with individual cutoff values for
  each item.
- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  outputs two new variables indicating the differences between observed
  infit/outfit and cutoff threshold values.
- [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  optionally sorts the table output based on misfit per item, using
  either “infit” or “outfit”.
- [`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md)
  optionally shows observed item fit in the plot. Ideally, 95% CI would
  be shown, but the SE output from iarm::out_infit is not reliable
  according to the author (Müller, 2020), and iarm::boot_fit() does not
  output SE, only p-values.

## easyRasch 0.2.2.1

- Changed simulation based cutoff thresholds used by
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  to be `quantile(fitmetric, .995)` and .005 instead of .99 and .01 in
  previous version, to be consistent with
  [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  which uses the one-sided `quantile(fitmetric, .99)`.

## easyRasch 0.2.2

- New
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
  function, which replaces both
  [`RIitemfitPCM()`](https://pgmj.github.io/easyRasch/reference/RIitemfitPCM.md)
  and
  [`RIitemfitRM()`](https://pgmj.github.io/easyRasch/reference/RIitemfitRM.md)
  - Only outputs conditional MSQ (ZSTD irrelevant for conditional item
    fit)
  - Automatically uses RM or PCM depending on data structure.
  - Optional conditional highlighting of simulation based cutoff values,
    and includes the cutoff intervals when using `output = "table"`
    (default).
- Modified
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  to only use conditional MSQ when running simulations.
  - Automatically uses RM or PCM depending on data structure.
- Modified
  [`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md)
  accordingly.
- Removed `RIgetfitTable()` and `RIgetfitLoHi()` since this information
  now is included in the output from the new function
  [`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)

## easyRasch 0.2.1.1

- Added 1st and 99th percentiles (upper/lower limits) for simulation
  based item fit metrics from `RIgetfitTable()`
- [`RIitemfitPCM()`](https://pgmj.github.io/easyRasch/reference/RIitemfitPCM.md)
  and `RIgetfitLoHi()` now use 1st/99th percentile values from
  simulation as cutoffs.
- [`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md)
  now uses these options (see
  [`?ggdist::stat_dotsinterval`](https://mjskay.github.io/ggdist/reference/stat_dotsinterval.html)
  for details) for rendering the distribution of simulated+estimated
  item fit metrics

&nbsp;

    stat_dotsinterval(quantiles = iterations, point_interval = median_qi,
                            layout = "weave", slab_color = NA,
                            .width = c(0.66, 0.99)

## easyRasch 0.2.1

- Dichotomous data now working with
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  and
  [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  - no integration with
    [`RIitemfitRM()`](https://pgmj.github.io/easyRasch/reference/RIitemfitRM.md)
    yet.
- Renamed option `method` to `model` for
  [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
  and
  [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md)
  for consistency across functions.

## easyRasch 0.2.0

### Major update

Implemented two simulation functions to get cutoff values for item fit
and residual correlations (Yen’s Q3). For now, these only work with
polytomous (PCM) data.

As always, documentation is available by using `?function` (without the
parentheses otherwise usually included).

### New functions, and brief descriptions:

- [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md) -
  Get simulation based cutoff values for MSQ and ZSTD.
  - `RIgetfitTable()` - Summarises simulation based cutoff values for
    each item.
  - [`RIgetfitPlot()`](https://pgmj.github.io/easyRasch/reference/RIgetfitPlot.md) -
    Plot (one at a time)
- [`RIgetResidCor()`](https://pgmj.github.io/easyRasch/reference/RIgetResidCor.md) -
  Get simulation based cutoff values for Yen’s Q3 residual correlations
  - Based on Christensen et al. (2017, DOI: 10.1177/0146621616677520).
  - Uses your dataset to get appropriate cutoff values for use with
    [`RIresidcorr()`](https://pgmj.github.io/easyRasch/reference/RIresidcorr.md)

### Changes:

- [`RIitemfitPCM()`](https://pgmj.github.io/easyRasch/reference/RIitemfitPCM.md)
  now has two new options:
  - `simcut` Set to TRUE if you want to use simulation based cutoff
    values
  - `gf` The output object from
    [`RIgetfit()`](https://pgmj.github.io/easyRasch/reference/RIgetfit.md)
    is needed when `simcut = TRUE`
  - example command: `RIitemfitPCMtest(df, simcut = TRUE, gf = getfit)`
