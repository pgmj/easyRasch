# easyRasch
R package for Rasch Measurement Theory based psychometric analysis, intended for use with [Quarto](https://quarto.org) for documentation and presentation of analysis process and results. This package uses other packages for the Rasch analyses, primarily [eRm](https://cran.r-project.org/web/packages/eRm/), [mirt](https://cran.r-project.org/web/packages/mirt/), [psychotree](https://cran.r-project.org/web/packages/psychotree/), [iarm](https://cran.r-project.org/web/packages/iarm/), and [catR](https://cran.r-project.org/web/packages/catR/index.html). The package is intended to simplify the Rasch analysis process and provides easy creation of tables and figures with functions that have few options. The package has been tested on MacOS and Windows with R 4.1 to 4.4.

NOTE: this package was formerly known as `RISEkbmRasch`. The old vignette and package GitHub page will remain available but not receive any updates.

**Please regularly check the [NEWS.md](https://github.com/pgmj/easyRasch/blob/main/NEWS.md) file for notes on updates.**

There is a [vignette](https://pgmj.github.io/raschrvignette/RaschRvign.html) that is recommended reading after you skimmed this README. You will find a sample Rasch analysis in the vignette, with output from most of the package functions. The vignette is produced using Quarto, and its source code is of course also [available](https://github.com/pgmj/pgmj.github.io/blob/main/raschrvignette/RaschRvign.qmd).

Most functions have been developed with polytomous data analysis in mind, using the Rasch partial credit model (PCM). Also, the choice was made to rely primarily on conditional maximum likelihood (CML) estimation for item parameters, since it is robust under various conditions and enables "person-free assessment".

## Installation

First, install the [`devtools`](https://devtools.r-lib.org/) package:
```r
install.packages('devtools')
```

Then install the package and its dependencies: 
```r
devtools::install_github("pgmj/easyRasch", dependencies = TRUE)
```
One user has reported having to cancel the installation and restart R several times before being able to install and load the library. This may be a local issue, but I thought it worth mentioning. If others have the same experience, please let me know.

While not strictly necessary, it is highly recommended to install Quarto (and update your Rstudio and R installation if needed):
- https://quarto.org/docs/get-started/
- https://posit.co/download/rstudio-desktop/

### Upgrading
```r
detach("package:easyRasch", unload = TRUE) # not needed if you haven't loaded the package in your current session
devtools::install_github("pgmj/easyRasch")
```

## Using the package

Most functions in this package are relatively simple wrappers that create outputs such as tables and figures to make the Rasch analysis process quick and visual. The primary introduction to using the package is the [vignette](https://pgmj.github.io/raschrvignette/RaschRvign.html).

There are two data structure requirements:

1. you need to create a dataframe object named `itemlabels` that consists of two variables/columns:
  - the **first variable** named `itemnr`, containing variable names exactly as they are named in the dataframe containing data (for example q1, q2, q3, etc)
  - the **second variable** named `item`, containing either the questionnaire item or a description of it (or description of a task, etc)
2. the response data you want to analyze needs to be in a dataframe with participants as rows and items as columns/variables, with ONLY response data in the dataframe.
  - **the lowest response category needs to be zero (0)** for all items. See <https://pgmj.github.io/datawrangling.html#recoding-response-categories> for sample R code for recoding.
  - you will need to separate any demographic variables into a separate dataframe or separate vectors (preferrably as labeled factors), for analysis of differential item functioning (DIF). Then remove your DIF-variables from the dataframe with item data. **The dataframe with item data can only contain item data for the analysis functions to work** (no ID variable or other demographic variables).

For some Rasch-analysis functions in the package, there are separate functions for polytomous data (more than two response options for each item) and dichotomous/binary data. For instance, `RIitemfitPCM()` for the Partial Credit Model and `RIitemfitRM()` for the dichotomous Rasch Model. The Rating Scale Model (RSM) for polytomous data has not been implemented in any of the functions.

There are functions where the default is PCM and you can use the option `model = "RM"` for dichotomous data:

- `RItargeting()`
- `RItif()`
- `RIloadLoc()`
- `RIestThetas()`
- `RIpfit()`

### Notes on known issues

There are currently few or no checks on whether data input in functions are correct. This means that you need to make sure to follow the instructions above, or you may have unexpected outputs or difficult to interpret error messages. Start by using the functions for descriptive analysis and look closely at the output, which usually reveals mistakes in data coding or demographic variables left in the item dataset.

If there is too much missingness in your data, some functions may have issues or take a lot of time to run. In the Quarto template file there is a script for choosing how many responses a participant needs to have to be included in the analysis. You can experiment with this if you run in to trouble. 

Currently, the `RIloadLoc()` function does not work with any missing data (due to the underlying PCA function), and the workaround for now is to run this command with `na.omit()` for the dataframe (ie. `RIloadLoc(na.omit(df))`. Other reasons for functions taking longer time to run is having a lot of items (30+), and/or if you have a lot of response categories that are disordered (often happens with more than 4-6 response categories, especially if they are unlabeled in the questionnaire).

The `RIitemfitPCM2()` function, that makes use of multiple random subsamples to avoid inflated infit/outfit ZSTD values and runs on multiple CPU's/cores, will fail if there is a lot of missing data or very few responses in some categories. Increasing the sample size and/or decreasing the number of parallel CPUs/cores can help. If that fails, revert to the function `RIitemfitPCM()` that only uses one CPU/core.

### For the curious

For those new to R, it may be useful to know that you can easily access the code in each function by using the base R `View()` function. For example, `View(RItargeting)` shows the code for the `RItargeting()` function that creates a Wright map style figure (after installing and loading the easyRasch package). You can also find the documentation/help on each command by using the command `?RItargeting` in the console (replace `RItargeting` with the function you are interested in).

If you are new to R, [Hadley Wickham's book "R for data science"](https://r4ds.hadley.nz/) is a great place to start. Also have a look at [Introduction to R with Tidyverse](https://introduction-r-tidyverse.netlify.app/) by Sophie Lee.

## Author

[Magnus Johansson](https://www.ri.se/en/person/magnus-p-johansson) is a licensed psychologist with a PhD in behavior analysis. He works as a research scientist at [RISE Research Institutes of Sweden](https://ri.se/en), Department of System Transition, and is an affiliated researcher at [Karolinska Institutet](https://medarbetare.ki.se/orgid/52082137).

- ORCID: [0000-0003-1669-592X](https://orcid.org/0000-0003-1669-592X)
- Bluesky: [@pgmj.bsky.social](https://bsky.app/profile/pgmj.bsky.social) 
- Mastodon: [@pgmj@scicomm.xyz](https://scicomm.xyz/@pgmj)

## License

This work is licensed under [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/).
