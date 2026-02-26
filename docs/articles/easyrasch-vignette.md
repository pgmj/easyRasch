# easyRasch package vignette

## Preamble

This page contains a sample Rasch analysis to showcase most of the core
functions in the `easyRasch` package. A more extensive guide that also
helps to show the benefits of the Quarto publishing system, is available
here: <https://pgmj.github.io/raschrvignette/RaschRvign.html>

It is strongly recommended to go to the link above. I’ve become rather
addicted to the convenience of panel-tabsets that Quarto offers that
allow side-by-side viewing of output. These are unfortunately not
available when generating this package article/vignette using `pkgdown`
(AFAIK). Lacking panel-tabsets, this page will become quite long (not to
say that the other one is short either…).

**NOTE: This package was previously known as `RISEkbmRasch`**

If you are new to Rasch Measurement Theory, you may find this intro
presentation useful:
<https://pgmj.github.io/RaschIRTlecture/slides.html>

There is a separate GitHub repository containing a template R-project to
simplify getting started with the `easyRasch` package and conducting a
reproducible Rasch analysis in R:
<https://github.com/pgmj/RISEraschTemplate>

## Introduction

This vignette will walk through a sample analysis using an open dataset
with polytomous questionnaire data.

One of the aims with this package is to simplify reproducible
psychometric analysis to shed light on the measurement properties of a
scale, questionnaire or test. In a preprint ([Johansson et al.
2023](#ref-johansson)), our [research
group](https://www.ri.se/en/what-we-do/projects/center-for-categorically-based-measurements)
propose that the basic aspects of a psychometric analysis should include
information about:

- Unidimensionality & local independence
- Response categories (monotonicity)
- Invariance (Differential Item Functioning)
- Targeting
- Measurement uncertainties (reliability)

We’ll include several ways to investigate these measurement properties
using Rasch Measurement Theory. There are also functions in the package
less directly related to the criteria above that will be demonstrated in
this vignette.

Please note that this is just a sample analysis to showcase the R
package. It is not intended as a “best practice” psychometric analysis
example.

You can skip ahead to the Rasch analysis part in [Section 5](#sec-rasch)
if you are eager to look at the package output :)

## Getting started

Since the package is intended for use with Quarto, this vignette has
also been created with Quarto. A “template” .qmd file [is
available](https://github.com/pgmj/RISEraschTemplate/blob/main/analysis.qmd)
that can be useful to have handy for copy&paste when running a new
analysis. You can also download a complete copy of the Quarto/R code to
produce this document
[here](https://github.com/pgmj/pgmj.github.io/blob/main/raschrvignette/RaschRvign.qmd).

Loading the `easyRasch` package will automatically load all the packages
it depends on. However, it could be desirable to explicitly load all
packages used to enable the automatic creation of citations for them,
using the `grateful` package (see [Section 14](#sec-grateful)).

``` r
library(easyRasch) # devtools::install_github("pgmj/easyRasch")
library(grateful)
library(ggrepel)
library(car)
library(kableExtra)
library(readxl)
library(tidyverse)
library(eRm)
library(iarm)
library(mirt)
library(psych)
library(ggplot2)
library(psychotree)
library(matrixStats)
library(reshape)
library(knitr)
library(patchwork)
library(formattable) 
library(glue)
library(foreach)
library(doParallel)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename
```

### Loading data

We will use data from a recent paper investigating the “initial
elevation effect” ([Anvari et al. 2022](#ref-anvari2022)), and focus on
the 10 negative items from the PANAS. The data is available at the OSF
website.

``` r
df.all <- read_csv("https://osf.io/download/6fbr5/") # you can use this line to load the data directly from the OSF website
#df.all <- read_csv("vignettes/anvari2022data.csv") # or this line if you have downloaded the datafile in the /vignettes/articles folder
# if you have issues with the link, please try downloading manually using the same URL as above
# and read the file from your local drive.

# subset items and demographic variables
df <- df.all %>% 
  select(starts_with("PANASD2_1"),
         starts_with("PANASD2_20"),
         age,Sex,Group) %>% 
  select(!PANASD2_10_Active) %>% 
  select(!PANASD2_1_Attentive)
```

The [`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html)
function provides a quick overview of our dataframe.

``` r
glimpse(df)
```

    Rows: 1,856
    Columns: 13
    $ PANASD2_11_Distressed <dbl> 2, 2, 2, 1, 2, 2, 4, 1, 1, 3, 1, 4, 2, 4, 4, 1, …
    $ PANASD2_12_Upset      <dbl> 1, 1, 4, 1, 1, 5, 2, 1, 2, 2, 2, 3, 1, 3, 5, 1, …
    $ PANASD2_13_Hostile    <dbl> 1, 1, 2, 1, 1, 3, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, …
    $ PANASD2_14_Irritable  <dbl> 1, 1, 3, 1, 2, 5, 3, 1, 2, 4, 2, 3, 1, 2, 3, 1, …
    $ PANASD2_15_Scared     <dbl> 1, 1, 3, 1, 1, 4, 1, 1, 1, 2, 2, 2, 1, 4, 4, 1, …
    $ PANASD2_16_Afraid     <dbl> 1, 1, 4, 1, 1, 3, 1, 1, 1, 3, 1, 2, 1, 4, 4, 1, …
    $ PANASD2_17_Ashamed    <dbl> 1, 1, 2, 1, 1, 3, 1, 1, 1, 2, 1, 4, 1, 1, 3, 1, …
    $ PANASD2_18_Guilty     <dbl> 2, 1, 2, 1, 1, 3, 3, 1, 1, 3, 1, 4, 1, 1, 3, 1, …
    $ PANASD2_19_Nervous    <dbl> 1, 1, 2, 1, 2, 4, 4, 1, 1, 4, 2, 4, 2, 1, 5, 1, …
    $ PANASD2_20_Jittery    <dbl> 1, 2, 3, 1, 1, 2, 3, 3, 2, 1, 2, 2, 1, 1, 4, 1, …
    $ age                   <dbl> 27, 32, 21, 27, 20, 22, 23, 25, 21, 26, 38, 36, …
    $ Sex                   <chr> "Male", "Male", "Female", "Male", "Male", "Male"…
    $ Group                 <chr> "Later Start", "Later Start", "Later Start", "La…

We have 1856 rows, ie. respondents. All variables except Sex and Group
are of class `dbl`, which means they are numeric and can have decimals.
Integer (numeric with no decimals) would also be fine for our purposes.
The two demographic variables currently of class `chr` (character) will
need to be converted to factors (`fct`), and we will do that later on.

(If you import a dataset where item variables are of class character,
you will need to recode to numeric.)

### Itemlabels

Then we set up the itemlabels dataframe. This could also be done using
the free [LibreOffice
Calc](https://www.libreoffice.org/download/download-libreoffice/) or MS
Excel. Just make sure the file has the same structure, with two
variables named `itemnr` and `item` that contain the item variable names
and item description. The item variable names have to match the variable
names in the item dataframe.

``` r
itemlabels <- df %>% 
  select(starts_with("PAN")) %>% 
  names() %>% 
  as_tibble() %>% 
  separate(value, c(NA, "item"), sep ="_[0-9][0-9]_") %>% 
  mutate(itemnr = paste0("PANAS_",c(11:20)), .before = "item")
```

The `itemlabels` dataframe looks like this.

``` r
itemlabels
```

    # A tibble: 10 × 2
       itemnr   item
       <chr>    <chr>
     1 PANAS_11 Distressed
     2 PANAS_12 Upset
     3 PANAS_13 Hostile
     4 PANAS_14 Irritable
     5 PANAS_15 Scared
     6 PANAS_16 Afraid
     7 PANAS_17 Ashamed
     8 PANAS_18 Guilty
     9 PANAS_19 Nervous
    10 PANAS_20 Jittery   

### Demographics

Variables for invariance tests such as Differential Item Functioning
(DIF) need to be separated into vectors (ideally as factors with
specified levels and labels) with the same length as the number of rows
in the dataset. This means that any kind of removal of respondents/rows
with missing data needs to be done before separating the DIF variables.

We need to check how the `Sex` variable has been coded and which
responses are present in the data.

``` r
table(df$Sex)
```

      CONSENT REVOKED      DATA EXPIRED            Female              Male
                    2                 1               896               955
    Prefer not to say
                    2 

Since there are only 5 respondents using labels outside of Female/Male
(too few for meaningful statistical analysis), we will remove them to
have a complete dataset for all variables in this example.

``` r
df <- df %>% 
  filter(Sex %in% c("Female","Male"))
```

Let’s make the variable a factor (instead of class “character”) and put
in in a vector separate from the item dataframe.

``` r
dif.sex <- factor(df$Sex)
```

And remove our DIF demographic variable from the item dataset.

``` r
df$Sex <- NULL
```

We can now make use of a very simple function included in this package!

``` r
RIdemographics(dif.sex, "Sex")
```

| Sex    |   n | Percent |
|:-------|----:|--------:|
| Female | 896 |    48.4 |
| Male   | 955 |    51.6 |

Let’s move on to the age variable.

``` r
glimpse(df$age)
```

     num [1:1851] 27 32 21 27 20 22 23 25 21 26 ...

Sometimes age is provided in categories, but here we have a numeric
variable with age in years. Let’s have a quick look at the age
distribution using a histogram, and calculate mean, sd and range.

``` r
### simpler version of the ggplot below using base R function hist()
# hist(df$age, col = "#009ca6")
# abline(v = mean(age, na.rm = TRUE))
# 
# df %>% 
#   summarise(Mean = round(mean(age, na.rm = T),1),
#             StDev = round(sd(age, na.rm = T),1)
#             )

ggplot(df) +
  geom_histogram(aes(x = age), 
                 fill = "#009ca6",
                 col = "black") +
  # add the average as a vertical line
  geom_vline(xintercept = mean(df$age), 
             linewidth = 1.5,
             linetype = 2,
             col = "orange") +
  # add a light grey field indicating the standard deviation
  annotate("rect", ymin = 0, ymax = Inf, 
           xmin = (mean(df$age, na.rm = TRUE) - sd(df$age, na.rm = TRUE)), xmax = (mean(df$age, na.rm = TRUE) + sd(df$age, na.rm = TRUE)), 
           alpha = .2) +
  labs(title = "",
       x = "Age in years",
       y = "Number of respondents",
       caption = glue("Note. Mean age is {round(mean(df$age, na.rm = T),1)} years with a standard deviation of {round(sd(df$age, na.rm = T),1)}. Age range is {min(df$age)} to {max(df$age)}.")
       ) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-12-1.png)

Age also needs to be a separate vector, and removed from the item
dataframe.

``` r
dif.age <- df$age
df$age <- NULL
```

There is also a grouping variable which needs to be converted to a
factor.

``` r
dif.group <- factor(df$Group)
df$Group <- NULL
RIdemographics(dif.group, "Group")
```

| Group         |   n | Percent |
|:--------------|----:|--------:|
| Earlier Start | 901 |    48.7 |
| Later Start   | 950 |    51.3 |

With only item data remaining in the dataframe, we can easily rename the
items in the item dataframe. These names match the `itemlabels` variable
`itemnr`.

``` r
names(df) <- itemlabels$itemnr
```

Now we are all set for the psychometric analysis!

## Descriptives

Let’s familiarize ourselves with the data before diving into the
analysis.

### Missing data

First, we visualize the proportion of missing data on item level.

``` r
RImissing(df)
```

    [1] "No missing data."

No missing data in this dataset. If we had missing data, we could also
use
[`RImissingP()`](https://pgmj.github.io/easyRasch/reference/RImissingP.md)
to look at which respondents have missing data and how much.

### Overall responses

This provides us with an overall picture of the data distribution. As a
bonus, any oddities/mistakes in recoding the item data from categories
to numbers will be clearly visible.

``` r
RIallresp(df)
```

| Response category | Number of responses | Percent |
|------------------:|--------------------:|--------:|
|                 1 |                9430 |    50.9 |
|                 2 |                4136 |    22.3 |
|                 3 |                2676 |    14.5 |
|                 4 |                1722 |     9.3 |
|                 5 |                 546 |     2.9 |

Most R packages for Rasch analysis require the lowest response category
to be zero, which makes it necessary for us to recode our data, from
using the range of 1-5 to 0-4.

``` r
df <- df %>% 
  mutate(across(everything(), ~ car::recode(.x, "1=0;2=1;3=2;4=3;5=4", as.factor = F)))

# always check that your recoding worked as intended.
RIallresp(df)
```

| Response category | Number of responses | Percent |
|------------------:|--------------------:|--------:|
|                 0 |                9430 |    50.9 |
|                 1 |                4136 |    22.3 |
|                 2 |                2676 |    14.5 |
|                 3 |                1722 |     9.3 |
|                 4 |                 546 |     2.9 |

#### Floor/ceiling effects

Now, we can also look at the raw distribution of sum scores. The
[`RIrawdist()`](https://pgmj.github.io/easyRasch/reference/RIrawdist.md)
function is a bit crude, since it requires responses in all response
categories to accurately calculate max and min scores.

``` r
RIrawdist(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-19-1.png)

We can see a floor effect with 11.8% of participants responding in the
lowest category for all items.

#### Guttman structure

While not really necessary, it could be interesting to see whether the
response patterns follow a Guttman-like structure. Items and persons are
sorted based on lower-\>higher responses, and we should see the color
move from yellow in the lower left corner to blue in the upper right
corner.

``` r
RIheatmap(df) +
  theme(axis.text.x = element_blank())
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-20-1.png)

In this data, we see the floor effect on the left, with 11.8% of
respondents all yellow, and a rather weak Guttman structure. This could
also be due to a low variation in item locations/difficulties. Since we
have a very large sample I added a
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) option
to remove the x-axis text, which would anyway just be a blur of the 1851
respondent row numbers. Each thin vertical slice in the figure is one
respondent.

### Item level descriptives

There are many ways to look at the item level data, and we’ll get them
all together in the tab-panel below. The
[`RItileplot()`](https://pgmj.github.io/easyRasch/reference/RItileplot.md)
is probably most informative, since it provides the number of responses
in each response category for each item. It is usually recommended to
have at least ~10 responses in each category for psychometric analysis,
no matter which methodology is used.

Kudos to [Solomon
Kurz](https://solomonkurz.netlify.app/blog/2021-05-11-yes-you-can-fit-an-exploratory-factor-analysis-with-lavaan/)
for providing the idea and code on which the tile plot function is
built!

Most people will be familiar with the barplot, and this is probably most
intuitive to understand the response distribution within each item.
However, if there are many items it will take a while to review, and
does not provide the same overview as a tileplot or stacked bars.

For this section, when using Quarto, I like to have the items available
in the margin, but now we’ll have to settle for a table inline.

``` r
RIlistitems(df)
```

| itemnr   | item       |
|:---------|:-----------|
| PANAS_11 | Distressed |
| PANAS_12 | Upset      |
| PANAS_13 | Hostile    |
| PANAS_14 | Irritable  |
| PANAS_15 | Scared     |
| PANAS_16 | Afraid     |
| PANAS_17 | Ashamed    |
| PANAS_18 | Guilty     |
| PANAS_19 | Nervous    |
| PANAS_20 | Jittery    |

#### Tile plot

``` r
RItileplot(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-22-1.png)

While response patterns are skewed for all items, there are more than 10
responses in each category for all items which is helpful for the
analysis.

#### Stacked bars

``` r
RIbarstack(df) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-23-1.png)

#### Barplots

``` r
RIbarplot(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-1.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-2.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-3.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-4.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-5.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-6.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-7.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-8.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-9.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-10.png)

## Rasch analysis 1

The eRm package and Conditional Maximum Likelihood (CML) estimation will
be used primarily, with the Partial Credit Model since this is
polytomous data.

This is also where the [five basic psychometric
aspects](https://doi.org/10.31219/osf.io/3htzc) are good to recall.

- Unidimensionality & local independence
- Response categories
- Invariance
- Targeting
- Measurement uncertainties (reliability)

We will begin by looking at unidimensionality, response categories, and
targeting in parallel below. For unidimensionality, we are mostly
interested in item fit and residual correlations, as well as PCA of
residuals and loadings on the first residual contrast. At the same time,
disordered response categories can influence item fit to some extent
(and vice versa), and knowledge about targeting can be useful if it is
necessary to remove items due to residual correlations.

When unidimensionality and response categories are found to work
adequately, we will move on to invariance testing (Differential Item
Functioning, DIF). It should be noted that DIF should be evaluated in
parallel with all other psychometric aspects, but since it is a more
complex issue it is kept in a separate section in this vignette (as is
person fit). Finally, when/if invariance/DIF also looks acceptable, we
can investigate reliability/measurement uncertainties.

### Conditional item fit

``` r
simfit1 <- RIgetfit(df, iterations = 1000, cpu = 8) # save simulation output to object `simfit1`
RIitemfit(df, simfit1)
```

[TABLE]

[`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
works with both dichotomous and polytomous data (no option needed).

It is important to note that the new (since version 0.2.2, released
2024-08-19)
[`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
function uses **conditional** outfit/infit, which is both robust to
different sample sizes and makes ZSTD unnecessary ([Müller
2020](#ref-muller_item_2020)).

Since the distribution of item fit statistics are not known, we need to
use simulation to determine appropriate cutoff threshold values for the
current sample and items.
[`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
can also use the simulation based cutoff values and use them for
conditional highlighting of misfitting items. See the [blog post on
simulation based cutoffs](https://pgmj.github.io/simcutoffs.html) for
some more details on this.
[`RIitemfit()`](https://pgmj.github.io/easyRasch/reference/RIitemfit.md)
can also be used without cutoffs and conditional highlighting. For a
possibly useful rule-of-thumb cutoff for infit MSQ only, use the option
`cutoff = "Smith98"` ([Smith, Schumacker, and Bush
1998](#ref-smith_using_1998); [Müller 2020](#ref-muller_item_2020)).
However, this cutoff is not applicable for all items, only for what can
be expected for the *average* item fit. The simulation/bootstrap-based
cutoff values will be more accurate for every item in your data.

Briefly stated, the simulation uses the properties of the current sample
and items, and simulates n iterations of data that fit the Rasch model
to get an empirical distribution of item fit that we can use for
comparison with the observed data. This is also known as “parametric
bootstrapping”.

The simulation can take quite a bit of time to run if you have complex
data/many items/many participants, and/or choose to use many iterations.
While insufficient testing has been done to make any strong
recommendations, I think 500 iterations is a good starting point.

For reference, the simulation above, using 10 items with 5 response
categories each and 1851 respondents, takes about 52 seconds to run on 8
cpu cores (Macbook Pro M1 Max) for 1000 iterations.

I’ll cite Ostini & Nering ([2006](#ref-ostini_polytomous_2006)) on the
description of outfit and infit (pages 86-87):

> Response residuals can be summed over respondents to obtain an item
> fit measure. Generally, the accumulation is done with squared
> standardized residuals, which are then divided by the total number of
> respondents to obtain a mean square statistic. In this form, the
> statistic is referred to as an **unweighted mean square** (Masters &
> Wright, 1997; Wright & Masters, 1982) and has also come to be known as
> **“outfit”** (Smith, Schumacker, & Bush, 1998; Wu, 1997), perhaps
> because it is highly sensitive to outlier responses (Adams & Khoo,
> 1996; Smith et al., 1998; Wright & Masters, 1982).

> A weighted version of this statistic was developed to counteract its
> sensitivity to outliers (Smith, 2000). In its weighted form, the
> squared standardized residual is multiplied by the observed response
> variance and then divided by the sum of the item response variances.
> This is sometimes referred to as an **information weighted mean
> square** and has become known as **“infit”** (Smith et al., 1998; Wu,
> 1997).

A low item fit value (sometimes referred to as “overfitting” the Rasch
model) indicates that responses are too predictable and provide little
information. This is often the case for items that are very
general/broad in scope in relation to the latent variable.

A high item fit value (sometimes referred to as “underfitting” the Rasch
model) can indicate several things, often multidimensionality or a
question that is difficult to interpret. This could for instance be a
question that asks about two things at the same time or is ambiguous for
other reasons.

### Item-restscore

This is another useful function from the `iarm` package. It shows the
expected and observed correlation between an item and a score based on
the rest of the items ([Kreiner 2011](#ref-kreiner_note_2011)).
Similarly, but inverted, to item fit, a lower observed correlation value
than expected indicates that the item may not belong to the dimension. A
higher than expected observed value indicates an overfitting and
possibly redundant item.

``` r
RIrestscore(df)
```

| Item | Observed value | Model expected value | Absolute difference | Adjusted p-value (BH) | Statistical significance level | Location | Relative location |
|:---|---:|---:|---:|---:|:---|---:|---:|
| PANAS_11 | 0.58 | 0.64 | 0.06 | 0.001 | \*\* | -0.33 | 1.14 |
| PANAS_12 | 0.71 | 0.64 | 0.07 | 0.000 | \*\*\* | -0.06 | 1.42 |
| PANAS_13 | 0.57 | 0.62 | 0.05 | 0.015 | \* | 0.43 | 1.91 |
| PANAS_14 | 0.61 | 0.64 | 0.03 | 0.066 | . | -0.32 | 1.15 |
| PANAS_15 | 0.73 | 0.63 | 0.10 | 0.000 | \*\*\* | 0.16 | 1.63 |
| PANAS_16 | 0.72 | 0.64 | 0.08 | 0.000 | \*\*\* | 0.00 | 1.48 |
| PANAS_17 | 0.68 | 0.63 | 0.05 | 0.003 | \*\* | 0.36 | 1.83 |
| PANAS_18 | 0.62 | 0.63 | 0.01 | 0.408 |  | 0.30 | 1.77 |
| PANAS_19 | 0.67 | 0.64 | 0.03 | 0.088 | . | -0.49 | 0.98 |
| PANAS_20 | 0.56 | 0.63 | 0.07 | 0.000 | \*\*\* | -0.05 | 1.42 |

### Conditional item characteristic curves

The [`iarm`](https://cran.r-project.org/web/packages/iarm/index.html)
package ([Mueller and Santiago 2022](#ref-mueller_iarm_2022)) provides
several interesting functions for assessing item fit, DIF and other
things. Some of these functions may be included in a future version of
the `easyRasch` package. Below are conditional item characteristic
curves (ICC’s) using the estimated theta (factor score).

These curves indicate item fit on a group level, where resondents are
split into “class intervals” based on their sum score/factor score.

``` r
library(iarm)
ICCplot(as.data.frame(df), 
        itemnumber = 1:4, 
        method = "cut", 
        cinumber = 6, # number of class intervals to split respondents into
        itemdescrip = c("PANAS_11","PANAS_12","PANAS_13","PANAS_14"))
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-27-1.png)

    [1] "Please press Zoom on the Plots window to see the plot"

A similar, but even more informative and flexible, visualization has
been made available in the
[`RASCHplot`](https://github.com/ERRTG/RASCHplot/) package ([Buchardt,
Christensen, and Jensen 2023](#ref-buchardt_visualizing_2023)), which
needs to be installed from GitHub (see code below). The linked paper is
recommended reading, not least for descriptions of the useful options
available. Below are some sample plots showing conditional ICC’s using
the raw sum score.

``` r
library(RASCHplot) # devtools::install_github("ERRTG/RASCHplot")

CICCplot(PCM(df),
         which.item = c(1:4),
         lower.groups = c(0,7,14,21,28),
         grid.items = TRUE)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-28-1.png)

### PCA of residuals

Principal Component Analysis of Rasch model residuals.

``` r
RIpcmPCA(df)
```

| Eigenvalues | Proportion of variance |
|------------:|:-----------------------|
|        1.79 | 16.9%                  |
|        1.47 | 15.1%                  |
|        1.28 | 13.6%                  |
|        1.14 | 13.3%                  |
|        1.06 | 11.9%                  |

Based on rule of thumb, the first eigenvalue should be below 2.0 to
support unidimensionality. However, this is seldom accurate and needs to
be complemented with checking item fit and residual correlations. The
target value should probably be below 1.75, based on my experience, but
really I find this metric redundant and only keep it here for those
coming from Winsteps who might be looking for it. Speaking of Winsteps,
the “explained variance” will not be comparable to Winsteps
corresponding metric, since this one only shows the results from the
analysis of residuals.

### Residual correlations

Similarly to item fit, we need to run simulations to get a useful cutoff
threshold value for when residual correlations amongst item pairs are
too large to support the local independence assumption ([Christensen,
Makransky, and Horton 2017](#ref-christensen2017)).

And again, the simulation can take a bit of time, but it is necessary to
set the appropriate cutoff value.

``` r
simcor1 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor1$p99)
```

[TABLE]

The matrix above shows item-pair correlations of item residuals, with
highlights in red showing correlations crossing the threshold compared
to the average item-pair correlation (for all item-pairs) ([Christensen,
Makransky, and Horton 2017](#ref-christensen2017)). Rasch model residual
correlations (Yen’s Q3) are calculated using the
[mirt](https://cran.r-project.org/web/packages/mirt/index.html) package.

### Partial gamma LD

Another way to assess local (in)dependence is by partial gamma
coefficients ([Kreiner and Christensen
2004](#ref-kreiner_analysis_2004)). This is also a function from the
`iarm` package. See
[`?iarm::partgam_LD`](https://rdrr.io/pkg/iarm/man/partgam_LD.html) for
details.

``` r
RIpartgamLD(df)
```

| Item 1   | Item 2   | Partial gamma |    SE | Lower CI | Upper CI | Adjusted p-value (BH) |
|:---------|:---------|:--------------|------:|---------:|---------:|----------------------:|
| PANAS_15 | PANAS_16 | 0.634         | 0.030 |    0.576 |    0.693 |                 0.000 |
| PANAS_16 | PANAS_15 | 0.634         | 0.030 |    0.576 |    0.692 |                 0.000 |
| PANAS_17 | PANAS_18 | 0.577         | 0.033 |    0.512 |    0.642 |                 0.000 |
| PANAS_18 | PANAS_17 | 0.551         | 0.034 |    0.484 |    0.619 |                 0.000 |
| PANAS_12 | PANAS_14 | 0.341         | 0.037 |    0.269 |    0.413 |                 0.000 |
| PANAS_16 | PANAS_19 | 0.297         | 0.040 |    0.219 |    0.375 |                 0.000 |
| PANAS_14 | PANAS_12 | 0.293         | 0.038 |    0.217 |    0.368 |                 0.000 |
| PANAS_15 | PANAS_19 | 0.287         | 0.040 |    0.208 |    0.366 |                 0.000 |
| PANAS_19 | PANAS_16 | 0.274         | 0.040 |    0.197 |    0.352 |                 0.000 |
| PANAS_19 | PANAS_15 | 0.261         | 0.041 |    0.181 |    0.340 |                 0.000 |
| PANAS_14 | PANAS_13 | 0.209         | 0.042 |    0.127 |    0.291 |                 0.000 |
| PANAS_13 | PANAS_14 | 0.202         | 0.041 |    0.121 |    0.283 |                 0.000 |
| PANAS_12 | PANAS_13 | 0.191         | 0.044 |    0.105 |    0.278 |                 0.001 |

### 1st contrast loadings

``` r
RIloadLoc(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-32-1.png)

Here we see item locations and their loadings on the first residual
contrast. This figure can be helpful to identify clusters in data or
multidimensionality.

### Analysis of response categories

The `xlims` setting changes the x-axis limits for the plots. The default
values usually make sense, and we mostly add this option to point out
the possibility of doing so. You can also choose to only show plots for
only specific items.

``` r
RIitemCats(df, xlims = c(-5,5))
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-1.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-2.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-3.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-4.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-5.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-6.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-7.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-8.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-9.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-10.png)

Each response category for each item should have a curve that indicates
it to be the most probably response at some point on the latent variable
(x axis in the figure).

### Response categories MIRT

For a more compact figure.

``` r
mirt(df, model=1, itemtype='Rasch', verbose = FALSE) %>% 
  plot(type="trace", as.table = TRUE, 
       theta_lim = c(-5,5)) # changes x axis limits
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-34-1.png)

### Targeting

``` r
# increase fig-height in the chunk option above if you have many items
RItargeting(df, xlim = c(-5,4)) # xlim defaults to c(-4,4) if you omit this option
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-35-1.png)

This figure shows how well the items fit the respondents/persons. It is
a sort of [Wright Map](https://www.rasch.org/rmt/rmt253b.htm) that shows
person locations and item threshold locations on the same logit scale.

The top part shows person location histogram, the middle part an
inverted histogram of item threshold locations, and the bottom part
shows individual item threshold locations. The histograms also show
means and standard deviations.

### Item hierarchy

Here the items are sorted on their average threshold location (black
diamonds). 84% confidence intervals are shown around each item threshold
location. For further details, see the caption text below the figure.

The numbers displayed in the plot can be disabled using the option
`numbers = FALSE`.

``` r
RIitemHierarchy(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-36-1.png)

### Analysis 1 comments

Item fit shows a lot of issues.

Item 18 has issues with the second lowest category being disordered.
Several other items have very short distances between thresholds 1 and
2, which is also clearly seen in the Item Hierarchy figure above.

Two item-pairs show residual correlations far above the cutoff value:

- 15 and 16 (scared and afraid)
- 17 and 18 (ashamed and guilty)

Since item 15 also has a residual correlation with item 19, we will
remove it. In the second pair, item 18 will be removed since it also has
problems with disordered response categories.

We have multiple “diagnostics” to review when deciding which item to
remove if there are strong residual correlations between two items. Here
is a list of commonly used criteria:

- item fit
- item threshold locations compared to sample locations (targeting)
- ordering of response categories
- DIF
- and whether there are residual correlations between one item and
  multiple other items

``` r
removed.items <- c("PANAS_15","PANAS_18")

df_backup <- df

df <- df_backup %>% 
  select(!any_of(removed.items))
```

As seen in the code above, I chose to create a copy of the dataframe
with the removed items omitted. This can be useful if, at a later stage
in the analysis, I want to be able to quickly “go back” and reinstate an
item or undo any other change I have made.

## Rasch analysis 2

With items 15 and 18 removed.

|  itemnr  | item       |
|:--------:|:-----------|
| PANAS_11 | Distressed |
| PANAS_12 | Upset      |
| PANAS_13 | Hostile    |
| PANAS_14 | Irritable  |
| PANAS_16 | Afraid     |
| PANAS_17 | Ashamed    |
| PANAS_19 | Nervous    |
| PANAS_20 | Jittery    |

### Conditional item fit

``` r
simfit2 <- RIgetfit(df, iterations = 1000, cpu = 8)
RIitemfit(df, simcut = simfit2)
```

[TABLE]

### PCA of residuals

``` r
RIpcmPCA(df)
```

| Eigenvalues | Proportion of variance |
|------------:|:-----------------------|
|        1.52 | 18.9%                  |
|        1.33 | 17.1%                  |
|        1.19 | 16.6%                  |
|        1.15 | 14.6%                  |
|        1.00 | 13.1%                  |

### Residual correlations

``` r
simcor2 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor2$p99)
```

[TABLE]

### 1st contrast loadings

``` r
RIloadLoc(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-42-1.png)

### Targeting

``` r
RItargeting(df, xlim = c(-4,4), bins = 45)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-43-1.png)

### Item hierarchy

``` r
RIitemHierarchy(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-44-1.png)

### Analysis 2 comments

Items 16 & 19, and 12 & 14 show problematic residual correlations.

Let’s look at DIF before taking action upon this information. While we
are keeping DIF as a separate section in this vignette, it is
recommended to include DIF-analysis in the `panel-tabset` above (on item
fit, PCA, residual correlations, etc).

## DIF - differential item functioning

We’ll be looking at whether item (threshold) locations are stable
between demographic subgroups.

There are several DIF analysis tools available. The first one uses the
package `psychotree`, which relies on statistical significance at p \<
.05 as an indicator for DIF. This is a criterion that is highly sample
size sensitive, and we are always interested in the size/magnitude of
DIF as well, since that will inform us about the impact of DIF on the
estimated latent variable.

The structure of DIF is also an important and complex aspect,
particularly for polytomous data. Uniform DIF means that the DIF is
similar across the latent continuum. We can test this in R using the
`lordif` package, as demonstrated in [Section 7.6](#sec-lordif).
However, it should be noted that the `lordif` package does not provide
an option to use Rasch models, and there may be results that are caused
by also allowing the discrimination parameter to vary across items.

A recent preprint ([Henninger et al. 2024](#ref-henninger_partial_2024))
does a great job illustrating “differential step functioning” (DSF),
which is when item threshold locations in polytomous data show varying
levels of DIF. It also describes a forthcoming development of the
`psychotree` where one can use DIF effect size and purification
functions to evaluate DIF/DSF. When the updated package is available, I
will work to implement these new functions into the `easyRasch` package
as well.

It is important to ensure that no cells in the data are empty for
subgroups when conducting a DIF analysis. Split the data using the
DIF-variable and create separate tileplots to review the response
distribution in the DIF-groups.

``` r
difPlots <- df %>% # save the output into the `difPlots` object
  add_column(gender = dif.sex) %>% # add the DIF variable to the dataframe
  split(.$gender) %>% # split the data using the DIF variable
  map(~ RItileplot(.x %>% dplyr::select(!gender)) + labs(title = .x$gender)) # create separate tileplots for each group

difPlots$Female + difPlots$Male # the actual name of the plots (in this case Male/Female) will be determined by the factor labels
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-45-1.png)

### Sex

|  itemnr  | item       |
|:--------:|:-----------|
| PANAS_11 | Distressed |
| PANAS_12 | Upset      |
| PANAS_13 | Hostile    |
| PANAS_14 | Irritable  |
| PANAS_16 | Afraid     |
| PANAS_17 | Ashamed    |
| PANAS_19 | Nervous    |
| PANAS_20 | Jittery    |

#### Table

``` r
RIdifTable(df, dif.sex)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-47-1.png)

|     Item |      2 |      3 | Mean location | StDev | MaxDiff |
|---------:|-------:|-------:|--------------:|------:|--------:|
| PANAS_11 | -0.314 | -0.196 |        -0.255 | 0.083 |   0.117 |
| PANAS_12 |  0.028 | -0.044 |        -0.008 | 0.051 |   0.073 |
| PANAS_13 |  0.553 |  0.402 |         0.478 | 0.107 |   0.151 |
| PANAS_14 | -0.328 | -0.183 |        -0.255 | 0.103 |   0.146 |
| PANAS_16 |  0.004 |  0.114 |         0.059 | 0.078 |   0.111 |
| PANAS_17 |  0.520 |  0.290 |         0.405 | 0.163 |   0.230 |
| PANAS_19 | -0.495 | -0.355 |        -0.425 | 0.099 |   0.140 |
| PANAS_20 |  0.032 | -0.028 |         0.002 | 0.042 |   0.059 |

#### Figure items

``` r
RIdifFigure(df, dif.sex)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-48-1.png)

#### Figure thresholds

``` r
RIdifFigThresh(df, dif.sex)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-49-1.png)

While no item shows problematic levels of DIF regarding item location,
as shown by the table, there is an interesting pattern in the thresholds
figure. The lowest threshold seems to be slightly lower for node 3
(Male) for all items. Also, item 11 shows a much wider spread of item
locations for node 3 compared to node 2.

The results do not require any action since the difference is small.

### Age

The `psychotree` package uses a model-based recursive partitioning that
is particularly useful when you have a continuous variable such as age
in years and a large enough sample. It will test different ways to
partition the age variable to determine potential group differences
([Strobl, Kopf, and Zeileis 2015](#ref-strobl2015); [Strobl et al.
2021](#ref-strobl2021)).

``` r
RIdifTable(df, dif.age)
```

    [1] "No statistically significant DIF found."

No DIF found for age.

### Group

``` r
RIdifTable(df, dif.group)
```

    [1] "No statistically significant DIF found."

And no DIF for group.

### Sex and age

The `psychotree` package also allows for DIF interaction analysis with
multiple DIF variables. We can use
[`RIdifTable2()`](https://pgmj.github.io/easyRasch/reference/RIdifTable2.md)
to input two DIF variables.

``` r
RIdifTable2(df, dif.sex, dif.age)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-52-1.png)

|     Item |      2 |      3 | Mean location | StDev | MaxDiff |
|---------:|-------:|-------:|--------------:|------:|--------:|
| PANAS_11 | -0.314 | -0.196 |        -0.255 | 0.083 |   0.117 |
| PANAS_12 |  0.028 | -0.044 |        -0.008 | 0.051 |   0.073 |
| PANAS_13 |  0.553 |  0.402 |         0.478 | 0.107 |   0.151 |
| PANAS_14 | -0.328 | -0.183 |        -0.255 | 0.103 |   0.146 |
| PANAS_16 |  0.004 |  0.114 |         0.059 | 0.078 |   0.111 |
| PANAS_17 |  0.520 |  0.290 |         0.405 | 0.163 |   0.230 |
| PANAS_19 | -0.495 | -0.355 |        -0.425 | 0.099 |   0.140 |
| PANAS_20 |  0.032 | -0.028 |         0.002 | 0.042 |   0.059 |

No interaction effect found for sex and age. The analysis only shows the
previously identified DIF for sex.

### LRT-based DIF

We’ll use the group variable as an example. First, we can simply run the
test to get the overall result.

``` r
erm.out <- PCM(df)
LRtest(erm.out, splitcr = dif.group)
```

    Andersen LR-test:
    LR-value: 46.864
    Chi-square df: 31
    p-value:  0.034 

Review the documentation for further details, using
[`?LRtest`](https://rdrr.io/pkg/eRm/man/LRtest.html) in your R console
panel in Rstudio. There is also a plotting function,
[`plotGOF()`](https://rdrr.io/pkg/eRm/man/LRtest.html) that may be of
interest.

#### Item location table

``` r
RIdifTableLR(df, dif.group)
```

[TABLE]

#### Item location figure

``` r
RIdifFigureLR(df, dif.group) + theme_rise()
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-55-1.png)

#### Item threshold table

``` r
RIdifThreshTblLR(df, dif.group)
```

[TABLE]

#### Item threshold figure

``` r
RIdifThreshFigLR(df, dif.group) + theme_rise()
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-57-1.png)

The item threshold table shows that the top threshold for item 13
differs more than 0.5 logits between groups. In this set of 8 items with
4 thresholds each, it is unlikely to result in problematic differences
in estimated person scores.

### Logistic Ordinal Regression DIF

The `lordif` package ([Choi, Gibbons, and Crane
2011](#ref-choi_lordif_2011)) does not use a Rasch measurement model, it
only offers a choice between the Graded Response Model (GRM) and the
Generalized Partial Credit Model (GPCM). Both of these are 2PL models,
meaning that they estimate a discrimination parameter for each item in
addition to the item threshold parameters. `lordif` relies on the `mirt`
package.

There are several nice features available in the `lordif` package.
First, we get a χ2 test of uniform or non-uniform DIF. Second, there are
three possible methods/criteria for flagging items with potential DIF.
One of these uses a likelihood ratio (LR) χ2 test, while the other two
are indicators of DIF size/magnitude, either using a pseudo R2 statistic
(“McFadden”, “Nagelkerke”, or “CoxSnell”) or a Beta criterion. For
further details, see
[`?lordif`](https://rdrr.io/pkg/lordif/man/lordif.html) in your R
console or the paper describing the package ([Choi, Gibbons, and Crane
2011](#ref-choi_lordif_2011)).

Below is some sample code to get you started with `lordif`.

``` r
library(lordif)

g_dif <- lordif(as.data.frame(df), as.numeric(dif.sex), # make sure that the data is in a dataframe-object and that the DIF variable is numeric
                criterion = c("Chisqr"), 
                alpha = 0.01, 
                beta.change = 0.1,
                model = "GPCM",
                R2.change = 0.02)

g_dif_sum <- summary(g_dif)
```

``` r
# threshold values for colorizing the table below
alpha = 0.01
beta.change = 0.1
R2.change = 0.02

g_dif_sum$stats %>% 
  as.data.frame() %>% 
  select(!all_of(c("item","df12","df13","df23"))) %>% 
  round(3) %>% 
  add_column(itemnr = names(df), .before = "ncat") %>% 
  mutate(across(c(chi12,chi13,chi23), ~ cell_spec(.x,
                               color = case_when(
                                 .x < alpha ~ "red",
                                 TRUE ~ "black"
                               )))) %>%
  mutate(across(starts_with("pseudo"), ~ cell_spec(.x,
                               color = case_when(
                                 .x > R2.change ~ "red",
                                 TRUE ~ "black"
                               )))) %>%
  mutate(beta12 =  cell_spec(beta12,
                               color = case_when(
                                 beta12 > beta.change ~ "red",
                                 TRUE ~ "black"
                               ))) %>% 
  kbl_rise()
```

| itemnr | ncat | chi12 | chi13 | chi23 | beta12 | pseudo12.McFadden | pseudo13.McFadden | pseudo23.McFadden | pseudo12.Nagelkerke | pseudo13.Nagelkerke | pseudo23.Nagelkerke | pseudo12.CoxSnell | pseudo13.CoxSnell | pseudo23.CoxSnell |
|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| PANAS_11 | 5 | 0.323 | 0 | 0 | 0.002 | 0 | 0.004 | 0.003 | 0 | 0.005 | 0.005 | 0 | 0.005 | 0.005 |
| PANAS_12 | 5 | 0.013 | 0.019 | 0.192 | 0.008 | 0.001 | 0.002 | 0 | 0.001 | 0.002 | 0 | 0.001 | 0.002 | 0 |
| PANAS_13 | 5 | 0.106 | 0.057 | 0.077 | 0.007 | 0.001 | 0.001 | 0.001 | 0.001 | 0.002 | 0.001 | 0.001 | 0.002 | 0.001 |
| PANAS_14 | 5 | 0.182 | 0.401 | 0.83 | 0.002 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| PANAS_16 | 5 | 0.64 | 0.17 | 0.068 | 0.001 | 0 | 0.001 | 0.001 | 0 | 0.001 | 0.001 | 0 | 0.001 | 0.001 |
| PANAS_17 | 5 | 0 | 0 | 0.32 | 0.032 | 0.008 | 0.008 | 0 | 0.011 | 0.011 | 0 | 0.01 | 0.01 | 0 |
| PANAS_19 | 5 | 0.178 | 0.25 | 0.33 | 0.002 | 0 | 0 | 0 | 0 | 0.001 | 0 | 0 | 0.001 | 0 |
| PANAS_20 | 5 | 0.68 | 0.349 | 0.164 | 0.001 | 0 | 0 | 0 | 0 | 0.001 | 0.001 | 0 | 0.001 | 0.001 |

We can review the results regarding uniform/non-uniform DIF by looking
at the `chi*` columns. Uniform DIF is indicated by column `chi12` and
non-uniform DIF by `chi23`, while column `chi13` represents “an overall
test of”total DIF effect” ([Choi, Gibbons, and Crane
2011](#ref-choi_lordif_2011)).

While the table indicates significant chi2-tests for items 11 and 17,
the magnitude estimates are low for these items.

There are some plots available as well, using the base R
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) function. For
some reason the plots won’t render in this Quarto document, so I will
try to sort that out at some point.

``` r
plot(g_dif) # use option `graphics.off()` to get the plots rendered one by one
#plot(g_dif, graphics.off())
```

### Partial gamma DIF

The `iarm` package provides a function to assess DIF by partial gamma
([Bjorner et al. 1998](#ref-bjorner_differential_1998)). It should be
noted that this function only shows a single partial gamma value per
item, so if you have more than two groups in your comparison, you will
want to also use other methods to understand your results better.

There are some recommended cutoff-values mentioned in the paper above:

No or negligible DIF:

- Gamma within the interval -0.21 to 0.21, *or*
- Gamma not significantly different from 0

Slight to moderate DIF:

- Gamma within the interval -0.31 to 0.31 (and outside -0.21 to 0.21),
  *or*
- not significantly outside the interval -0.21 to 0.21

Moderate to large DIF:

- Gamma outside the interval -0.31 to 0.31, **and**
- significantly outside the interval -0.21 to 0.21

``` r
RIpartgamDIF(df, dif.sex)
```

| Item     | Partial gamma |    SE | Lower CI | Upper CI | Adjusted p-value (BH) |
|:---------|:--------------|------:|---------:|---------:|----------------------:|
| PANAS_17 | 0.234         | 0.054 |    0.127 |    0.341 |                     0 |

We can see “slight” DIF for item 17, with a statistically significant
gamma of .23.

## Rasch analysis 3

While there were no significant issues with DIF for any item/subgroup
combination, we need to address the previously identified problem:

- Items 16 and 19 have the largest residual correlation.

We’ll remove item 19 since item 16 has better targeting.

``` r
removed.items <- c(removed.items,"PANAS_19")

df_backup2 <- df

df <- df_backup2 %>% 
  select(!any_of(removed.items))
```

|  itemnr  | item       |
|:--------:|:-----------|
| PANAS_11 | Distressed |
| PANAS_12 | Upset      |
| PANAS_13 | Hostile    |
| PANAS_14 | Irritable  |
| PANAS_16 | Afraid     |
| PANAS_17 | Ashamed    |
| PANAS_20 | Jittery    |

### Item fit

``` r
simfit3 <- RIgetfit(df, iterations = 1000, cpu = 8)
RIitemfit(df, simfit3)
```

[TABLE]

### CICC

``` r
CICCplot(PCM(df),
         which.item = c(1:3,7),
         lower.groups = c(0,7,14,21,28),
         grid.items = TRUE)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-65-1.png)

### Residual correlations

``` r
simcor3 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor3$p99)
```

[TABLE]

### Targeting

``` r
RItargeting(df, bins = 45)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-67-1.png)

### Item hierarchy

``` r
RIitemHierarchy(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-68-1.png)

### Analysis 3 comments

No problematic residual correlations remaining. Several items show
misfit but we will end this sample analysis here and move on to show
other functions.

There are several item thresholds that are very closely located, as
shown in the item hierarchy figure. This is not ideal, since it will
inflate reliability estimates. However, we will not modify the response
categories for this analysis, we only note that this is not workable and
should be dealt with by trying variations of merged response categories
to achieve better separation of threshold locations without disordering.

## Reliability

``` r
RItif(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-69-1.png)

The figure above shows the Test Information Function (TIF), which
indicates the reliability of all items making up the test/scale (not the
reliability of the sample).

The default cutoff value used in
[`RItif()`](https://pgmj.github.io/easyRasch/reference/RItif.md) is TIF
= 3.33, which corresponds to person separation index (PSI) = 0.7. PSI is
similar to reliability coefficients such as omega and alpha, ranging
from 0 to 1. You can change the TIF cutoff by using the option `cutoff`,
for instance `cutoff = 2.5` (TIF values range from 1 and up).

While 11.8% of respondents had a floor effect based on the raw sum
scored data, the figure above shows us that 41.8% are located below the
point where the items produce a PSI of 0.7 or higher. Again, note that
this figure shows the reliability of the test/scale, not the sample. If
you want to add the sample reliability use option `samplePSI = TRUE`.
More details are available in the documentation
[`?RItif`](https://pgmj.github.io/easyRasch/reference/RItif.md).

## Person fit

We can also look at how the respondents fit the Rasch model with these
items. By default,
[`RIpfit()`](https://pgmj.github.io/easyRasch/reference/RIpfit.md)
outputs a histogram and a hex heatmap with the person infit ZSTD
statistic, using +/- 1.96 as cutoff values. This is currently the only
person fit method implemented in the `easyRasch` package, and the
curious analyst is suggested to look at the package
[PerFit](https://www.rdocumentation.org/packages/PerFit/versions/1.4.6/topics/PerFit-package)
for more tools.

``` r
RIpfit(df)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-70-1.png)

![](easyrasch-vignette_files/figure-html/unnamed-chunk-70-2.png)

You can export the person fit values to a new variable in the dataframe
by specifying `output = "dataframe"`, or if you just want the row
numbers for respondents with deviant infit values, `output = "rowid"`.

You can also specify a grouping variable to visualize the person fit for
different groups.

``` r
RIpfit(df, group = dif.sex, output = "heatmap")
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-71-1.png)

Person fit is a useful way to identify respondents with unexpected
response patterns and investigate this further.

### `PerFit` sample code

While none of the functions in the `PerFit` package has been implemented
in `easyRasch`, this is some code to get you started if you are
interested in using it. There are multiple methods/functions available
for polytomous and dichotomous data, see the package
[documentation](https://www.rdocumentation.org/packages/PerFit/versions/1.4.6/topics/PerFit-package).

For this example, we’ll use the non-parametric U3 statistic generalized
to polytomous items ([Emons 2008](#ref-emons_nonparametric_2008)).

#### U3poly

``` r
library(PerFit)
pfit_u3poly <- U3poly(matrix = df, 
                      Ncat = 5, # make sure to input number of response categories, not thresholds
                      IRT.PModel = "PCM")
```

#### Cutoff information

``` r
cutoff(pfit_u3poly)
```

    $Cutoff
    [1] 0.4013

    $Cutoff.SE
    [1] 0.0233

    $Prop.flagged
    [1] 0.1059

    $Tail
    [1] "upper"

    $Cutoff.CI
      2.5%  97.5%
    0.3644 0.4695

    attr(,"class")
    [1] "PerFit.cutoff"

#### Flagged respondents

``` r
flagged.resp(pfit_u3poly) %>% 
  pluck("Scores") %>% 
  as.data.frame() %>% 
  arrange(desc(PFscores))
```

        FlaggedID It1 It4 It7 It2 It5 It3 It6 PFscores
    1         159   0   0   0   0   0   0   1   1.0000
    2         168   0   0   0   0   0   0   1   1.0000
    3         193   0   0   0   0   0   0   1   1.0000
    4         214   0   0   0   0   0   0   1   1.0000
    5         222   0   0   0   0   0   4   0   1.0000
    6         324   0   0   0   0   0   0   1   1.0000
    7         667   0   0   0   0   0   0   1   1.0000
    8        1014   0   0   0   0   0   4   0   1.0000
    9        1146   0   0   0   0   0   0   1   1.0000
    10       1171   0   0   0   0   0   0   1   1.0000
    11       1609   0   0   0   0   0   0   2   1.0000
    12        555   0   0   0   0   0   3   0   0.9484
    13        782   0   0   0   0   0   3   0   0.9484
    14       1015   0   0   0   0   0   3   0   0.9484
    15        187   0   4   2   4   4   4   4   0.9089
    16       1290   0   1   0   0   0   4   0   0.9020
    17        225   0   0   0   0   0   2   0   0.8939
    18        939   0   0   0   0   0   2   0   0.8939
    19       1364   0   0   0   0   0   2   0   0.8939
    20       1738   0   0   0   0   0   2   0   0.8939
    21       1770   0   0   0   0   0   2   0   0.8939
    22       1021   0   1   0   0   0   0   4   0.8880
    23         26   0   0   0   0   0   1   0   0.8476
    24        596   0   0   0   0   0   1   0   0.8476
    25        902   0   0   0   0   0   1   0   0.8476
    26       1098   0   0   0   0   0   1   0   0.8476
    27       1230   0   0   0   0   0   1   0   0.8476
    28       1418   0   0   0   0   0   1   0   0.8476
    29       1475   0   0   0   0   0   1   0   0.8476
    30       1847   0   0   0   0   0   1   0   0.8476
    31       1036   0   2   0   4   4   0   4   0.8127
    32         44   4   0   4   0   0   4   0   0.7964
    33       1075   0   3   0   4   1   4   4   0.7765
    34       1472   4   0   0   0   0   0   4   0.7624
    35        463   0   0   4   0   0   0   0   0.7439
    36        488   0   0   4   0   0   0   0   0.7439
    37       1421   0   0   4   0   0   0   0   0.7439
    38       1531   4   4   0   4   0   4   0   0.7272
    39        971   0   0   0   0   3   0   0   0.7021
    40       1774   0   0   0   4   4   1   3   0.7009
    41        446   0   4   2   4   4   3   4   0.6770
    42       1673   1   0   0   0   4   0   0   0.6588
    43        814   2   0   0   0   1   4   0   0.6560
    44        701   0   0   0   0   2   0   0   0.6536
    45       1578   0   0   0   0   2   0   0   0.6536
    46        487   0   4   2   4   4   1   4   0.6504
    47       1258   0   0   3   0   0   0   0   0.6198
    48       1681   0   0   3   0   0   0   0   0.6198
    49        571   0   0   0   2   0   0   0   0.6101
    50       1402   0   0   0   2   0   0   0   0.6101
    51       1682   0   0   0   2   0   0   0   0.6101
    52        597   0   4   0   0   0   0   0   0.6095
    53        994   1   0   0   0   4   0   1   0.6088
    54        563   0   2   0   4   0   0   0   0.6067
    55        721   4   0   0   0   0   0   0   0.5996
    56       1159   4   0   0   0   0   0   0   0.5996
    57       1602   4   0   0   0   0   0   0   0.5996
    58       1797   4   0   0   0   0   0   0   0.5996
    59       1170   4   4   4   0   0   0   0   0.5993
    60       1287   4   2   0   4   4   0   0   0.5962
    61       1006   0   3   0   4   4   2   0   0.5930
    62       1383   0   4   1   4   0   3   2   0.5925
    63        541   0   4   4   4   2   2   4   0.5887
    64        356   0   0   0   0   1   0   0   0.5825
    65        470   0   0   0   0   1   0   0   0.5825
    66        567   0   0   0   0   1   0   0   0.5825
    67        623   0   0   0   0   1   0   0   0.5825
    68        659   0   0   0   0   1   0   0   0.5825
    69        710   0   0   0   0   1   0   0   0.5825
    70        987   0   0   0   0   1   0   0   0.5825
    71       1095   0   0   0   0   1   0   0   0.5825
    72       1106   0   0   0   0   1   0   0   0.5825
    73       1268   0   0   0   0   1   0   0   0.5825
    74       1271   0   0   0   0   1   0   0   0.5825
    75       1276   0   0   0   0   1   0   0   0.5825
    76       1480   0   0   0   0   1   0   0   0.5825
    77       1763   0   0   0   0   1   0   0   0.5825
    78        576   0   0   2   0   0   3   0   0.5814
    79        690   3   0   0   3   3   0   4   0.5802
    80        727   3   0   2   0   0   0   4   0.5778
    81        916   0   0   1   0   0   0   3   0.5768
    82        712   3   0   0   0   0   3   0   0.5731
    83        799   1   4   0   4   0   0   0   0.5687
    84        754   0   1   0   0   0   0   3   0.5674
    85        717   4   4   4   4   4   0   0   0.5585
    86        472   1   0   0   1   4   0   0   0.5560
    87        173   3   4   0   4   0   0   0   0.5501
    88        851   0   0   1   0   1   0   3   0.5494
    89        227   0   0   0   0   1   0   1   0.5386
    90        409   0   0   0   0   1   0   1   0.5386
    91       1192   0   1   0   0   0   3   0   0.5360
    92        247   4   0   1   0   0   0   0   0.5295
    93       1758   1   4   0   4   3   3   3   0.5229
    94       1703   0   1   0   0   0   0   2   0.5109
    95        531   4   1   3   0   4   3   3   0.5095
    96        622   0   3   0   0   0   0   0   0.5088
    97        762   0   3   0   0   0   0   0   0.5088
    98        981   4   0   4   3   1   0   0   0.5079
    99        278   1   0   0   2   0   0   3   0.5076
    100       584   0   0   0   1   0   0   1   0.5020
    101      1045   0   0   0   1   0   0   1   0.5020
    102      1191   0   0   0   1   0   0   1   0.5020
    103      1556   0   1   0   1   0   3   0   0.4993
    104       820   2   0   0   0   0   2   3   0.4985
    105       481   1   4   3   2   0   3   4   0.4973
    106        38   4   3   0   0   0   0   0   0.4895
    107       728   3   4   4   1   3   0   4   0.4889
    108        56   0   0   0   1   0   0   0   0.4844
    109       183   0   0   0   1   0   0   0   0.4844
    110       209   0   0   0   1   0   0   0   0.4844
    111       331   0   0   0   1   0   0   0   0.4844
    112       410   0   0   0   1   0   0   0   0.4844
    113       797   0   0   0   1   0   0   0   0.4844
    114       886   0   0   0   1   0   0   0   0.4844
    115      1067   0   0   0   1   0   0   0   0.4844
    116      1269   0   0   0   1   0   0   0   0.4844
    117      1558   0   0   0   1   0   0   0   0.4844
    118      1663   0   0   0   1   0   0   0   0.4844
    119      1680   0   0   0   1   0   0   0   0.4844
    120      1726   0   0   0   1   0   0   0   0.4844
    121       197   0   0   0   0   1   1   0   0.4817
    122       562   1   0   3   0   0   3   0   0.4802
    123       451   3   4   0   0   3   0   3   0.4800
    124      1583   0   0   4   2   3   1   1   0.4791
    125      1214   0   2   2   3   4   2   4   0.4754
    126        73   3   3   0   2   0   4   2   0.4732
    127       702   0   3   0   1   0   3   0   0.4732
    128      1642   4   0   1   0   0   2   0   0.4725
    129       527   0   0   1   0   0   2   0   0.4706
    130       857   0   0   1   0   0   2   0   0.4706
    131      1311   0   0   1   0   0   2   0   0.4706
    132       434   2   0   4   0   1   0   0   0.4691
    133       788   4   2   2   4   1   0   4   0.4680
    134       606   1   2   2   0   0   0   4   0.4663
    135      1601   3   0   0   0   0   3   2   0.4601
    136      1550   3   3   0   3   4   1   4   0.4572
    137        96   3   0   0   0   0   0   0   0.4554
    138       352   3   0   0   0   0   0   0   0.4554
    139       755   3   0   0   0   0   0   0   0.4554
    140       830   3   0   0   0   0   0   0   0.4554
    141      1060   3   0   0   0   0   0   0   0.4554
    142      1374   3   0   0   0   0   0   0   0.4554
    143      1714   3   0   0   0   0   0   0   0.4554
    144      1745   3   0   0   0   0   0   0   0.4554
    145       166   0   1   0   0   0   2   0   0.4551
    146       684   0   0   0   2   3   0   1   0.4546
    147      1019   0   0   0   1   3   0   1   0.4546
    148      1800   1   0   1   0   0   3   0   0.4531
    149         8   0   0   2   0   0   0   0   0.4496
    150        54   0   0   2   0   0   0   0   0.4496
    151        82   0   0   2   0   0   0   0   0.4496
    152       343   0   0   2   0   0   0   0   0.4496
    153       401   0   0   2   0   0   0   0   0.4496
    154       538   0   0   2   0   0   0   0   0.4496
    155       603   0   0   2   0   0   0   0   0.4496
    156       914   0   0   2   0   0   0   0   0.4496
    157      1132   0   0   2   0   0   0   0   0.4496
    158      1144   0   0   2   0   0   0   0   0.4496
    159      1193   0   0   2   0   0   0   0   0.4496
    160      1239   0   0   2   0   0   0   0   0.4496
    161      1493   0   0   2   0   0   0   0   0.4496
    162      1532   0   0   2   0   0   0   0   0.4496
    163      1606   0   0   2   0   0   0   0   0.4496
    164      1747   0   0   2   0   0   0   0   0.4496
    165      1752   0   0   2   0   0   0   0   0.4496
    166      1768   0   0   2   0   0   0   0   0.4496
    167      1822   0   0   2   0   0   0   0   0.4496
    168      1176   0   0   0   1   0   1   0   0.4451
    169        62   1   1   0   0   0   3   0   0.4442
    170      1503   4   4   3   4   0   3   1   0.4432
    171        53   0   1   4   1   1   0   0   0.4428
    172       505   3   4   4   0   1   0   0   0.4424
    173       129   0   0   3   2   0   0   0   0.4398
    174       221   0   4   0   3   1   1   0   0.4395
    175       114   0   0   2   0   3   0   0   0.4386
    176       658   0   0   2   0   3   0   0   0.4386
    177       207   3   0   3   4   4   0   2   0.4385
    178      1447   1   4   0   0   1   0   0   0.4358
    179       959   0   3   3   0   0   0   0   0.4294
    180       428   4   4   0   3   3   2   4   0.4281
    181       500   2   4   0   4   3   3   0   0.4243
    182      1666   3   1   4   0   3   0   0   0.4238
    183       115   0   1   0   1   1   3   0   0.4224

The dataframe shown under the tab `Flagged respondents` above contains a
variable named `FlaggedID` which represents the row id’s. This variable
is useful if one wants to filter out respondents with deviant response
patterns (person misfit). There are indications that persons with misfit
may affect results of Andersen’s LR-test for DIF ([Artner
2016](#ref-artner_simulation_2016)).

### Item fit without aberrant responses

We can remove the misfitting persons to see how that affects item fit.
Let’s also compare with the misfitting respondents identified by
[`RIpfit()`](https://pgmj.github.io/easyRasch/reference/RIpfit.md).

``` r
misfits <- flagged.resp(pfit_u3poly) %>% 
  pluck("Scores") %>% 
  as.data.frame() %>% 
  pull(FlaggedID)

misfits2 <- RIpfit(df, output = "rowid")
```

#### All respondents

``` r
RIitemfit(df, simcut = simfit3)
```

[TABLE]

#### U3 misfit removed

``` r
RIitemfit(df[-misfits,], simcut = simfit3)
```

[TABLE]

#### ZSTD misfit removed

``` r
RIitemfit(df[-misfits2,], simcut = simfit3)
```

[TABLE]

## Item parameters

To allow others (and oneself) to use the item parameters estimated for
estimation of person locations/thetas, we should make the item
parameters available. The function will also write a csv-file with the
item threshold locations. Estimations of person locations/thetas can be
done with the [`thetaEst()`](https://rdrr.io/pkg/catR/man/thetaEst.html)
function from the `catR` package. This is implemented in the function
[`RIestThetasOLD()`](https://pgmj.github.io/easyRasch/reference/RIestThetasOLD.md),
see below for details.

First, we’ll output the parameters into a table.

``` r
RIitemparams(df)
```

|  | Threshold 1 | Threshold 2 | Threshold 3 | Threshold 4 | Item location |
|:---|---:|---:|---:|---:|:---|
| PANAS_11 | -1.63 | -0.67 | -0.22 | 1.21 | -0.33 |
| PANAS_12 | -0.80 | -0.39 | 0.06 | 0.89 | -0.06 |
| PANAS_13 | -0.29 | -0.15 | 0.56 | 1.53 | 0.42 |
| PANAS_14 | -1.38 | -0.59 | -0.16 | 0.87 | -0.32 |
| PANAS_16 | -0.59 | -0.44 | -0.06 | 1.08 | 0 |
| PANAS_17 | -0.06 | 0.01 | 0.48 | 0.93 | 0.34 |
| PANAS_20 | -1.27 | -0.61 | 0.35 | 1.32 | -0.05 |
| Note: |  |  |  |  |  |
|  Item location is the average of the thresholds for each item. |  |  |  |  |  |

The parameters can also be output to a dataframe or a file, using the
option `output = "dataframe"` or `output = "file"`.

## Ordinal sum score to interval score

This table shows the corresponding “raw” ordinal sum score values and
logit scores, with standard errors for each logit value. Interval scores
are estimated using WL based on a simulated dataset using the item
parameters estimated from the input dataset. The choice of WL as default
is due to the lower bias compared to ML estimation ([Warm
1989](#ref-warm1989)).

(An option will hopefully be added at some point to create this table
based on only item parameters.)

``` r
RIscoreSE(df)
```

| Ordinal sum score | Logit score | Logit std.error |
|------------------:|------------:|----------------:|
|                 0 |      -3.642 |           0.620 |
|                 1 |      -2.543 |           0.716 |
|                 2 |      -2.032 |           0.653 |
|                 3 |      -1.693 |           0.578 |
|                 4 |      -1.437 |           0.515 |
|                 5 |      -1.228 |           0.468 |
|                 6 |      -1.050 |           0.432 |
|                 7 |      -0.893 |           0.406 |
|                 8 |      -0.750 |           0.386 |
|                 9 |      -0.618 |           0.371 |
|                10 |      -0.493 |           0.361 |
|                11 |      -0.373 |           0.353 |
|                12 |      -0.258 |           0.348 |
|                13 |      -0.144 |           0.346 |
|                14 |      -0.031 |           0.346 |
|                15 |       0.083 |           0.348 |
|                16 |       0.198 |           0.352 |
|                17 |       0.317 |           0.359 |
|                18 |       0.441 |           0.368 |
|                19 |       0.572 |           0.381 |
|                20 |       0.712 |           0.397 |
|                21 |       0.866 |           0.418 |
|                22 |       1.037 |           0.447 |
|                23 |       1.233 |           0.484 |
|                24 |       1.464 |           0.534 |
|                25 |       1.747 |           0.601 |
|                26 |       2.118 |           0.681 |
|                27 |       2.666 |           0.745 |
|                28 |       3.804 |           0.638 |

### Ordinal/interval figure

The figure below can also be generated to illustrate the relationship
between ordinal sum score and logit interval score. The errorbars
default to show the standard error at each point, multiplied by 1.96.

``` r
RIscoreSE(df, output = "figure")
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-81-1.png)

### Estimating interval level person scores

Based on the Rasch analysis output of item parameters, we can estimate
each individuals location or score (also known as “theta”).
[`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md)
by default uses WLE estimation based on item parameters from a partial
credit model (PCM) and outputs a dataframe with person locations (WLE)
and measurement error (SEM) on the logit scale.

``` r
thetas <- RIestThetas(df)

head(thetas)
```

             WLE       SEM
    1 -2.5430672 0.7160122
    2 -2.0319918 0.6533338
    3 -0.1439296 0.3460439
    4 -3.6420132 0.6202853
    5 -2.0319918 0.6533338
    6  0.1980813 0.3520345

Each individual has a standard error of measurement (SEM) associated
with their estimated location/score. This is included in the output of
the
[`RIestThetas()`](https://pgmj.github.io/easyRasch/reference/RIestThetas.md)
function as the `SEM` variable, as seen above. We can review the
distribution of measurement error with a figure.

We can take a look at the distribution of person locations (thetas)
using a histogram.

``` r
hist(thetas$WLE, 
     col = "#009ca6", 
     main = "Histogram of person locations (thetas)", 
     breaks = 20)
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-83-1.png)

[`RIestThetasOLD()`](https://pgmj.github.io/easyRasch/reference/RIestThetasOLD.md)
can be used with a pre-specified item (threshold) location matrix. The
choice of WL as default is due to the lower bias compared to ML
estimation ([Warm 1989](#ref-warm1989)). Similarly to
[`RIscoreSE()`](https://pgmj.github.io/easyRasch/reference/RIscoreSE.md)
you can (and may indeed need to) change the range of logit scores, using
the option `theta_range`. The default is `c(-7,7)`, which should
hopefully work in most circumstances.

If you would like to use an existing item threshold location matrix,
this code may be helpful:

``` r
itemParameters <- read_csv("itemParameters.csv") %>% 
  as.matrix()
```

This creates a matrix object (not a dataframe), with each item as a row,
and the threshold locations as columns.

## Figure design

Most of the figures created by the functions can be styled (colors,
fonts, etc) by adding theme settings to them. You can use the standard
ggplot function
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) and
related theme-functions. As usual it is possible to “stack” theme
functions, as seen in the example below.

You can also change coloring, axis limits/breaks, etc, just by adding
ggplot options with a `+` sign.

A custom theme function,
[`theme_rise()`](https://pgmj.github.io/easyRasch/reference/theme_rise.md),
is included in the `easyRasch` package. It might be easier to use if you
are not familiar with
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

For instance, you might like to change the font to “Lato” for the item
hierarchy figure, and make the background transparent.

``` r
RIitemHierarchy(df) +
  theme_minimal() + # first apply the minimal theme to make the background transparent
  theme_rise(fontfamily = "Lato") # then apply theme_rise, which simplifies making changes to all plot elements
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-85-1.png)

As of package version 0.1.30.0, the
[`RItargeting()`](https://pgmj.github.io/easyRasch/reference/RItargeting.md)
function allows more flexibility in styling too, by having an option to
return a list object with the three separate plots. See the
[NEWS](https://github.com/pgmj/easyRasch/blob/main/NEWS.md#01300) file
for more details. Since the
[`RItargeting()`](https://pgmj.github.io/easyRasch/reference/RItargeting.md)
function uses the `patchwork` library to combine plots, you can also
make use of [the many functions that `patchwork`
includes](https://patchwork.data-imaginist.com/articles/patchwork.html).
For instance, you can set a title with a specific theme:

``` r
RItargeting(df) + plot_annotation(title = "Targeting", theme = theme_rise(fontfamily = "Arial"))
```

![](easyrasch-vignette_files/figure-html/unnamed-chunk-86-1.png)

In order to change font for text *inside* plots (such as “t1” for
thresholds) you will need to add an additional line of code.

``` r
update_geom_defaults("text", list(family = "Lato"))
```

Please note that the line of code above updates the default settings for
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
for the whole session. Also, some functions, such as
[`RIloadLoc()`](https://pgmj.github.io/easyRasch/reference/RIloadLoc.md),
make use of
[`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
for which you would need to change the code above from “text” to
“text_repel”.

A simple way to only change font family and font size would be to use
`theme_minimal(base_family = "Calibri", base_size = 14)`. Please see the
[reference page](https://ggplot2.tidyverse.org/reference/ggtheme.html)
for default ggplot themes for alternatives to
[`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Software used

The `grateful` package is a nice way to give credit to the packages used
in making the analysis. The package can create both a bibliography file
and a table object, which is handy for automatically creating a
reference list based on the packages used (or at least explicitly
loaded).

``` r
library(grateful)
pkgs <- cite_packages(cite.tidyverse = TRUE, 
                      output = "table",
                      bib.file = "grateful-refs.bib",
                      include.RStudio = TRUE,
                      out.dir = getwd())
# If kbl() is used to generate this table, the references will not be added to the Reference list.
formattable(pkgs, 
            table.attr = 'class=\"table table-striped\" style="font-size: 13px; font-family: Lato; width: 80%"')
```

| Package | Version |                        Citation |
|--------:|--------:|--------------------------------:|
|    base |   4.4.2 | R Core Team ([2024](#ref-base)) |

## Additional credits

Thanks to my [colleagues at
RISE](https://www.ri.se/en/what-we-do/projects/center-for-category-based-measurements)
for providing feedback and testing the package on Windows and MacOS
platforms. Also, thanks to [Mike
Linacre](https://www.winsteps.com/linacre.htm) and [Jeanette
Melin](https://www.ri.se/en/person/jeanette-melin) for providing useful
feedback to improve this vignette.

## Session info

``` r
sessionInfo()
```

    R version 4.4.2 (2024-10-31)
    Platform: aarch64-apple-darwin20
    Running under: macOS Sequoia 15.2

    Matrix products: default
    BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib
    LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

    time zone: Europe/Stockholm
    tzcode source: internal

    attached base packages:
     [1] parallel  grid      stats4    stats     graphics  grDevices utils
     [8] datasets  methods   base

    other attached packages:
     [1] PerFit_1.4.6      ltm_1.2-0         polycor_0.8-1     msm_1.7.1
     [5] MASS_7.3-61       lordif_0.3-3      rms_6.8-1         Hmisc_5.2-0
     [9] RASCHplot_0.1.0   knitr_1.48        readxl_1.4.3      car_3.1-2
    [13] carData_3.0-5     grateful_0.2.4    easyRasch_0.3.3   doParallel_1.0.17
    [17] iterators_1.0.14  furrr_0.3.1       future_1.34.0     foreach_1.5.2
    [21] ggdist_3.3.2      janitor_2.2.0     iarm_0.4.3        hexbin_1.28.4
    [25] catR_3.17         glue_1.8.0        ggrepel_0.9.6     patchwork_1.3.0
    [29] reshape_0.8.9     matrixStats_1.4.1 psychotree_0.16-1 psychotools_0.7-4
    [33] partykit_1.2-22   mvtnorm_1.3-1     libcoin_1.0-10    psych_2.4.6.26
    [37] mirt_1.43         lattice_0.22-6    eRm_1.0-6         lubridate_1.9.3
    [41] forcats_1.0.0     stringr_1.5.1     dplyr_1.1.4       purrr_1.0.2
    [45] readr_2.1.5       tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.1
    [49] tidyverse_2.0.0   kableExtra_1.4.0  formattable_0.2.1

    loaded via a namespace (and not attached):
      [1] bitops_1.0-8         RColorBrewer_1.1-3   tools_4.4.2
      [4] backports_1.5.0      utf8_1.2.4           R6_2.5.1
      [7] DT_0.33              vegan_2.6-8          sm_2.2-6.0
     [10] mgcv_1.9-1           permute_0.9-7        withr_3.0.2
     [13] gridExtra_2.3        progressr_0.14.0     quantreg_5.98
     [16] cli_3.6.3            sandwich_3.1-1       labeling_0.4.3
     [19] polspline_1.1.25     pbapply_1.7-2        systemfonts_1.1.0
     [22] foreign_0.8-87       relimp_1.0-5         svglite_2.1.3
     [25] R.utils_2.12.3       parallelly_1.38.0    sessioninfo_1.2.2
     [28] rstudioapi_0.17.1    generics_0.1.3       vroom_1.6.5
     [31] distributional_0.4.0 qvcalc_1.0.3         Matrix_1.7-1
     [34] fansi_1.0.6          abind_1.4-5          R.methodsS3_1.8.2
     [37] lifecycle_1.0.4      multcomp_1.4-26      yaml_2.3.10
     [40] snakecase_0.11.1     inum_1.0-5           ggstance_0.3.7
     [43] irtoys_0.2.2         promises_1.3.2       crayon_1.5.3
     [46] cowplot_1.1.3        pillar_1.9.0         fda_6.1.8
     [49] vcdExtra_0.8-5       future.apply_1.11.2  admisc_0.35
     [52] codetools_0.2-20     beepr_2.0            data.table_1.16.0
     [55] vcd_1.4-12           vctrs_0.6.5          testthat_3.2.1.1
     [58] cellranger_1.1.0     gtable_0.3.5         cachem_1.1.0
     [61] ks_1.14.2            xfun_0.46            mime_0.12
     [64] fds_1.8              pracma_2.4.4         pcaPP_2.0-4
     [67] survival_3.7-0       audio_0.1-11         RPushbullet_0.3.4
     [70] TH.data_1.1-2        nlme_3.1-166         bit64_4.0.5
     [73] rprojroot_2.0.4      Deriv_4.1.3          KernSmooth_2.23-24
     [76] rpart_4.1.23         colorspace_2.1-1     gnm_1.1-5
     [79] nnet_7.3-19          mnormt_2.1.1         tidyselect_1.2.1
     [82] bit_4.0.5            compiler_4.4.2       curl_6.0.1
     [85] htmlTable_2.4.3      SparseM_1.84-2       expm_0.999-9
     [88] xml2_1.3.6           checkmate_2.3.2      scales_1.3.0
     [91] lmtest_0.9-40        digest_0.6.37        rainbow_3.8
     [94] rmarkdown_2.28       ca_0.71.1            htmltools_0.5.8.1
     [97] pkgconfig_2.0.3      base64enc_0.1-3      SimDesign_2.17.1
    [100] fastmap_1.2.0        rlang_1.1.4          htmlwidgets_1.6.4
    [103] shiny_1.10.0         farver_2.1.2         zoo_1.8-12
    [106] jsonlite_1.8.9       mclust_6.1.1         dcurver_0.9.2
    [109] R.oo_1.26.0          RCurl_1.98-1.16      magrittr_2.0.3
    [112] Formula_1.2-5        munsell_0.5.1        Rcpp_1.0.13-1
    [115] stringi_1.8.4        brio_1.1.5           plyr_1.8.9
    [118] listenv_0.9.1        splines_4.4.2        hms_1.1.3
    [121] ggpubr_0.6.0         ggsignif_0.6.4       reshape2_1.4.4
    [124] GPArotation_2024.3-1 evaluate_1.0.1       renv_1.0.7
    [127] deSolve_1.40         tzdb_0.4.0           httpuv_1.6.15
    [130] MatrixModels_0.5-3   broom_1.0.7          xtable_1.8-4
    [133] rstatix_0.7.2        later_1.4.0          viridisLite_0.4.2
    [136] snow_0.4-4           memoise_2.0.1        cluster_2.1.6
    [139] corrplot_0.92        timechange_0.3.0     globals_0.16.3
    [142] hdrcde_3.4           here_1.0.1          

## References

Anvari, Farid, Emir Efendić, Jerome Olsen, Ruben C. Arslan, Malte Elson,
and Iris K. Schneider. 2022. “Bias in Self-Reports: An Initial Elevation
Phenomenon.” *Social Psychological and Personality Science*, October,
19485506221129160. <https://doi.org/10.1177/19485506221129160>.

Artner, Richard. 2016. “A Simulation Study of Person-Fit in the Rasch
Model.” *Psychological Test and Assessment Modeling* 58 (3): 531–63.

Bjorner, Jakob B., Svend Kreiner, John E. Ware, Mogens T. Damsgaard, and
Per Bech. 1998. “Differential Item Functioning in the Danish Translation
of the SF-36.” *Journal of Clinical Epidemiology* 51 (11): 1189–1202.
<https://doi.org/10.1016/S0895-4356(98)00111-5>.

Buchardt, Ann-Sophie, Karl Bang Christensen, and Normann Jensen. 2023.
“Visualizing Rasch Item Fit Using Conditional Item Characteristic Curves
in R.” *Psychological Test and Assessment Modeling* 65 (2): 206–19.

Choi, Seung W., Laura E. Gibbons, and Paul K. Crane. 2011. “Lordif: An R
Package for Detecting Differential Item Functioning Using Iterative
Hybrid Ordinal Logistic Regression/Item Response Theory and Monte Carlo
Simulations.” *Journal of Statistical Software* 39 (1): 1–30.
<https://doi.org/10.18637/jss.v039.i08>.

Christensen, Karl Bang, Guido Makransky, and Mike Horton. 2017.
“Critical Values for Yen’s Q3: Identification of Local Dependence in the
Rasch Model Using Residual Correlations.” *Applied Psychological
Measurement* 41 (3): 178–94. <https://doi.org/10.1177/0146621616677520>.

Emons, Wilco H. M. 2008. “Nonparametric Person-Fit Analysis of
Polytomous Item Scores.” *Applied Psychological Measurement* 32 (3):
224–47. <https://doi.org/10.1177/0146621607302479>.

Henninger, Mirka, Jan Radek, Marie-Ann Sengewald, and Carolin Strobl.
2024. “Partial Credit Trees Meet the Partial Gamma Coefficient for
Quantifying DIF and DSF in Polytomous Items.” OSF.
<https://doi.org/10.31234/osf.io/47sah>.

Johansson, Magnus, Marit Preuter, Simon Karlsson, Marie-Louise
Möllerberg, Hanna Svensson, and Jeanette Melin. 2023. “Valid and
Reliable? Basic and Expanded Recommendations for Psychometric Reporting
and Quality Assessment.” <https://doi.org/10.31219/osf.io/3htzc>.

Kreiner, Svend. 2011. “A Note on Item–Restscore Association in Rasch
Models.” *Applied Psychological Measurement* 35 (7): 557–61.
<https://doi.org/10.1177/0146621611410227>.

Kreiner, Svend, and Karl Bang Christensen. 2004. “Analysis of Local
Dependence and Multidimensionality in Graphical Loglinear Rasch Models.”
*Communications in Statistics - Theory and Methods* 33 (6): 1239–76.
<https://doi.org/10.1081/STA-120030148>.

Mueller, Marianne, and Pedro Henrique Ribeiro Santiago. 2022. “Iarm:
Item Analysis in Rasch Models.”
<https://cran.r-project.org/web/packages/iarm/index.html>.

Müller, Marianne. 2020. “Item Fit Statistics for Rasch Analysis: Can We
Trust Them?” *Journal of Statistical Distributions and Applications* 7
(1): 5. <https://doi.org/10.1186/s40488-020-00108-7>.

Ostini, Remo, and Michael Nering. 2006. *Polytomous Item Response Theory
Models*. 2455 Teller Road, Thousand Oaks California 91320 United States
of America: SAGE Publications, Inc.
<https://doi.org/10.4135/9781412985413>.

R Core Team. 2024. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

Smith, R. M., R. E. Schumacker, and M. J. Bush. 1998. “[Using Item Mean
Squares to Evaluate Fit to the Rasch
Model](https://www.ncbi.nlm.nih.gov/pubmed/9661732).” *Journal of
Outcome Measurement* 2 (1): 66–78.

Strobl, Carolin, Julia Kopf, and Achim Zeileis. 2015. “Rasch Trees: A
New Method for Detecting Differential Item Functioning in the Rasch
Model.” *Psychometrika* 80 (2): 289–316.
<https://doi.org/10.1007/s11336-013-9388-3>.

Strobl, Carolin, Lennart Schneider, Julia Kopf, and Achim Zeileis. 2021.
“Using the Raschtree Function for Detecting Differential Item
Functioning in the Rasch Model,” 12.

Warm, Thomas A. 1989. “Weighted Likelihood Estimation of Ability in Item
Response Theory.” *Psychometrika* 54 (3): 427–50.
<https://doi.org/10.1007/BF02294627>.
