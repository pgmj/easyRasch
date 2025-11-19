---
title: "easyRasch package vignette"
subtitle: "Simplifying Rasch Measurement Theory in R"
author: 
  name: 'Magnus Johansson'
  affiliation: 'RISE Research Institutes of Sweden'
  affiliation-url: 'https://www.ri.se/en/shic'
  orcid: '0000-0003-1669-592X'
date: last-modified
execute: 
  echo: true
  cache: true
  warning: false
  message: false
editor_options: 
  chunk_output_type: console
format: 
  html:
    toc: true
bibliography: 
  - references.bib
  - grateful-refs.bib
vignette: >
  %\VignetteIndexEntry{'Vignette'}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{quarto::html}
---

## Preamble

This page contains a sample Rasch analysis to showcase most of the core functions in the `easyRasch` package. A more extensive guide that also helps to show the benefits of the Quarto publishing system, is available here: <https://pgmj.github.io/raschrvignette/RaschRvign.html>

It is strongly recommended to go to the link above. I've become rather addicted to the convenience of panel-tabsets that Quarto offers that allow side-by-side viewing of output. These are unfortunately 
not available when generating this package article/vignette using `pkgdown` (AFAIK). Lacking panel-tabsets, this page will become quite long (not to say that the other one is short either...).

**NOTE: This package was previously known as `RISEkbmRasch`**

If you are new to Rasch Measurement Theory, you may find this intro presentation useful:
<https://pgmj.github.io/RaschIRTlecture/slides.html>

There is a separate GitHub repository containing a template R-project to simplify getting started with the `easyRasch` package and conducting a reproducible Rasch analysis in R: <https://github.com/pgmj/RISEraschTemplate>

## Introduction

This vignette will walk through a sample analysis using an open dataset with polytomous questionnaire data.

One of the aims with this package is to simplify reproducible psychometric analysis to shed light on the measurement properties of a scale, questionnaire or test. In a preprint [@johansson], our [research group](https://www.ri.se/en/what-we-do/projects/center-for-categorically-based-measurements) propose that the basic aspects of a psychometric analysis should include information about:

-   Unidimensionality & local independence
-   Response categories (monotonicity)
-   Invariance (Differential Item Functioning)
-   Targeting
-   Measurement uncertainties (reliability)

We'll include several ways to investigate these measurement properties using Rasch Measurement Theory. There are also functions in the package less directly related to the criteria above that will be demonstrated in this vignette.

Please note that this is just a sample analysis to showcase the R package. It is not intended as a "best practice" psychometric analysis example.

You can skip ahead to the Rasch analysis part in @sec-rasch if you are eager to look at the package output :)

## Getting started

Since the package is intended for use with Quarto, this vignette has also been created with Quarto. A "template" .qmd file [is available](https://github.com/pgmj/RISEraschTemplate/blob/main/analysis.qmd) that can be useful to have handy for copy&paste when running a new analysis.  You can also download a complete copy of the Quarto/R code to produce this document [here](https://github.com/pgmj/pgmj.github.io/blob/main/raschrvignette/RaschRvign.qmd).

Loading the `easyRasch` package will automatically load all the packages it depends on. However, it could be desirable to explicitly load all packages used to enable the automatic creation of citations for them, using the `grateful` package (see @sec-grateful).

::: {.cell}

```{.r .cell-code}
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
:::

### Loading data

We will use data from a recent paper investigating the "initial elevation effect" [@anvari2022], and focus on the 10 negative items from the PANAS. The data is available at the OSF website.

::: {.cell}

```{.r .cell-code}
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
:::

The `glimpse()` function provides a quick overview of our dataframe.

::: {.cell}

```{.r .cell-code}
glimpse(df)
```

::: {.cell-output .cell-output-stdout}

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
```


:::
:::

We have 1856 rows, ie. respondents. All variables except Sex and Group are of class `dbl`, which means they are numeric and can have decimals. Integer (numeric with no decimals) would also be fine for our purposes. The two demographic variables currently of class `chr` (character) will need to be converted to factors (`fct`), and we will do that later on.

(If you import a dataset where item variables are of class character, you will need to recode to numeric.)

### Itemlabels

Then we set up the itemlabels dataframe. This could also be done using the free [LibreOffice Calc](https://www.libreoffice.org/download/download-libreoffice/) or MS Excel. Just make sure the file has the same structure, with two variables named `itemnr` and `item` that contain the item variable names and item description. The item variable names have to match the variable names in the item dataframe.

::: {.cell}

```{.r .cell-code}
itemlabels <- df %>% 
  select(starts_with("PAN")) %>% 
  names() %>% 
  as_tibble() %>% 
  separate(value, c(NA, "item"), sep ="_[0-9][0-9]_") %>% 
  mutate(itemnr = paste0("PANAS_",c(11:20)), .before = "item")
```
:::

The `itemlabels` dataframe looks like this.

::: {.cell}

```{.r .cell-code}
itemlabels
```

::: {.cell-output .cell-output-stdout}

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
```


:::
:::

### Demographics

Variables for invariance tests such as Differential Item Functioning (DIF) need to be separated into vectors (ideally as factors with specified levels and labels) with the same length as the number of rows in the dataset. This means that any kind of removal of respondents/rows with missing data needs to be done before separating the DIF variables.

We need to check how the `Sex` variable has been coded and which responses are present in the data.

::: {.cell}

```{.r .cell-code}
table(df$Sex)
```

::: {.cell-output .cell-output-stdout}

```

  CONSENT REVOKED      DATA EXPIRED            Female              Male 
                2                 1               896               955 
Prefer not to say 
                2 
```


:::
:::

Since there are only 5 respondents using labels outside of Female/Male (too few for meaningful statistical analysis), we will remove them to have a complete dataset for all variables in this example.

::: {.cell}

```{.r .cell-code}
df <- df %>% 
  filter(Sex %in% c("Female","Male"))
```
:::

Let's make the variable a factor (instead of class "character") and put in in a vector separate from the item dataframe.

::: {.cell}

```{.r .cell-code}
dif.sex <- factor(df$Sex)
```
:::

And remove our DIF demographic variable from the item dataset.

::: {.cell}

```{.r .cell-code}
df$Sex <- NULL
```
:::

We can now make use of a very simple function included in this package!

::: {.cell}

```{.r .cell-code}
RIdemographics(dif.sex, "Sex")
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Sex </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 896 </td>
   <td style="text-align:right;"> 48.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 955 </td>
   <td style="text-align:right;"> 51.6 </td>
  </tr>
</tbody>
</table>

`````
:::
:::

Let's move on to the age variable.

::: {.cell}

```{.r .cell-code}
glimpse(df$age)
```

::: {.cell-output .cell-output-stdout}

```
 num [1:1851] 27 32 21 27 20 22 23 25 21 26 ...
```


:::
:::

Sometimes age is provided in categories, but here we have a numeric variable with age in years. Let's have a quick look at the age distribution using a histogram, and calculate mean, sd and range.

::: {.cell}

```{.r .cell-code}
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

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::
:::

Age also needs to be a separate vector, and removed from the item dataframe.

::: {.cell}

```{.r .cell-code}
dif.age <- df$age
df$age <- NULL
```
:::

There is also a grouping variable which needs to be converted to a factor.

::: {.cell}

```{.r .cell-code}
dif.group <- factor(df$Group)
df$Group <- NULL
RIdemographics(dif.group, "Group")
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Group </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> n </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Earlier Start </td>
   <td style="text-align:right;"> 901 </td>
   <td style="text-align:right;"> 48.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Later Start </td>
   <td style="text-align:right;"> 950 </td>
   <td style="text-align:right;"> 51.3 </td>
  </tr>
</tbody>
</table>

`````
:::
:::

With only item data remaining in the dataframe, we can easily rename the items in the item dataframe. These names match the `itemlabels` variable `itemnr`.

::: {.cell}

```{.r .cell-code}
names(df) <- itemlabels$itemnr
```
:::

Now we are all set for the psychometric analysis!

## Descriptives

Let's familiarize ourselves with the data before diving into the analysis.

### Missing data

First, we visualize the proportion of missing data on item level.

::: {.cell}

```{.r .cell-code}
RImissing(df)
```

::: {.cell-output .cell-output-stdout}

```
[1] "No missing data."
```


:::
:::

No missing data in this dataset. If we had missing data, we could also use `RImissingP()` to look at which respondents have missing data and how much.

### Overall responses

This provides us with an overall picture of the data distribution. As a bonus, any oddities/mistakes in recoding the item data from categories to numbers will be clearly visible.

::: {.cell}

```{.r .cell-code}
RIallresp(df)
```

::: {.cell-output-display}

<table class="table table-striped" style="font-size: 15px;
                  font-family: Lato; width: 50%">
 <thead>
  <tr>
   <th style="text-align:right;"> Response category </th>
   <th style="text-align:right;"> Number of responses </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">1</span> </td>
   <td style="text-align:right;"> 9430 </td>
   <td style="text-align:right;"> 50.9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">2</span> </td>
   <td style="text-align:right;"> 4136 </td>
   <td style="text-align:right;"> 22.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">3</span> </td>
   <td style="text-align:right;"> 2676 </td>
   <td style="text-align:right;"> 14.5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">4</span> </td>
   <td style="text-align:right;"> 1722 </td>
   <td style="text-align:right;"> 9.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">5</span> </td>
   <td style="text-align:right;"> 546 </td>
   <td style="text-align:right;"> 2.9 </td>
  </tr>
</tbody>
</table>

:::
:::

Most R packages for Rasch analysis require the lowest response category to be zero, which makes it necessary for us to recode our data, from using the range of 1-5 to 0-4.

::: {.cell}

```{.r .cell-code}
df <- df %>% 
  mutate(across(everything(), ~ car::recode(.x, "1=0;2=1;3=2;4=3;5=4", as.factor = F)))

# always check that your recoding worked as intended.
RIallresp(df)
```

::: {.cell-output-display}

<table class="table table-striped" style="font-size: 15px;
                  font-family: Lato; width: 50%">
 <thead>
  <tr>
   <th style="text-align:right;"> Response category </th>
   <th style="text-align:right;"> Number of responses </th>
   <th style="text-align:right;"> Percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">0</span> </td>
   <td style="text-align:right;"> 9430 </td>
   <td style="text-align:right;"> 50.9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">1</span> </td>
   <td style="text-align:right;"> 4136 </td>
   <td style="text-align:right;"> 22.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">2</span> </td>
   <td style="text-align:right;"> 2676 </td>
   <td style="text-align:right;"> 14.5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">3</span> </td>
   <td style="text-align:right;"> 1722 </td>
   <td style="text-align:right;"> 9.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> <span style="font-weight: bold">4</span> </td>
   <td style="text-align:right;"> 546 </td>
   <td style="text-align:right;"> 2.9 </td>
  </tr>
</tbody>
</table>

:::
:::

#### Floor/ceiling effects

Now, we can also look at the raw distribution of sum scores. The `RIrawdist()` function is a bit crude, since it requires responses in all response categories to accurately calculate max and min scores.

::: {.cell}

```{.r .cell-code}
RIrawdist(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-19-1.png){width=672}
:::
:::

We can see a floor effect with 11.8% of participants responding in the lowest category for all items.

#### Guttman structure

While not really necessary, it could be interesting to see whether the response patterns follow a Guttman-like structure. Items and persons are sorted based on lower-\>higher responses, and we should see the color move from yellow in the lower left corner to blue in the upper right corner.

::: {.cell}

```{.r .cell-code}
RIheatmap(df) +
  theme(axis.text.x = element_blank())
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-20-1.png){width=672}
:::
:::

In this data, we see the floor effect on the left, with 11.8% of respondents all yellow, and a rather weak Guttman structure. This could also be due to a low variation in item locations/difficulties. Since we have a very large sample I added a `theme()` option to remove the x-axis text, which would anyway just be a blur of the 1851 respondent row numbers. Each thin vertical slice in the figure is one respondent.

### Item level descriptives

There are many ways to look at the item level data, and we'll get them all together in the tab-panel below. The `RItileplot()` is probably most informative, since it provides the number of responses in each response category for each item. It is usually recommended to have at least \~10 responses in each category for psychometric analysis, no matter which methodology is used.

Kudos to [Solomon Kurz](https://solomonkurz.netlify.app/blog/2021-05-11-yes-you-can-fit-an-exploratory-factor-analysis-with-lavaan/) for providing the idea and code on which the tile plot function is built!

Most people will be familiar with the barplot, and this is probably most intuitive to understand the response distribution within each item. However, if there are many items it will take a while to review, and does not provide the same overview as a tileplot or stacked bars.

For this section, when using Quarto, I like to have the items available in the margin, but now we'll have to settle for a table inline.

::: {.cell}

```{.r .cell-code}
RIlistitems(df)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> itemnr </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> item </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> Distressed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> Upset </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> Hostile </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> Irritable </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:left;"> Scared </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> Afraid </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> Ashamed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_18 </td>
   <td style="text-align:left;"> Guilty </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> Nervous </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> Jittery </td>
  </tr>
</tbody>
</table>

`````
:::
:::

#### Tile plot

::: {.cell}

```{.r .cell-code}
RItileplot(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-22-1.png){width=672}
:::
:::

While response patterns are skewed for all items, there are more than 10 responses in each category for all items which is helpful for the analysis.

#### Stacked bars

::: {.cell}

```{.r .cell-code}
RIbarstack(df) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-23-1.png){width=672}
:::
:::

#### Barplots


::: {.cell layout-ncol="2"}

```{.r .cell-code}
RIbarplot(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-1.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-2.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-3.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-4.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-5.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-6.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-7.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-8.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-9.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-24-10.png){width=672}
:::
:::




## Rasch analysis 1 {#sec-rasch}

The eRm package and Conditional Maximum Likelihood (CML) estimation will be used primarily, with the Partial Credit Model since this is polytomous data.

This is also where the [five basic psychometric aspects](https://doi.org/10.31219/osf.io/3htzc) are good to recall.

-   Unidimensionality & local independence
-   Response categories
-   Invariance
-   Targeting
-   Measurement uncertainties (reliability)

We will begin by looking at unidimensionality, response categories, and targeting in parallel below. For unidimensionality, we are mostly interested in item fit and residual correlations, as well as PCA of residuals and loadings on the first residual contrast. At the same time, disordered response categories can influence item fit to some extent (and vice versa), and knowledge about targeting can be useful if it is necessary to remove items due to residual correlations.

When unidimensionality and response categories are found to work adequately, we will move on to invariance testing (Differential Item Functioning, DIF). It should be noted that DIF should be evaluated in parallel with all other psychometric aspects, but since it is a more complex issue it is kept in a separate section in this vignette (as is person fit). Finally, when/if invariance/DIF also looks acceptable, we can investigate reliability/measurement uncertainties.

### Conditional item fit

::: {.cell}

```{.r .cell-code}
simfit1 <- RIgetfit(df, iterations = 1000, cpu = 8) # save simulation output to object `simfit1`
RIitemfit(df, simfit1)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> InfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> OutfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit diff </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit diff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.198</span> </td>
   <td style="text-align:left;"> [0.927, 1.078] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.255</span> </td>
   <td style="text-align:left;"> [0.923, 1.094] </td>
   <td style="text-align:left;"> 0.12 </td>
   <td style="text-align:left;"> 0.161 </td>
   <td style="text-align:right;"> -0.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.842</span> </td>
   <td style="text-align:left;"> [0.916, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.852</span> </td>
   <td style="text-align:left;"> [0.888, 1.127] </td>
   <td style="text-align:left;"> 0.074 </td>
   <td style="text-align:left;"> 0.036 </td>
   <td style="text-align:right;"> -0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.174</span> </td>
   <td style="text-align:left;"> [0.906, 1.087] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.346</span> </td>
   <td style="text-align:left;"> [0.865, 1.139] </td>
   <td style="text-align:left;"> 0.087 </td>
   <td style="text-align:left;"> 0.207 </td>
   <td style="text-align:right;"> 0.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.064</span> </td>
   <td style="text-align:left;"> [0.918, 1.085] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.11</span> </td>
   <td style="text-align:left;"> [0.902, 1.104] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> 0.006 </td>
   <td style="text-align:right;"> -0.32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.783</span> </td>
   <td style="text-align:left;"> [0.915, 1.106] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.725</span> </td>
   <td style="text-align:left;"> [0.876, 1.138] </td>
   <td style="text-align:left;"> 0.132 </td>
   <td style="text-align:left;"> 0.151 </td>
   <td style="text-align:right;"> 0.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.798</span> </td>
   <td style="text-align:left;"> [0.919, 1.114] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.781</span> </td>
   <td style="text-align:left;"> [0.864, 1.125] </td>
   <td style="text-align:left;"> 0.121 </td>
   <td style="text-align:left;"> 0.083 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.953</span> </td>
   <td style="text-align:left;"> [0.897, 1.094] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.92</span> </td>
   <td style="text-align:left;"> [0.826, 1.154] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_18 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.088</span> </td>
   <td style="text-align:left;"> [0.907, 1.098] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.172</span> </td>
   <td style="text-align:left;"> [0.853, 1.143] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> 0.029 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.921</span> </td>
   <td style="text-align:left;"> [0.92, 1.089] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.933</span> </td>
   <td style="text-align:left;"> [0.917, 1.085] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.205</span> </td>
   <td style="text-align:left;"> [0.918, 1.079] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.257</span> </td>
   <td style="text-align:left;"> [0.897, 1.097] </td>
   <td style="text-align:left;"> 0.126 </td>
   <td style="text-align:left;"> 0.16 </td>
   <td style="text-align:right;"> -0.05 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> MSQ values based on conditional calculations (n = 1851 complete cases).<br>                                Simulation based thresholds from 1000 simulated datasets.</td></tr>
</tfoot>
</table>

`````
:::
:::

`RIitemfit()` works with both dichotomous and polytomous data (no option needed).

It is important to note that the new (since version 0.2.2, released 2024-08-19) `RIitemfit()` function uses **conditional** outfit/infit, which is both robust to different sample sizes and makes ZSTD unnecessary [@muller_item_2020].

Since the distribution of item fit statistics are not known, we need to use simulation to determine appropriate cutoff threshold values for the current sample and items.  `RIitemfit()` can also use the simulation based cutoff values and use them for conditional highlighting of misfitting items. See the [blog post on simulation based cutoffs](https://pgmj.github.io/simcutoffs.html) for some more details on this. `RIitemfit()` can also be used without cutoffs and conditional highlighting. For a possibly useful rule-of-thumb cutoff for infit MSQ only, use the option `cutoff = "Smith98"` [@smith_using_1998;@muller_item_2020]. However, this cutoff is not applicable for all items, only for what can be expected for the *average* item fit. The simulation/bootstrap-based cutoff values will be more accurate for every item in your data.

Briefly stated, the simulation uses the properties of the current sample and items, and simulates n iterations of data that fit the Rasch model to get an empirical distribution of item fit that we can use for comparison with the observed data. This is also known as "parametric bootstrapping".

The simulation can take quite a bit of time to run if you have complex data/many items/many participants, and/or choose to use many iterations. While insufficient testing has been done to make any strong recommendations, I think 500 iterations is a good starting point.

For reference, the simulation above, using 10 items with 5 response categories each and 1851 respondents, takes about 52 seconds to run on 8 cpu cores (Macbook Pro M1 Max) for 1000 iterations.

I'll cite Ostini & Nering [-@ostini_polytomous_2006] on the description of outfit and infit (pages 86-87):

> Response residuals can be summed over respondents to obtain an item fit measure. Generally, the accumulation is done with squared standardized residuals, which are then divided by the total number of respondents to obtain a mean square statistic. In this form, the statistic is referred to as an **unweighted mean square** (Masters & Wright, 1997; Wright & Masters, 1982) and has also come to be known as **“outfit”** (Smith, Schumacker, & Bush, 1998; Wu, 1997), perhaps because it is highly sensitive to outlier responses (Adams & Khoo, 1996; Smith et al., 1998; Wright & Masters, 1982).

> A weighted version of this statistic was developed to counteract its sensitivity to outliers (Smith, 2000). In its weighted form, the squared standardized residual is multiplied by the observed response variance and then divided by the sum of the item response variances. This is sometimes referred to as an **information weighted mean square** and has become known as **“infit”** (Smith et al., 1998; Wu, 1997).

A low item fit value (sometimes referred to as "overfitting" the Rasch model) indicates that responses are too predictable and provide little information. This is often the case for items that are very general/broad in scope in relation to the latent variable.

A high item fit value (sometimes referred to as "underfitting" the Rasch model) can indicate several things, often multidimensionality or a question that is difficult to interpret. This could for instance be a question that asks about two things at the same time or is ambiguous for other reasons.

### Item-restscore

This is another useful function from the `iarm` package. It shows the expected and observed correlation between an item and a score based on the rest of the items [@kreiner_note_2011]. Similarly, but inverted, to item fit, a lower observed correlation value than expected indicates that the item may not belong to the dimension. A higher than expected observed value indicates an overfitting and possibly redundant item.

::: {.cell}

```{.r .cell-code}
RIrestscore(df)
```


::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Observed value </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Model expected value </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Absolute difference </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Adjusted p-value (BH) </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Statistical significance level </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Location </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Relative location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:left;"> ** </td>
   <td style="text-align:right;"> -0.33 </td>
   <td style="text-align:right;"> 1.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:left;"> *** </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> 1.42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.015 </td>
   <td style="text-align:left;"> * </td>
   <td style="text-align:right;"> 0.43 </td>
   <td style="text-align:right;"> 1.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.066 </td>
   <td style="text-align:left;"> . </td>
   <td style="text-align:right;"> -0.32 </td>
   <td style="text-align:right;"> 1.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:left;"> *** </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;"> 1.63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:left;"> *** </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:right;"> 0.68 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:left;"> ** </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 1.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_18 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.408 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 1.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.088 </td>
   <td style="text-align:left;"> . </td>
   <td style="text-align:right;"> -0.49 </td>
   <td style="text-align:right;"> 0.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:left;"> *** </td>
   <td style="text-align:right;"> -0.05 </td>
   <td style="text-align:right;"> 1.42 </td>
  </tr>
</tbody>
</table>

`````
:::
:::

### Conditional item characteristic curves

The [`iarm`](https://cran.r-project.org/web/packages/iarm/index.html) package [@mueller_iarm_2022] provides several interesting functions for assessing item fit, DIF and other things. Some of these functions may be included in a future version of the `easyRasch` package. Below are conditional item characteristic curves (ICC's) using the estimated theta (factor score).

These curves indicate item fit on a group level, where resondents are split into "class intervals" based on their sum score/factor score.

::: {.cell}

```{.r .cell-code}
library(iarm)
ICCplot(as.data.frame(df), 
        itemnumber = 1:4, 
        method = "cut", 
        cinumber = 6, # number of class intervals to split respondents into
        itemdescrip = c("PANAS_11","PANAS_12","PANAS_13","PANAS_14"))
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-27-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] "Please press Zoom on the Plots window to see the plot"
```


:::
:::

A similar, but even more informative and flexible, visualization has been made available in the [`RASCHplot`](https://github.com/ERRTG/RASCHplot/) package [@buchardt_visualizing_2023], which needs to be installed from GitHub (see code below). The linked paper is recommended reading, not least for descriptions of the useful options available. Below are some sample plots showing conditional ICC's using the raw sum score.

::: {.cell}

```{.r .cell-code}
library(RASCHplot) # devtools::install_github("ERRTG/RASCHplot")

CICCplot(PCM(df),
         which.item = c(1:4),
         lower.groups = c(0,7,14,21,28),
         grid.items = TRUE)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-28-1.png){width=672}
:::
:::

### PCA of residuals

Principal Component Analysis of Rasch model residuals.

::: {.cell}

```{.r .cell-code}
RIpcmPCA(df)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Eigenvalues </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Proportion of variance </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1.79 </td>
   <td style="text-align:left;"> 16.9% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.47 </td>
   <td style="text-align:left;"> 15.1% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.28 </td>
   <td style="text-align:left;"> 13.6% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.14 </td>
   <td style="text-align:left;"> 13.3% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.06 </td>
   <td style="text-align:left;"> 11.9% </td>
  </tr>
</tbody>
</table>

`````
:::
:::

Based on rule of thumb, the first eigenvalue should be below 2.0 to support unidimensionality. However, this is seldom accurate and needs to be complemented with checking item fit and residual correlations. The target value should probably be below 1.75, based on my experience, but really I find this metric redundant and only keep it here for those coming from Winsteps who might be looking for it. Speaking of Winsteps, the "explained variance" will not be comparable to Winsteps corresponding metric, since this one only shows the results from the analysis of residuals.

### Residual correlations

Similarly to item fit, we need to run simulations to get a useful cutoff threshold value for when residual correlations amongst item pairs are too large to support the local independence assumption [@christensen2017].

And again, the simulation can take a bit of time, but it is necessary to set the appropriate cutoff value.

::: {.cell}

```{.r .cell-code}
simcor1 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor1$p99)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_11 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_12 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_13 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_14 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_15 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_16 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_17 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_18 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_19 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_20 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.1</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.05</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.01</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.11</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.09</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.07</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.14</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.22</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.29</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.17</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.1</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.25</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.27</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.38</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.18</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.09</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.09</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.19</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.08</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_18 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.19</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.16</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.18</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.32</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.25</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.14</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.1</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.08</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.21</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.12</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.06</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.22</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.07</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.05</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.19</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.16</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.07</span> </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Relative cut-off value is -0.001, which is 0.101 above the average correlation (-0.102).<br>                                Correlations above the cut-off are highlighted in red text.</td></tr>
</tfoot>
</table>

`````
:::
:::

The matrix above shows item-pair correlations of item residuals, with highlights in red showing correlations crossing the threshold compared to the average item-pair correlation (for all item-pairs) [@christensen2017]. Rasch model residual correlations (Yen's Q3) are calculated using the [mirt](https://cran.r-project.org/web/packages/mirt/index.html) package.

### Partial gamma LD

Another way to assess local (in)dependence is by partial gamma coefficients [@kreiner_analysis_2004]. This is also a function from the `iarm` package. See `?iarm::partgam_LD` for details.

::: {.cell}

```{.r .cell-code}
RIpartgamLD(df)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item 1 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item 2 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Partial gamma </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Lower CI </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Upper CI </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Adjusted p-value (BH) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.634</span> </td>
   <td style="text-align:right;"> 0.030 </td>
   <td style="text-align:right;"> 0.576 </td>
   <td style="text-align:right;"> 0.693 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.634</span> </td>
   <td style="text-align:right;"> 0.030 </td>
   <td style="text-align:right;"> 0.576 </td>
   <td style="text-align:right;"> 0.692 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> PANAS_18 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.577</span> </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 0.512 </td>
   <td style="text-align:right;"> 0.642 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_18 </td>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.551</span> </td>
   <td style="text-align:right;"> 0.034 </td>
   <td style="text-align:right;"> 0.484 </td>
   <td style="text-align:right;"> 0.619 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.341</span> </td>
   <td style="text-align:right;"> 0.037 </td>
   <td style="text-align:right;"> 0.269 </td>
   <td style="text-align:right;"> 0.413 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.297</span> </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 0.219 </td>
   <td style="text-align:right;"> 0.375 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.293</span> </td>
   <td style="text-align:right;"> 0.038 </td>
   <td style="text-align:right;"> 0.217 </td>
   <td style="text-align:right;"> 0.368 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.287</span> </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 0.208 </td>
   <td style="text-align:right;"> 0.366 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.274</span> </td>
   <td style="text-align:right;"> 0.040 </td>
   <td style="text-align:right;"> 0.197 </td>
   <td style="text-align:right;"> 0.352 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> PANAS_15 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.261</span> </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 0.181 </td>
   <td style="text-align:right;"> 0.340 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.209</span> </td>
   <td style="text-align:right;"> 0.042 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 0.291 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.202</span> </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 0.121 </td>
   <td style="text-align:right;"> 0.283 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.191</span> </td>
   <td style="text-align:right;"> 0.044 </td>
   <td style="text-align:right;"> 0.105 </td>
   <td style="text-align:right;"> 0.278 </td>
   <td style="text-align:right;"> 0.001 </td>
  </tr>
</tbody>
</table>

`````
:::
:::

### 1st contrast loadings

::: {.cell}

```{.r .cell-code}
RIloadLoc(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-32-1.png){width=672}
:::
:::

Here we see item locations and their loadings on the first residual contrast. This figure can be helpful to identify clusters in data or multidimensionality.

### Analysis of response categories

The `xlims` setting changes the x-axis limits for the plots. The default values usually make sense, and we mostly add this option to point out the possibility of doing so. You can also choose to only show plots for only specific items.


::: {.cell layout-ncol="2"}

```{.r .cell-code}
RIitemCats(df, xlims = c(-5,5))
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-1.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-2.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-3.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-4.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-5.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-6.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-7.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-8.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-9.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-33-10.png){width=672}
:::
:::


Each response category for each item should have a curve that indicates it to be the most probably response at some point on the latent variable (x axis in the figure).

### Response categories MIRT

For a more compact figure.

::: {.cell}

```{.r .cell-code}
mirt(df, model=1, itemtype='Rasch', verbose = FALSE) %>% 
  plot(type="trace", as.table = TRUE, 
       theta_lim = c(-5,5)) # changes x axis limits
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-34-1.png){width=672}
:::
:::

### Targeting

::: {.cell}

```{.r .cell-code}
# increase fig-height in the chunk option above if you have many items
RItargeting(df, xlim = c(-5,4)) # xlim defaults to c(-4,4) if you omit this option
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-35-1.png){width=672}
:::
:::

This figure shows how well the items fit the respondents/persons. It is a sort of [Wright Map](https://www.rasch.org/rmt/rmt253b.htm) that shows person locations and item threshold locations on the same logit scale.

The top part shows person location histogram, the middle part an inverted histogram of item threshold locations, and the bottom part shows individual item threshold locations. The histograms also show means and standard deviations.

### Item hierarchy

Here the items are sorted on their average threshold location (black diamonds). 84% confidence intervals are shown around each item threshold location. For further details, see the caption text below the figure.

The numbers displayed in the plot can be disabled using the option `numbers = FALSE`.

::: {.cell}

```{.r .cell-code}
RIitemHierarchy(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-36-1.png){width=672}
:::
:::


### Analysis 1 comments

Item fit shows a lot of issues.

Item 18 has issues with the second lowest category being disordered. Several other items have very short distances between thresholds 1 and 2, which is also clearly seen in the Item Hierarchy figure above.

Two item-pairs show residual correlations far above the cutoff value:

-   15 and 16 (scared and afraid)
-   17 and 18 (ashamed and guilty)

Since item 15 also has a residual correlation with item 19, we will remove it. In the second pair, item 18 will be removed since it also has problems with disordered response categories.

We have multiple "diagnostics" to review when deciding which item to remove if there are strong residual correlations between two items. Here is a list of commonly used criteria:

- item fit
- item threshold locations compared to sample locations (targeting)
- ordering of response categories
- DIF
- and whether there are residual correlations between one item and multiple other items


::: {.cell}

```{.r .cell-code}
removed.items <- c("PANAS_15","PANAS_18")

df_backup <- df

df <- df_backup %>% 
  select(!any_of(removed.items))
```
:::

As seen in the code above, I chose to create a copy of the dataframe with the removed items omitted. This can be useful if, at a later stage in the analysis, I want to be able to quickly "go back" and reinstate an item or undo any other change I have made.

## Rasch analysis 2

With items 15 and 18 removed.


::: {.cell .column-margin}
::: {.cell-output-display}

<table class="table table-striped" style="font-size: 13px; font-family: Lato">
 <thead>
  <tr>
   <th style="text-align:center;"> itemnr </th>
   <th style="text-align:left;"> item </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_11</span> </td>
   <td style="text-align:left;"> Distressed </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_12</span> </td>
   <td style="text-align:left;"> Upset </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_13</span> </td>
   <td style="text-align:left;"> Hostile </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_14</span> </td>
   <td style="text-align:left;"> Irritable </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_16</span> </td>
   <td style="text-align:left;"> Afraid </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_17</span> </td>
   <td style="text-align:left;"> Ashamed </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_19</span> </td>
   <td style="text-align:left;"> Nervous </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_20</span> </td>
   <td style="text-align:left;"> Jittery </td>
  </tr>
</tbody>
</table>

:::
:::


### Conditional item fit

::: {.cell}

```{.r .cell-code}
simfit2 <- RIgetfit(df, iterations = 1000, cpu = 8)
RIitemfit(df, simcut = simfit2)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> InfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> OutfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit diff </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit diff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.14</span> </td>
   <td style="text-align:left;"> [0.909, 1.084] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.184</span> </td>
   <td style="text-align:left;"> [0.916, 1.094] </td>
   <td style="text-align:left;"> 0.056 </td>
   <td style="text-align:left;"> 0.09 </td>
   <td style="text-align:right;"> -0.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.801</span> </td>
   <td style="text-align:left;"> [0.911, 1.093] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.799</span> </td>
   <td style="text-align:left;"> [0.895, 1.122] </td>
   <td style="text-align:left;"> 0.11 </td>
   <td style="text-align:left;"> 0.096 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.108</span> </td>
   <td style="text-align:left;"> [0.915, 1.089] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.252</span> </td>
   <td style="text-align:left;"> [0.853, 1.157] </td>
   <td style="text-align:left;"> 0.019 </td>
   <td style="text-align:left;"> 0.095 </td>
   <td style="text-align:right;"> 0.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.976</span> </td>
   <td style="text-align:left;"> [0.916, 1.08] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.022</span> </td>
   <td style="text-align:left;"> [0.903, 1.088] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.876</span> </td>
   <td style="text-align:left;"> [0.92, 1.098] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.854</span> </td>
   <td style="text-align:left;"> [0.877, 1.142] </td>
   <td style="text-align:left;"> 0.044 </td>
   <td style="text-align:left;"> 0.023 </td>
   <td style="text-align:right;"> 0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.04</span> </td>
   <td style="text-align:left;"> [0.904, 1.1] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.01</span> </td>
   <td style="text-align:left;"> [0.866, 1.183] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.937</span> </td>
   <td style="text-align:left;"> [0.925, 1.082] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.957</span> </td>
   <td style="text-align:left;"> [0.919, 1.084] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.159</span> </td>
   <td style="text-align:left;"> [0.921, 1.076] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.194</span> </td>
   <td style="text-align:left;"> [0.91, 1.099] </td>
   <td style="text-align:left;"> 0.083 </td>
   <td style="text-align:left;"> 0.095 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> MSQ values based on conditional calculations (n = 1851 complete cases).<br>                                Simulation based thresholds from 1000 simulated datasets.</td></tr>
</tfoot>
</table>

`````
:::
:::

### PCA of residuals

::: {.cell}

```{.r .cell-code}
RIpcmPCA(df)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Eigenvalues </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Proportion of variance </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1.52 </td>
   <td style="text-align:left;"> 18.9% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.33 </td>
   <td style="text-align:left;"> 17.1% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.19 </td>
   <td style="text-align:left;"> 16.6% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.15 </td>
   <td style="text-align:left;"> 14.6% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:left;"> 13.1% </td>
  </tr>
</tbody>
</table>

`````
:::
:::

### Residual correlations

::: {.cell}

```{.r .cell-code}
simcor2 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor2$p99)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_11 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_12 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_13 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_14 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_16 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_17 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_19 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_20 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.16</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.11</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.06</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.19</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.03</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.01</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.16</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.08</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.25</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.28</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.18</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.06</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.09</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.19</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.28</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.18</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.12</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.16</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.1</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.27</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.11</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.18</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.09</span> </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Relative cut-off value is -0.035, which is 0.095 above the average correlation (-0.129).<br>                                Correlations above the cut-off are highlighted in red text.</td></tr>
</tfoot>
</table>

`````
:::
:::

### 1st contrast loadings

::: {.cell}

```{.r .cell-code}
RIloadLoc(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-42-1.png){width=672}
:::
:::

### Targeting

::: {.cell}

```{.r .cell-code}
RItargeting(df, xlim = c(-4,4), bins = 45)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-43-1.png){width=672}
:::
:::

### Item hierarchy

::: {.cell}

```{.r .cell-code}
RIitemHierarchy(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-44-1.png){width=672}
:::
:::


### Analysis 2 comments

Items 16 & 19, and 12 & 14 show problematic residual correlations.

Let's look at DIF before taking action upon this information. While we are keeping DIF as a separate section in this vignette, it is recommended to include DIF-analysis in the `panel-tabset` above (on item fit, PCA, residual correlations, etc).

## DIF - differential item functioning

We'll be looking at whether item (threshold) locations are stable between demographic subgroups.

There are several DIF analysis tools available. The first one uses the package `psychotree`, which relies on statistical significance at p < .05 as an indicator for DIF. This is a criterion that is highly sample size sensitive, and we are always interested in the size/magnitude of DIF as well, since that will inform us about the impact of DIF on the estimated latent variable. 

The structure of DIF is also an important and complex aspect, particularly for polytomous data. Uniform DIF means that the DIF is similar across the latent continuum. We can test this in R using the `lordif` package, as demonstrated in @sec-lordif. However, it should be noted that the `lordif` package does not provide an option to use Rasch models, and there may be results that are caused by also allowing the discrimination parameter to vary across items.

A recent preprint [@henninger_partial_2024] does a great job illustrating "differential step functioning" (DSF), which is when item threshold locations in polytomous data show varying levels of DIF. It also describes a forthcoming development of the `psychotree` where one can use DIF effect size and purification functions to evaluate DIF/DSF. When the updated package is available, I will work to implement these new functions into the `easyRasch` package as well.

It is important to ensure that no cells in the data are empty for subgroups when conducting a DIF analysis. Split the data using the DIF-variable and create separate tileplots to review the response distribution in the DIF-groups.

::: {.cell}

```{.r .cell-code}
difPlots <- df %>% # save the output into the `difPlots` object
  add_column(gender = dif.sex) %>% # add the DIF variable to the dataframe
  split(.$gender) %>% # split the data using the DIF variable
  map(~ RItileplot(.x %>% dplyr::select(!gender)) + labs(title = .x$gender)) # create separate tileplots for each group

difPlots$Female + difPlots$Male # the actual name of the plots (in this case Male/Female) will be determined by the factor labels
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-45-1.png){width=672}
:::
:::

### Sex


::: {.cell .column-margin}
::: {.cell-output-display}

<table class="table table-striped" style="font-size: 13px; font-family: Lato">
 <thead>
  <tr>
   <th style="text-align:center;"> itemnr </th>
   <th style="text-align:left;"> item </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_11</span> </td>
   <td style="text-align:left;"> Distressed </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_12</span> </td>
   <td style="text-align:left;"> Upset </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_13</span> </td>
   <td style="text-align:left;"> Hostile </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_14</span> </td>
   <td style="text-align:left;"> Irritable </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_16</span> </td>
   <td style="text-align:left;"> Afraid </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_17</span> </td>
   <td style="text-align:left;"> Ashamed </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_19</span> </td>
   <td style="text-align:left;"> Nervous </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_20</span> </td>
   <td style="text-align:left;"> Jittery </td>
  </tr>
</tbody>
</table>

:::
:::


#### Table

::: {.cell}

```{.r .cell-code}
RIdifTable(df, dif.sex)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-47-1.png){width=672}
:::

::: {.cell-output-display}

<table class="table table-striped" style="font-size: 15px; font-family: Lato">
 <thead>
  <tr>
   <th style="text-align:right;"> Item </th>
   <th style="text-align:right;"> 2 </th>
   <th style="text-align:right;"> 3 </th>
   <th style="text-align:right;"> Mean location </th>
   <th style="text-align:right;"> StDev </th>
   <th style="text-align:right;"> MaxDiff </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> PANAS_11 </td>
   <td style="text-align:right;"> -0.314 </td>
   <td style="text-align:right;"> -0.196 </td>
   <td style="text-align:right;"> -0.255 </td>
   <td style="text-align:right;"> 0.083 </td>
   <td style="text-align:right;"> <span style="color: black">0.117</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_12 </td>
   <td style="text-align:right;"> 0.028 </td>
   <td style="text-align:right;"> -0.044 </td>
   <td style="text-align:right;"> -0.008 </td>
   <td style="text-align:right;"> 0.051 </td>
   <td style="text-align:right;"> <span style="color: black">0.073</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_13 </td>
   <td style="text-align:right;"> 0.553 </td>
   <td style="text-align:right;"> 0.402 </td>
   <td style="text-align:right;"> 0.478 </td>
   <td style="text-align:right;"> 0.107 </td>
   <td style="text-align:right;"> <span style="color: black">0.151</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_14 </td>
   <td style="text-align:right;"> -0.328 </td>
   <td style="text-align:right;"> -0.183 </td>
   <td style="text-align:right;"> -0.255 </td>
   <td style="text-align:right;"> 0.103 </td>
   <td style="text-align:right;"> <span style="color: black">0.146</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_16 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;"> 0.059 </td>
   <td style="text-align:right;"> 0.078 </td>
   <td style="text-align:right;"> <span style="color: black">0.111</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_17 </td>
   <td style="text-align:right;"> 0.520 </td>
   <td style="text-align:right;"> 0.290 </td>
   <td style="text-align:right;"> 0.405 </td>
   <td style="text-align:right;"> 0.163 </td>
   <td style="text-align:right;"> <span style="color: black">0.230</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_19 </td>
   <td style="text-align:right;"> -0.495 </td>
   <td style="text-align:right;"> -0.355 </td>
   <td style="text-align:right;"> -0.425 </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> <span style="color: black">0.140</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_20 </td>
   <td style="text-align:right;"> 0.032 </td>
   <td style="text-align:right;"> -0.028 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.042 </td>
   <td style="text-align:right;"> <span style="color: black">0.059</span> </td>
  </tr>
</tbody>
</table>

:::
:::

#### Figure items

::: {.cell}

```{.r .cell-code}
RIdifFigure(df, dif.sex)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-48-1.png){width=672}
:::
:::

#### Figure thresholds

::: {.cell}

```{.r .cell-code}
RIdifFigThresh(df, dif.sex)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-49-1.png){width=672}
:::
:::

While no item shows problematic levels of DIF regarding item location, as shown by the table, there is an interesting pattern in the thresholds figure. The lowest threshold seems to be slightly lower for node 3 (Male) for all items. Also, item 11 shows a much wider spread of item locations for node 3 compared to node 2.

The results do not require any action since the difference is small.

### Age

The `psychotree` package uses a model-based recursive partitioning that is particularly useful when you have a continuous variable such as age in years and a large enough sample. It will test different ways to partition the age variable to determine potential group differences [@strobl2015; @strobl2021].

::: {.cell}

```{.r .cell-code}
RIdifTable(df, dif.age)
```

::: {.cell-output .cell-output-stdout}

```
[1] "No statistically significant DIF found."
```


:::
:::

No DIF found for age.

### Group

::: {.cell}

```{.r .cell-code}
RIdifTable(df, dif.group)
```

::: {.cell-output .cell-output-stdout}

```
[1] "No statistically significant DIF found."
```


:::
:::

And no DIF for group.

### Sex and age

The `psychotree` package also allows for DIF interaction analysis with multiple DIF variables. We can use `RIdifTable2()` to input two DIF variables.

::: {.cell}

```{.r .cell-code}
RIdifTable2(df, dif.sex, dif.age)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-52-1.png){width=672}
:::

::: {.cell-output-display}

<table class="table table-striped" style="font-size: 15px; font-family: Lato">
 <thead>
  <tr>
   <th style="text-align:right;"> Item </th>
   <th style="text-align:right;"> 2 </th>
   <th style="text-align:right;"> 3 </th>
   <th style="text-align:right;"> Mean location </th>
   <th style="text-align:right;"> StDev </th>
   <th style="text-align:right;"> MaxDiff </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> PANAS_11 </td>
   <td style="text-align:right;"> -0.314 </td>
   <td style="text-align:right;"> -0.196 </td>
   <td style="text-align:right;"> -0.255 </td>
   <td style="text-align:right;"> 0.083 </td>
   <td style="text-align:right;"> <span style="color: black">0.117</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_12 </td>
   <td style="text-align:right;"> 0.028 </td>
   <td style="text-align:right;"> -0.044 </td>
   <td style="text-align:right;"> -0.008 </td>
   <td style="text-align:right;"> 0.051 </td>
   <td style="text-align:right;"> <span style="color: black">0.073</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_13 </td>
   <td style="text-align:right;"> 0.553 </td>
   <td style="text-align:right;"> 0.402 </td>
   <td style="text-align:right;"> 0.478 </td>
   <td style="text-align:right;"> 0.107 </td>
   <td style="text-align:right;"> <span style="color: black">0.151</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_14 </td>
   <td style="text-align:right;"> -0.328 </td>
   <td style="text-align:right;"> -0.183 </td>
   <td style="text-align:right;"> -0.255 </td>
   <td style="text-align:right;"> 0.103 </td>
   <td style="text-align:right;"> <span style="color: black">0.146</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_16 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;"> 0.059 </td>
   <td style="text-align:right;"> 0.078 </td>
   <td style="text-align:right;"> <span style="color: black">0.111</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_17 </td>
   <td style="text-align:right;"> 0.520 </td>
   <td style="text-align:right;"> 0.290 </td>
   <td style="text-align:right;"> 0.405 </td>
   <td style="text-align:right;"> 0.163 </td>
   <td style="text-align:right;"> <span style="color: black">0.230</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_19 </td>
   <td style="text-align:right;"> -0.495 </td>
   <td style="text-align:right;"> -0.355 </td>
   <td style="text-align:right;"> -0.425 </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;"> <span style="color: black">0.140</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> PANAS_20 </td>
   <td style="text-align:right;"> 0.032 </td>
   <td style="text-align:right;"> -0.028 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.042 </td>
   <td style="text-align:right;"> <span style="color: black">0.059</span> </td>
  </tr>
</tbody>
</table>

:::
:::

No interaction effect found for sex and age. The analysis only shows the previously identified DIF for sex.

### LRT-based DIF {#sec-diflrt}

We'll use the group variable as an example. First, we can simply run the test to get the overall result.

::: {.cell}

```{.r .cell-code}
erm.out <- PCM(df)
LRtest(erm.out, splitcr = dif.group)
```

::: {.cell-output .cell-output-stdout}

```

Andersen LR-test: 
LR-value: 46.864 
Chi-square df: 31 
p-value:  0.034 
```


:::
:::

Review the documentation for further details, using `?LRtest` in your R console panel in Rstudio. There is also a plotting function, `plotGOF()` that may be of interest.

#### Item location table
::: {.cell}

```{.r .cell-code}
RIdifTableLR(df, dif.group)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
<tr>
<th style="empty-cells: hide;" colspan="1"></th>
<th style="padding-bottom:0; padding-left:5px;padding-right:5px;text-align: center; font-weight: bold; " colspan="4"><div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">Item locations</div></th>
<th style="padding-bottom:0; padding-left:5px;padding-right:5px;text-align: center; font-weight: bold; " colspan="3"><div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">Standard errors</div></th>
</tr>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Earlier Start </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Later Start </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> MaxDiff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> All </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE_Earlier Start </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE_Later Start </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE_All </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.069</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.072</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.003</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.073 </td>
   <td style="text-align:right;"> 0.140 </td>
   <td style="text-align:right;"> 0.124 </td>
   <td style="text-align:right;font-style: italic;"> 0.093 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.33</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.346</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.016</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.342 </td>
   <td style="text-align:right;"> 0.155 </td>
   <td style="text-align:right;"> 0.133 </td>
   <td style="text-align:right;font-style: italic;"> 0.101 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.69</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.966</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.276</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.826 </td>
   <td style="text-align:right;"> 0.185 </td>
   <td style="text-align:right;"> 0.189 </td>
   <td style="text-align:right;font-style: italic;"> 0.129 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.118</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.054</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.064</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.084 </td>
   <td style="text-align:right;"> 0.142 </td>
   <td style="text-align:right;"> 0.121 </td>
   <td style="text-align:right;font-style: italic;"> 0.092 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.447</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.362</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.085</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.401 </td>
   <td style="text-align:right;"> 0.160 </td>
   <td style="text-align:right;"> 0.134 </td>
   <td style="text-align:right;font-style: italic;"> 0.102 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.66</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.822</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.162</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.751 </td>
   <td style="text-align:right;"> 0.184 </td>
   <td style="text-align:right;"> 0.172 </td>
   <td style="text-align:right;font-style: italic;"> 0.125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.028</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.122</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.094</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.083 </td>
   <td style="text-align:right;"> 0.136 </td>
   <td style="text-align:right;"> 0.117 </td>
   <td style="text-align:right;font-style: italic;"> 0.088 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.351</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.348</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.003</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.352 </td>
   <td style="text-align:right;"> 0.160 </td>
   <td style="text-align:right;"> 0.138 </td>
   <td style="text-align:right;font-style: italic;"> 0.104 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Values highlighted in red are above the chosen cutoff 0.5 logits. Background color brown and blue indicate the lowest and highest values among the DIF groups.</td></tr>
</tfoot>
</table>

`````
:::
:::
#### Item location figure
::: {.cell}

```{.r .cell-code}
RIdifFigureLR(df, dif.group) + theme_rise()
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-55-1.png){width=672}
:::
:::
#### Item threshold table
::: {.cell}

```{.r .cell-code}
RIdifThreshTblLR(df, dif.group)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
<tr>
<th style="empty-cells: hide;" colspan="1"></th>
<th style="padding-bottom:0; padding-left:5px;padding-right:5px;text-align: center; font-weight: bold; " colspan="4"><div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">Threshold locations</div></th>
<th style="padding-bottom:0; padding-left:5px;padding-right:5px;text-align: center; font-weight: bold; " colspan="3"><div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">Standard errors</div></th>
</tr>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item threshold </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Earlier Start </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Later Start </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> MaxDiff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> All </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE_Earlier Start </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE_Later Start </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE_All </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_11</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-1.245</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-1.241</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.004</span> </td>
   <td style="text-align:right;font-style: italic;"> -1.240 </td>
   <td style="text-align:right;"> 0.098 </td>
   <td style="text-align:right;"> 0.094 </td>
   <td style="text-align:right;font-style: italic;"> 0.068 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.365</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.221</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.144</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.284 </td>
   <td style="text-align:right;"> 0.107 </td>
   <td style="text-align:right;"> 0.100 </td>
   <td style="text-align:right;font-style: italic;"> 0.073 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.281</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.096</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.185</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.180 </td>
   <td style="text-align:right;"> 0.131 </td>
   <td style="text-align:right;"> 0.114 </td>
   <td style="text-align:right;font-style: italic;"> 0.086 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.604</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.655</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.051</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.637 </td>
   <td style="text-align:right;"> 0.224 </td>
   <td style="text-align:right;"> 0.188 </td>
   <td style="text-align:right;font-style: italic;"> 0.144 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_12</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.484</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.362</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.122</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.418 </td>
   <td style="text-align:right;"> 0.092 </td>
   <td style="text-align:right;"> 0.091 </td>
   <td style="text-align:right;font-style: italic;"> 0.065 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.241</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.198</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.439</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.005 </td>
   <td style="text-align:right;"> 0.126 </td>
   <td style="text-align:right;"> 0.108 </td>
   <td style="text-align:right;font-style: italic;"> 0.082 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.479</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.456</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.023</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.467 </td>
   <td style="text-align:right;"> 0.169 </td>
   <td style="text-align:right;"> 0.129 </td>
   <td style="text-align:right;font-style: italic;"> 0.103 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.086</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.489</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.403</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.323 </td>
   <td style="text-align:right;"> 0.233 </td>
   <td style="text-align:right;"> 0.205 </td>
   <td style="text-align:right;font-style: italic;"> 0.153 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_13</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.067</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.23</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.297</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.093 </td>
   <td style="text-align:right;"> 0.092 </td>
   <td style="text-align:right;"> 0.089 </td>
   <td style="text-align:right;font-style: italic;"> 0.064 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.403</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.115</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.288</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.248 </td>
   <td style="text-align:right;"> 0.135 </td>
   <td style="text-align:right;"> 0.118 </td>
   <td style="text-align:right;font-style: italic;"> 0.088 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.889</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.042</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.153</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.983 </td>
   <td style="text-align:right;"> 0.197 </td>
   <td style="text-align:right;"> 0.164 </td>
   <td style="text-align:right;font-style: italic;"> 0.126 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.536</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">2.476</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: red !important;">0.94</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.979 </td>
   <td style="text-align:right;"> 0.316 </td>
   <td style="text-align:right;"> 0.384 </td>
   <td style="text-align:right;font-style: italic;"> 0.239 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_14</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-1.017</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.972</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.045</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.990 </td>
   <td style="text-align:right;"> 0.095 </td>
   <td style="text-align:right;"> 0.094 </td>
   <td style="text-align:right;font-style: italic;"> 0.066 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.134</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.272</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.138</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.205 </td>
   <td style="text-align:right;"> 0.111 </td>
   <td style="text-align:right;"> 0.103 </td>
   <td style="text-align:right;font-style: italic;"> 0.076 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.456</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.095</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.361</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.239 </td>
   <td style="text-align:right;"> 0.146 </td>
   <td style="text-align:right;"> 0.116 </td>
   <td style="text-align:right;font-style: italic;"> 0.091 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.168</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.366</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.198</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.294 </td>
   <td style="text-align:right;"> 0.216 </td>
   <td style="text-align:right;"> 0.173 </td>
   <td style="text-align:right;font-style: italic;"> 0.135 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_16</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.156</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.25</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.094</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.202 </td>
   <td style="text-align:right;"> 0.097 </td>
   <td style="text-align:right;"> 0.091 </td>
   <td style="text-align:right;font-style: italic;"> 0.066 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.009</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.091</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.1</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.046 </td>
   <td style="text-align:right;"> 0.130 </td>
   <td style="text-align:right;"> 0.112 </td>
   <td style="text-align:right;font-style: italic;"> 0.085 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.324</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.354</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.03</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.344 </td>
   <td style="text-align:right;"> 0.159 </td>
   <td style="text-align:right;"> 0.132 </td>
   <td style="text-align:right;font-style: italic;"> 0.102 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.611</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.435</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.176</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.508 </td>
   <td style="text-align:right;"> 0.253 </td>
   <td style="text-align:right;"> 0.201 </td>
   <td style="text-align:right;font-style: italic;"> 0.157 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_17</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.264</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.368</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.104</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.324 </td>
   <td style="text-align:right;"> 0.097 </td>
   <td style="text-align:right;"> 0.089 </td>
   <td style="text-align:right;font-style: italic;"> 0.066 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.389</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.421</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.032</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.412 </td>
   <td style="text-align:right;"> 0.146 </td>
   <td style="text-align:right;"> 0.128 </td>
   <td style="text-align:right;font-style: italic;"> 0.096 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.804</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.955</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.151</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.894 </td>
   <td style="text-align:right;"> 0.205 </td>
   <td style="text-align:right;"> 0.181 </td>
   <td style="text-align:right;font-style: italic;"> 0.136 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.182</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.545</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.363</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.373 </td>
   <td style="text-align:right;"> 0.288 </td>
   <td style="text-align:right;"> 0.290 </td>
   <td style="text-align:right;font-style: italic;"> 0.204 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_19</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-1.339</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-1.263</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.076</span> </td>
   <td style="text-align:right;font-style: italic;"> -1.297 </td>
   <td style="text-align:right;"> 0.100 </td>
   <td style="text-align:right;"> 0.098 </td>
   <td style="text-align:right;font-style: italic;"> 0.070 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.388</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.27</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.118</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.323 </td>
   <td style="text-align:right;"> 0.108 </td>
   <td style="text-align:right;"> 0.105 </td>
   <td style="text-align:right;font-style: italic;"> 0.075 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">0.101</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.333</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.434</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.143 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 0.111 </td>
   <td style="text-align:right;font-style: italic;"> 0.083 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.512</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.378</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.134</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.430 </td>
   <td style="text-align:right;"> 0.207 </td>
   <td style="text-align:right;"> 0.156 </td>
   <td style="text-align:right;font-style: italic;"> 0.125 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="border-bottom: 0;"><strong>PANAS_20</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c1 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.906</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.877</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.029</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.887 </td>
   <td style="text-align:right;"> 0.093 </td>
   <td style="text-align:right;"> 0.090 </td>
   <td style="text-align:right;font-style: italic;"> 0.065 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c2 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">-0.189</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">-0.257</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.068</span> </td>
   <td style="text-align:right;font-style: italic;"> -0.223 </td>
   <td style="text-align:right;"> 0.108 </td>
   <td style="text-align:right;"> 0.099 </td>
   <td style="text-align:right;font-style: italic;"> 0.073 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c3 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.037</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">0.585</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.452</span> </td>
   <td style="text-align:right;font-style: italic;"> 0.760 </td>
   <td style="text-align:right;"> 0.162 </td>
   <td style="text-align:right;"> 0.123 </td>
   <td style="text-align:right;font-style: italic;"> 0.098 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;font-weight: bold;" indentlevel="1"> c4 </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: burlywood !important;">1.463</span> </td>
   <td style="text-align:left;"> <span style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: lightblue !important;">1.941</span> </td>
   <td style="text-align:left;font-weight: bold;"> <span style="     color: black !important;">0.478</span> </td>
   <td style="text-align:right;font-style: italic;"> 1.756 </td>
   <td style="text-align:right;"> 0.277 </td>
   <td style="text-align:right;"> 0.238 </td>
   <td style="text-align:right;font-style: italic;"> 0.180 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Values highlighted in red are above the chosen cutoff 0.5 logits. Background color brown and blue indicate the lowest and highest values among the DIF groups.</td></tr>
</tfoot>
</table>

`````
:::
:::
#### Item threshold figure
::: {.cell}

```{.r .cell-code}
RIdifThreshFigLR(df, dif.group) + theme_rise()
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-57-1.png){width=672}
:::
:::


The item threshold table shows that the top threshold for item 13 differs more than 0.5 logits between groups. In this set of 8 items with 4 thresholds each, it is unlikely to result in problematic differences in estimated person scores.

### Logistic Ordinal Regression DIF {#sec-lordif}

The `lordif` package [@choi_lordif_2011] does not use a Rasch measurement model, it only offers a choice between the Graded Response Model (GRM) and the Generalized Partial Credit Model (GPCM). Both of these are 2PL models, meaning that they estimate a discrimination parameter for each item in addition to the item threshold parameters. `lordif` relies on the `mirt` package.

There are several nice features available in the `lordif` package. First, we get a χ2 test of uniform or non-uniform DIF. Second, there are three possible methods/criteria for flagging items with potential DIF. One of these uses a likelihood ratio (LR) χ2 test, while the other two are indicators of DIF size/magnitude, either using a pseudo R2 statistic ("McFadden", "Nagelkerke", or "CoxSnell") or a Beta criterion. For further details, see `?lordif` in your R console or the paper describing the package [@choi_lordif_2011].

Below is some sample code to get you started with `lordif`.

::: {.cell}

```{.r .cell-code}
library(lordif)

g_dif <- lordif(as.data.frame(df), as.numeric(dif.sex), # make sure that the data is in a dataframe-object and that the DIF variable is numeric
                criterion = c("Chisqr"), 
                alpha = 0.01, 
                beta.change = 0.1,
                model = "GPCM",
                R2.change = 0.02)

g_dif_sum <- summary(g_dif)
```
:::

::: {.cell}

```{.r .cell-code}
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

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> itemnr </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> ncat </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> chi12 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> chi13 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> chi23 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> beta12 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo12.McFadden </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo13.McFadden </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo23.McFadden </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo12.Nagelkerke </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo13.Nagelkerke </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo23.Nagelkerke </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo12.CoxSnell </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo13.CoxSnell </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> pseudo23.CoxSnell </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.323</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.004</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.003</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.005</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.005</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.005</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.005</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.013</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.019</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.192</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.008</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.106</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.057</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.077</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.007</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.182</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.401</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.83</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.64</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.17</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.068</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.32</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.032</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.008</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.008</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.011</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.011</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.01</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.01</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_19 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.178</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.25</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.33</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.002</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.68</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.349</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.164</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.001</span> </td>
  </tr>
</tbody>
</table>

`````
:::
:::

We can review the results regarding uniform/non-uniform DIF by looking at the `chi*` columns. Uniform DIF is indicated by column `chi12` and non-uniform DIF by `chi23`, while column `chi13` represents "an overall test of "total DIF effect" [@choi_lordif_2011].

While the table indicates significant chi2-tests for items 11 and 17, the magnitude estimates are low for these items.

There are some plots available as well, using the base R `plot()` function. For some reason the plots won't render in this Quarto document, so I will try to sort that out at some point.


::: {.cell layout-ncol="2"}

```{.r .cell-code}
plot(g_dif) # use option `graphics.off()` to get the plots rendered one by one
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-60-1.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-60-2.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-60-3.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-60-4.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-60-5.png){width=672}
:::

```{.r .cell-code}
#plot(g_dif, graphics.off())
```
:::


### Partial gamma DIF

The `iarm` package provides a function to assess DIF by partial gamma [@bjorner_differential_1998]. It should be noted that this function only shows a single partial gamma value per item, so if you have more than two groups in your comparison, you will want to also use other methods to understand your results better.

There are some recommended cutoff-values mentioned in the paper above:

No or negligible DIF:

- Gamma within the interval -0.21 to 0.21, *or*
- Gamma not significantly different from 0

Slight to moderate DIF:

- Gamma within the interval -0.31 to 0.31 (and outside -0.21 to 0.21), *or*
- not significantly outside the interval -0.21 to 0.21

Moderate to large DIF:

- Gamma outside the interval -0.31 to 0.31, **and**
- significantly outside the interval -0.21 to 0.21

::: {.cell}

```{.r .cell-code}
RIpartgamDIF(df, dif.sex)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Partial gamma </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> SE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Lower CI </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Upper CI </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Adjusted p-value (BH) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.234</span> </td>
   <td style="text-align:right;"> 0.054 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 0.341 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

`````
:::
:::

We can see "slight" DIF for item 17, with a statistically significant gamma of .23. 

## Rasch analysis 3

While there were no significant issues with DIF for any item/subgroup combination, we need to address the previously identified problem:

- Items 16 and 19 have the largest residual correlation.

We'll remove item 19 since item 16 has better targeting.

::: {.cell}

```{.r .cell-code}
removed.items <- c(removed.items,"PANAS_19")

df_backup2 <- df

df <- df_backup2 %>% 
  select(!any_of(removed.items))
```
:::


::: {.cell .column-margin}
::: {.cell-output-display}

<table class="table table-striped" style="font-size: 13px; font-family: Lato">
 <thead>
  <tr>
   <th style="text-align:center;"> itemnr </th>
   <th style="text-align:left;"> item </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_11</span> </td>
   <td style="text-align:left;"> Distressed </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_12</span> </td>
   <td style="text-align:left;"> Upset </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_13</span> </td>
   <td style="text-align:left;"> Hostile </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_14</span> </td>
   <td style="text-align:left;"> Irritable </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_16</span> </td>
   <td style="text-align:left;"> Afraid </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_17</span> </td>
   <td style="text-align:left;"> Ashamed </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style="color: grey; font-weight: bold">PANAS_20</span> </td>
   <td style="text-align:left;"> Jittery </td>
  </tr>
</tbody>
</table>

:::
:::


### Item fit

::: {.cell}

```{.r .cell-code}
simfit3 <- RIgetfit(df, iterations = 1000, cpu = 8)
RIitemfit(df, simfit3)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> InfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> OutfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit diff </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit diff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.127</span> </td>
   <td style="text-align:left;"> [0.924, 1.072] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.152</span> </td>
   <td style="text-align:left;"> [0.927, 1.079] </td>
   <td style="text-align:left;"> 0.055 </td>
   <td style="text-align:left;"> 0.073 </td>
   <td style="text-align:right;"> -0.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.783</span> </td>
   <td style="text-align:left;"> [0.91, 1.093] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.783</span> </td>
   <td style="text-align:left;"> [0.89, 1.096] </td>
   <td style="text-align:left;"> 0.127 </td>
   <td style="text-align:left;"> 0.107 </td>
   <td style="text-align:right;"> -0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.047</span> </td>
   <td style="text-align:left;"> [0.911, 1.102] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.127</span> </td>
   <td style="text-align:left;"> [0.885, 1.122] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:right;"> 0.42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.952</span> </td>
   <td style="text-align:left;"> [0.91, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.991</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.939</span> </td>
   <td style="text-align:left;"> [0.906, 1.087] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.968</span> </td>
   <td style="text-align:left;"> [0.889, 1.111] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.002</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.981</span> </td>
   <td style="text-align:left;"> [0.863, 1.172] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.162</span> </td>
   <td style="text-align:left;"> [0.904, 1.085] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.201</span> </td>
   <td style="text-align:left;"> [0.911, 1.093] </td>
   <td style="text-align:left;"> 0.077 </td>
   <td style="text-align:left;"> 0.108 </td>
   <td style="text-align:right;"> -0.05 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> MSQ values based on conditional calculations (n = 1851 complete cases).<br>                                Simulation based thresholds from 1000 simulated datasets.</td></tr>
</tfoot>
</table>

`````
:::
:::

### CICC

::: {.cell}

```{.r .cell-code}
CICCplot(PCM(df),
         which.item = c(1:3,7),
         lower.groups = c(0,7,14,21,28),
         grid.items = TRUE)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-65-1.png){width=672}
:::
:::

### Residual correlations

::: {.cell}

```{.r .cell-code}
simcor3 <- RIgetResidCor(df, iterations = 1000, cpu = 8)
RIresidcorr(df, cutoff = simcor3$p99)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;">   </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_11 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_12 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_13 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_14 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_16 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_17 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> PANAS_20 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.18</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.11</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.22</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.01</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">-0.04</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.14</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.06</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.26</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.26</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.2</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.09</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.14</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.22</span> </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0</span> </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.12</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.29</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.16</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.13</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.15</span> </td>
   <td style="text-align:left;"> <span style="     color: black !important;">-0.17</span> </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Relative cut-off value is -0.054, which is 0.093 above the average correlation (-0.147).<br>                                Correlations above the cut-off are highlighted in red text.</td></tr>
</tfoot>
</table>

`````
:::
:::

### Targeting

::: {.cell}

```{.r .cell-code}
RItargeting(df, bins = 45)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-67-1.png){width=672}
:::
:::

### Item hierarchy

::: {.cell}

```{.r .cell-code}
RIitemHierarchy(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-68-1.png){width=672}
:::
:::


### Analysis 3 comments

No problematic residual correlations remaining. Several items show misfit but we will end this sample analysis here and move on to show other functions.

There are several item thresholds that are very closely located, as shown in the item hierarchy figure. This is not ideal, since it will inflate reliability estimates. However, we will not modify the response categories for this analysis, we only note that this is not workable and should be dealt with by trying variations of merged response categories to achieve better separation of threshold locations without disordering.

## Reliability

::: {.cell}

```{.r .cell-code}
RItif(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-69-1.png){width=672}
:::
:::

The figure above shows the Test Information Function (TIF), which indicates the reliability of all items making up the test/scale (not the reliability of the sample).

The default cutoff value used in `RItif()` is TIF = 3.33, which corresponds to person separation index (PSI) = 0.7. PSI is similar to reliability coefficients such as omega and alpha, ranging from 0 to 1. You can change the TIF cutoff by using the option `cutoff`, for instance `cutoff = 2.5` (TIF values range from 1 and up).

While 11.8% of respondents had a floor effect based on the raw sum scored data, the figure above shows us that 41.8% are located below the point where the items produce a PSI of 0.7 or higher. Again, note that this figure shows the reliability of the test/scale, not the sample. If you want to add the sample reliability use option `samplePSI = TRUE`. More details are available in the documentation `?RItif`.

## Person fit

We can also look at how the respondents fit the Rasch model with these items. By default, `RIpfit()` outputs a histogram and a hex heatmap with the person infit ZSTD statistic, using +/- 1.96 as cutoff values. This is currently the only person fit method implemented in the `easyRasch` package, and the curious analyst is suggested to look at the package [PerFit](https://www.rdocumentation.org/packages/PerFit/versions/1.4.6/topics/PerFit-package) for more tools.

::: {.cell}

```{.r .cell-code}
RIpfit(df)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-70-1.png){width=672}
:::

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-70-2.png){width=672}
:::
:::

You can export the person fit values to a new variable in the dataframe by specifying `output = "dataframe"`, or if you just want the row numbers for respondents with deviant infit values, `output = "rowid"`.

You can also specify a grouping variable to visualize the person fit for different groups.

::: {.cell}

```{.r .cell-code}
RIpfit(df, group = dif.sex, output = "heatmap")
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-71-1.png){width=672}
:::
:::

Person fit is a useful way to identify respondents with unexpected response patterns and investigate this further.

### `PerFit` sample code

While none of the functions in the `PerFit` package has been implemented in `easyRasch`, this is some code to get you started if you are interested in using it. There are multiple methods/functions available for polytomous and dichotomous data, see the package [documentation](https://www.rdocumentation.org/packages/PerFit/versions/1.4.6/topics/PerFit-package).

For this example, we'll use the non-parametric U3 statistic generalized to polytomous items [@emons_nonparametric_2008].

#### U3poly
::: {.cell}

```{.r .cell-code}
library(PerFit)
pfit_u3poly <- U3poly(matrix = df, 
                      Ncat = 5, # make sure to input number of response categories, not thresholds
                      IRT.PModel = "PCM")
```
:::
#### Cutoff information
::: {.cell}

```{.r .cell-code}
cutoff(pfit_u3poly)
```

::: {.cell-output .cell-output-stdout}

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
```


:::
:::
#### Flagged respondents
::: {.cell}

```{.r .cell-code}
flagged.resp(pfit_u3poly) %>% 
  pluck("Scores") %>% 
  as.data.frame() %>% 
  arrange(desc(PFscores))
```

::: {.cell-output .cell-output-stdout}

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
```


:::
:::


The dataframe shown under the tab `Flagged respondents` above contains a variable named `FlaggedID` which represents the row id's. This variable is useful if one wants to filter out respondents with deviant response patterns (person misfit). There are indications that persons with misfit may affect results of Andersen's LR-test for DIF [@artner_simulation_2016].

### Item fit without aberrant responses

We can remove the misfitting persons to see how that affects item fit. Let's also compare with the misfitting respondents identified by `RIpfit()`.

::: {.cell}

```{.r .cell-code}
misfits <- flagged.resp(pfit_u3poly) %>% 
  pluck("Scores") %>% 
  as.data.frame() %>% 
  pull(FlaggedID)

misfits2 <- RIpfit(df, output = "rowid")
```
:::

#### All respondents
::: {.cell}

```{.r .cell-code}
RIitemfit(df, simcut = simfit3)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> InfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> OutfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit diff </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit diff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.127</span> </td>
   <td style="text-align:left;"> [0.924, 1.072] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.152</span> </td>
   <td style="text-align:left;"> [0.927, 1.079] </td>
   <td style="text-align:left;"> 0.055 </td>
   <td style="text-align:left;"> 0.073 </td>
   <td style="text-align:right;"> -0.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.783</span> </td>
   <td style="text-align:left;"> [0.91, 1.093] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.783</span> </td>
   <td style="text-align:left;"> [0.89, 1.096] </td>
   <td style="text-align:left;"> 0.127 </td>
   <td style="text-align:left;"> 0.107 </td>
   <td style="text-align:right;"> -0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.047</span> </td>
   <td style="text-align:left;"> [0.911, 1.102] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.127</span> </td>
   <td style="text-align:left;"> [0.885, 1.122] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:right;"> 0.42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.952</span> </td>
   <td style="text-align:left;"> [0.91, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.991</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.939</span> </td>
   <td style="text-align:left;"> [0.906, 1.087] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.968</span> </td>
   <td style="text-align:left;"> [0.889, 1.111] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.002</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.981</span> </td>
   <td style="text-align:left;"> [0.863, 1.172] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.162</span> </td>
   <td style="text-align:left;"> [0.904, 1.085] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.201</span> </td>
   <td style="text-align:left;"> [0.911, 1.093] </td>
   <td style="text-align:left;"> 0.077 </td>
   <td style="text-align:left;"> 0.108 </td>
   <td style="text-align:right;"> -0.05 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> MSQ values based on conditional calculations (n = 1851 complete cases).<br>                                Simulation based thresholds from 1000 simulated datasets.</td></tr>
</tfoot>
</table>

`````
:::
:::
#### U3 misfit removed
::: {.cell}

```{.r .cell-code}
RIitemfit(df[-misfits,], simcut = simfit3)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> InfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> OutfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit diff </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit diff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.124</span> </td>
   <td style="text-align:left;"> [0.924, 1.072] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.133</span> </td>
   <td style="text-align:left;"> [0.927, 1.079] </td>
   <td style="text-align:left;"> 0.052 </td>
   <td style="text-align:left;"> 0.054 </td>
   <td style="text-align:right;"> -0.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.809</span> </td>
   <td style="text-align:left;"> [0.91, 1.093] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.757</span> </td>
   <td style="text-align:left;"> [0.89, 1.096] </td>
   <td style="text-align:left;"> 0.101 </td>
   <td style="text-align:left;"> 0.133 </td>
   <td style="text-align:right;"> -0.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.98</span> </td>
   <td style="text-align:left;"> [0.911, 1.102] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.865</span> </td>
   <td style="text-align:left;"> [0.885, 1.122] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> 0.02 </td>
   <td style="text-align:right;"> 0.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.024</span> </td>
   <td style="text-align:left;"> [0.91, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.066</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.959</span> </td>
   <td style="text-align:left;"> [0.906, 1.087] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.917</span> </td>
   <td style="text-align:left;"> [0.889, 1.111] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.018</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.881</span> </td>
   <td style="text-align:left;"> [0.863, 1.172] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.136</span> </td>
   <td style="text-align:left;"> [0.904, 1.085] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.149</span> </td>
   <td style="text-align:left;"> [0.911, 1.093] </td>
   <td style="text-align:left;"> 0.051 </td>
   <td style="text-align:left;"> 0.056 </td>
   <td style="text-align:right;"> -0.07 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> MSQ values based on conditional calculations (n = 1669 complete cases).<br>                                Simulation based thresholds from 1000 simulated datasets.</td></tr>
</tfoot>
</table>

`````
:::
:::
#### ZSTD misfit removed
::: {.cell}

```{.r .cell-code}
RIitemfit(df[-misfits2,], simcut = simfit3)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Item </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> InfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> OutfitMSQ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit thresholds </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Infit diff </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Outfit diff </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PANAS_11 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.086</span> </td>
   <td style="text-align:left;"> [0.924, 1.072] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.093</span> </td>
   <td style="text-align:left;"> [0.927, 1.079] </td>
   <td style="text-align:left;"> 0.014 </td>
   <td style="text-align:left;"> 0.014 </td>
   <td style="text-align:right;"> -0.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_12 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.8</span> </td>
   <td style="text-align:left;"> [0.91, 1.093] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">0.796</span> </td>
   <td style="text-align:left;"> [0.89, 1.096] </td>
   <td style="text-align:left;"> 0.11 </td>
   <td style="text-align:left;"> 0.094 </td>
   <td style="text-align:right;"> -0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_13 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.027</span> </td>
   <td style="text-align:left;"> [0.911, 1.102] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.042</span> </td>
   <td style="text-align:left;"> [0.885, 1.122] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_14 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.977</span> </td>
   <td style="text-align:left;"> [0.91, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.006</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_16 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.943</span> </td>
   <td style="text-align:left;"> [0.906, 1.087] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.971</span> </td>
   <td style="text-align:left;"> [0.889, 1.111] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> -0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_17 </td>
   <td style="text-align:left;"> <span style="     color: black !important;">1.02</span> </td>
   <td style="text-align:left;"> [0.915, 1.095] </td>
   <td style="text-align:left;"> <span style="     color: black !important;">0.993</span> </td>
   <td style="text-align:left;"> [0.863, 1.172] </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:left;"> no misfit </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PANAS_20 </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.159</span> </td>
   <td style="text-align:left;"> [0.904, 1.085] </td>
   <td style="text-align:left;"> <span style="     color: red !important;">1.186</span> </td>
   <td style="text-align:left;"> [0.911, 1.093] </td>
   <td style="text-align:left;"> 0.074 </td>
   <td style="text-align:left;"> 0.093 </td>
   <td style="text-align:right;"> -0.08 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> MSQ values based on conditional calculations (n = 1695 complete cases).<br>                                Simulation based thresholds from 1000 simulated datasets.</td></tr>
</tfoot>
</table>

`````
:::
:::


## Item parameters

To allow others (and oneself) to use the item parameters estimated for estimation of person locations/thetas, we should make the item parameters available. The function will also write a csv-file with the item threshold locations. Estimations of person locations/thetas can be done with the `thetaEst()` function from the `catR` package. This is implemented in the function `RIestThetasOLD()`, see below for details.

First, we'll output the parameters into a table.

::: {.cell}

```{.r .cell-code}
RIitemparams(df)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:90%; font-size: 15px; width: auto !important;  font-family: Lato; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;border-bottom: 0;" class="table table-striped table-hover lightable-classic table">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">   </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Threshold 1 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Threshold 2 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Threshold 3 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Threshold 4 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Item location </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> PANAS_11 </td>
   <td style="text-align:right;"> -1.63 </td>
   <td style="text-align:right;"> -0.67 </td>
   <td style="text-align:right;"> -0.22 </td>
   <td style="text-align:right;"> 1.21 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    text-align: right;">-0.33</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> PANAS_12 </td>
   <td style="text-align:right;"> -0.80 </td>
   <td style="text-align:right;"> -0.39 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    text-align: right;">-0.06</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> PANAS_13 </td>
   <td style="text-align:right;"> -0.29 </td>
   <td style="text-align:right;"> -0.15 </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> 1.53 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    text-align: right;">0.42</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> PANAS_14 </td>
   <td style="text-align:right;"> -1.38 </td>
   <td style="text-align:right;"> -0.59 </td>
   <td style="text-align:right;"> -0.16 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    text-align: right;">-0.32</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> PANAS_16 </td>
   <td style="text-align:right;"> -0.59 </td>
   <td style="text-align:right;"> -0.44 </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> 1.08 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    text-align: right;">0</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> PANAS_17 </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.48 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    text-align: right;">0.34</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> PANAS_20 </td>
   <td style="text-align:right;"> -1.27 </td>
   <td style="text-align:right;"> -0.61 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 1.32 </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    text-align: right;">-0.05</span> </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Item location is the average of the thresholds for each item.</td></tr>
</tfoot>
</table>

`````
:::
:::

The parameters can also be output to a dataframe or a file, using the option `output = "dataframe"` or `output = "file"`.

## Ordinal sum score to interval score

This table shows the corresponding "raw" ordinal sum score values and logit scores, with standard errors for each logit value. Interval scores are estimated using WL based on a simulated dataset using the item parameters estimated from the input dataset. The choice of WL as default is due to the lower bias compared to ML estimation [@warm1989].

(An option will hopefully be added at some point to create this table based on only item parameters.)

::: {.cell}

```{.r .cell-code}
RIscoreSE(df)
```

::: {.cell-output-display}
`````{=html}
<table data-quarto-disable-processing="true" style="width:65%; font-size: 14px;  font-family: Arial; margin-left: auto; margin-right: auto;" class="table table-striped table-hover lightable-classic">
 <thead>
  <tr>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Ordinal sum score </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Logit score </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;font-weight: bold;"> Logit std.error </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -3.642 </td>
   <td style="text-align:right;"> 0.620 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -2.543 </td>
   <td style="text-align:right;"> 0.716 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -2.032 </td>
   <td style="text-align:right;"> 0.653 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> -1.693 </td>
   <td style="text-align:right;"> 0.578 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> -1.437 </td>
   <td style="text-align:right;"> 0.515 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> -1.228 </td>
   <td style="text-align:right;"> 0.468 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> -1.050 </td>
   <td style="text-align:right;"> 0.432 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> -0.893 </td>
   <td style="text-align:right;"> 0.406 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> -0.750 </td>
   <td style="text-align:right;"> 0.386 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> -0.618 </td>
   <td style="text-align:right;"> 0.371 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> -0.493 </td>
   <td style="text-align:right;"> 0.361 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> -0.373 </td>
   <td style="text-align:right;"> 0.353 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> -0.258 </td>
   <td style="text-align:right;"> 0.348 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> -0.144 </td>
   <td style="text-align:right;"> 0.346 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> -0.031 </td>
   <td style="text-align:right;"> 0.346 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0.083 </td>
   <td style="text-align:right;"> 0.348 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 0.198 </td>
   <td style="text-align:right;"> 0.352 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 0.317 </td>
   <td style="text-align:right;"> 0.359 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 0.441 </td>
   <td style="text-align:right;"> 0.368 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 0.572 </td>
   <td style="text-align:right;"> 0.381 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 0.712 </td>
   <td style="text-align:right;"> 0.397 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 0.866 </td>
   <td style="text-align:right;"> 0.418 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 1.037 </td>
   <td style="text-align:right;"> 0.447 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 1.233 </td>
   <td style="text-align:right;"> 0.484 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 1.464 </td>
   <td style="text-align:right;"> 0.534 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 1.747 </td>
   <td style="text-align:right;"> 0.601 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 2.118 </td>
   <td style="text-align:right;"> 0.681 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 2.666 </td>
   <td style="text-align:right;"> 0.745 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 3.804 </td>
   <td style="text-align:right;"> 0.638 </td>
  </tr>
</tbody>
</table>

`````
:::
:::

### Ordinal/interval figure

The figure below can also be generated to illustrate the relationship between ordinal sum score and logit interval score. The errorbars default to show the standard error at each point, multiplied by 1.96.

::: {.cell}

```{.r .cell-code}
RIscoreSE(df, output = "figure")
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-81-1.png){width=672}
:::
:::

### Estimating interval level person scores

Based on the Rasch analysis output of item parameters, we can estimate each individuals location or score (also known as "theta"). `RIestThetas()` by default uses WLE estimation based on item parameters from a partial credit model (PCM) and outputs a dataframe with person locations (WLE) and measurement error (SEM) on the logit scale.

::: {.cell}

```{.r .cell-code}
thetas <- RIestThetas(df)

head(thetas)
```

::: {.cell-output .cell-output-stdout}

```
         WLE       SEM
1 -2.5430672 0.7160122
2 -2.0319918 0.6533338
3 -0.1439296 0.3460439
4 -3.6420132 0.6202853
5 -2.0319918 0.6533338
6  0.1980813 0.3520345
```


:::
:::

Each individual has a standard error of measurement (SEM) associated with their estimated location/score. This is included in the output of the `RIestThetas()` function as the `SEM` variable, as seen above. We can review the distribution of measurement error with a figure.

We can take a look at the distribution of person locations (thetas) using a histogram.

::: {.cell}

```{.r .cell-code}
hist(thetas$WLE, 
     col = "#009ca6", 
     main = "Histogram of person locations (thetas)", 
     breaks = 20)
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-83-1.png){width=672}
:::
:::


`RIestThetasOLD()` can be used with a pre-specified item (threshold) location matrix. The choice of WL as default is due to the lower bias compared to ML estimation [@warm1989]. Similarly to `RIscoreSE()` you can (and may indeed need to) change the range of logit scores, using the option `theta_range`. The default is `c(-7,7)`, which should hopefully work in most circumstances.

If you would like to use an existing item threshold location matrix, this code may be helpful:

::: {.cell}

```{.r .cell-code}
itemParameters <- read_csv("itemParameters.csv") %>% 
  as.matrix()
```
:::

This creates a matrix object (not a dataframe), with each item as a row, and the threshold locations as columns.

## Figure design

Most of the figures created by the functions can be styled (colors, fonts, etc) by adding theme settings to them. You can use the standard ggplot function `theme()` and related theme-functions. As usual it is possible to "stack" theme functions, as seen in the example below.

You can also change coloring, axis limits/breaks, etc, just by adding ggplot options with a `+` sign.

A custom theme function, `theme_rise()`, is included in the `easyRasch` package. It might be easier to use if you are not familiar with `theme()`.

For instance, you might like to change the font to "Lato" for the item hierarchy figure, and make the background transparent.

::: {.cell}

```{.r .cell-code}
RIitemHierarchy(df) +
  theme_minimal() + # first apply the minimal theme to make the background transparent
  theme_rise(fontfamily = "Lato") # then apply theme_rise, which simplifies making changes to all plot elements
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-85-1.png){width=672}
:::
:::

As of package version 0.1.30.0, the `RItargeting()` function allows more flexibility in styling too, by having an option to return a list object with the three separate plots. See the [NEWS](https://github.com/pgmj/easyRasch/blob/main/NEWS.md#01300) file for more details. Since the `RItargeting()` function uses the `patchwork` library to combine plots, you can also make use of [the many functions that `patchwork` includes](https://patchwork.data-imaginist.com/articles/patchwork.html). For instance, you can set a title with a specific theme:

::: {.cell}

```{.r .cell-code}
RItargeting(df) + plot_annotation(title = "Targeting", theme = theme_rise(fontfamily = "Arial"))
```

::: {.cell-output-display}
![](easyrasch-vignette_files/figure-html/unnamed-chunk-86-1.png){width=672}
:::
:::

In order to change font for text *inside* plots (such as "t1" for thresholds) you will need to add an additional line of code.

``` r
update_geom_defaults("text", list(family = "Lato"))
```

Please note that the line of code above updates the default settings for `geom_text()` for the whole session. Also, some functions, such as `RIloadLoc()`, make use of `geom_text_repel()`, for which you would need to change the code above from "text" to "text_repel".

A simple way to only change font family and font size would be to use `theme_minimal(base_family = "Calibri", base_size = 14)`. Please see the [reference page](https://ggplot2.tidyverse.org/reference/ggtheme.html) for default ggplot themes for alternatives to `theme_minimal()`.

## Software used {#sec-grateful}

The `grateful` package is a nice way to give credit to the packages used in making the analysis. The package can create both a bibliography file and a table object, which is handy for automatically creating a reference list based on the packages used (or at least explicitly loaded).

::: {.cell}

```{.r .cell-code}
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

::: {.cell-output-display}

<table class="table table-striped" style="font-size: 13px; font-family: Lato; width: 80%">
 <thead>
  <tr>
   <th style="text-align:right;"> Package </th>
   <th style="text-align:right;"> Version </th>
   <th style="text-align:right;"> Citation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> base </td>
   <td style="text-align:right;"> 4.4.2 </td>
   <td style="text-align:right;"> @base </td>
  </tr>
</tbody>
</table>

:::
:::

## Additional credits

Thanks to my [colleagues at RISE](https://www.ri.se/en/what-we-do/projects/center-for-category-based-measurements) for providing feedback and testing the package on Windows and MacOS platforms. Also, thanks to [Mike Linacre](https://www.winsteps.com/linacre.htm) and [Jeanette Melin](https://www.ri.se/en/person/jeanette-melin) for providing useful feedback to improve this vignette.

## Session info

::: {.cell}

```{.r .cell-code}
sessionInfo()
```

::: {.cell-output .cell-output-stdout}

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
```


:::
:::


## References

