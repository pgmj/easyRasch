# Conditional Item Characteristic Curves

A wrapper function to simplify getting CICC curves from
[`iarm::ICCplot()`](https://rdrr.io/pkg/iarm/man/ICCplot.html) for any
number of items in the same figure. Uses the `patchwork` package, which
also allows for further additions to a plot using for instance:

## Usage

``` r
RIciccPlot(data, class_intervals = 5, method = "cut", dif = "no", dif_var = NA)
```

## Arguments

- data:

  Dataframe/tibble with only item response data coded as integers

- method:

  Either "cut" (default) or "score" for all possible total scores

- dif:

  Defaults to "no". Needs a defined `dif_var` if set to "yes"

- dif_var:

  An exogenous variable (ie. age group, sex) coded as a factor

- classintervals:

  Number of groups to divide respondents into

## Details

`+ plot_annotation(subtitle = "Some subtitle")`. See
[`?plot_annotation`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)
for more possibilities.

A useful option is for DIF analysis, which requires two optional
settings: `dif = "yes"` and `dif_var = your$difvariable`.

Text from [`?iarm::ICCplot`](https://rdrr.io/pkg/iarm/man/ICCplot.html):

Plots Item Characteristic Curves for dichotomous and polytomous items
using average scores within adjacent class intervals (method="cut").
