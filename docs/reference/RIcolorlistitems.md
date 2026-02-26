# Show items based on itemlabels file, with coloring options

Requires a dataframe with two columns, labeled "itemnr" and "item",
containing information on the item numbers (qN) and item content. This
dataframe has to be labeled "itemlabels".

## Usage

``` r
RIcolorlistitems(items, color)
```

## Arguments

- items:

  vector of row numbers for items to colorize background

- color:

  color of background ("lightblue" is default)

## Value

A table with items used in dataframe

## Details

Input a vector of item rows, i.e c(1,3,5) to colorize items 1, 3 and 5.
Optionally choose which background color will be used. "Lightblue" is
the default. Text will be black, so choose a light color which gives
good contrast for readability.
