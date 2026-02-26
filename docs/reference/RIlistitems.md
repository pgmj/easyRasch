# Show items based on itemlabels file

Requires a dataframe with two columns, labeled "itemnr" and "item",
containing information on the item numbers/labels and item
content/description. This dataframe has to be labeled `itemlabels`.

## Usage

``` r
RIlistitems(dfin, all.items = FALSE, ...)
```

## Arguments

- dfin:

  Dataframe with item data only

- all.items:

  Set to TRUE to list all items in 'itemlabels' df

## Value

A table with items used in dataframe

## Details

Default behavior is to only list items that are in the dataframe. Any
items eliminated during analysis process will not be included.

If all items in the original dataset are to be shown, use option
"all.items = TRUE".
