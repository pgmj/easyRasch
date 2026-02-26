# Create tile plot for all items, also showing the count of responses in each response category for each item

Create tile plot for all items, also showing the count of responses in
each response category for each item

## Usage

``` r
RItileplot(
  data,
  cutoff = 10,
  highlight = TRUE,
  percent = FALSE,
  text_color = "orange"
)
```

## Arguments

- data:

  Dataframe with item data only

- cutoff:

  Conditional highlighting of text in cells with n below cutoff

- highlight:

  Defaults to TRUE. Set to FALSE to disable text highlighting

- percent:

  Set to TRUE to replace n with percentage of item responses
