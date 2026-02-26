# Item response distribution figure

A figure showing each item's response distribution and n + % of
responses in each category. If you use Quarto, you may need to increase
the code chunk setting for `fig-height` to something above 5 (default).

## Usage

``` r
RIitemcols(
  data,
  ncols = 1,
  labelwrap = 25,
  text_ypos = 6,
  viridis_end = 0.9,
  font = "sans"
)
```

## Arguments

- data:

  Dataframe/tibble with only item response data coded as integers

- ncols:

  How many columns to display, defaults to 1

- labelwrap:

  Number of characters to show on each row in item description

- text_ypos:

  Position on y axis for the text on each column with n responses

- viridis_end:

  If you need to adapt the coloring of text on columns (0.9 is default)

- font:

  Choose font family

## Details

Works best with the `itemlabels` object set up as described in the
package README: <https://pgmj.github.io/easyRasch/#using-the-package>
