# Generates a plot showing the first residual contrast loadings based on a PCA of Rasch model residuals vs item locations.

Defaults to PCM, use `model = "RM"` for dichotomous data.

## Usage

``` r
RIloadLoc(dfin, output = "figure", pcx = c("PC1", "PC2", "PC3"), model = "PCM")
```

## Arguments

- dfin:

  Dataframe with item data only

- output:

  Either "figure" (default) or "dataframe"

- pcx:

  Number of principal components to output for "dataframe"

- model:

  Defaults to "PCM", use "RM" for dichotomous data

## Value

A plot with item locations (y) and loadings (x)

## Details

Note. This function does not work with missing responses in the dataset.
Missing data is automatically filtered out.
