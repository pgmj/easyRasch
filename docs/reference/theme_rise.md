# A simple ggplot theme for RISE formatting

Use is optional :)

## Usage

``` r
theme_rise(
  fontfamily = "Lato",
  axissize = 13,
  titlesize = 15,
  margins = 12,
  axisface = "plain",
  panelDist = 0.6,
  ...
)
```

## Arguments

- fontfamily:

  Font family for all plot text

- axissize:

  Font size for axis labels

- titlesize:

  Font size for plot title

- margins:

  Distance of axis labels to plot

- axisface:

  Set to "bold" if you want bold axis labels

## Value

Add + theme_rise() to your ggplot or RIfunction that outputs a ggplot

## Details

See ?element_text for more details on available settings.

Please note that using this theme does not update the session defaults
for geom_text and geom_text_repel. You can add the relevant line(s)
manually:

    update_geom_defaults("text", list(family = fontfamily)) +
    update_geom_defaults("text_repel", list(family = fontfamily))
