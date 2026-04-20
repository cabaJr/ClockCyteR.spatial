# Calculate plot height from type and width

Calculate plot height from type and width

## Usage

``` r
plot_height(plot_type, plot_width, ...)
```

## Arguments

- plot_type:

  Character; plot-type key (see
  [`plot_group()`](https://cabajr.github.io/ClockCyteR.spatial/reference/plot_group.md)).

- plot_width:

  Numeric; plot width in pixels.

- ...:

  Optional: `cell_coords` (data frame with column `Y`) for `"spatial"`
  plots; `node_data` (data frame with column `Y`) for
  `"network_spatial"` plots.

## Value

Numeric; recommended plot height in pixels.
