# Doughnut chart of cluster abundance

Doughnut chart of cluster abundance

## Usage

``` r
doughnut_plot(node_data, colors_map)
```

## Arguments

- node_data:

  Data frame with a `cluster` column.

- colors_map:

  Data frame with columns `cluster` and `color`, as returned by
  [`assign_cluster_colors()`](https://cabajr.github.io/ClockCyteR.spatial/reference/assign_cluster_colors.md).

## Value

A `ggplot` object.
