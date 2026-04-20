# Prepare layout components for network plotting

Prepare layout components for network plotting

## Usage

``` r
network_plot_layout(igraph_obj, channel_nodes, color_map)
```

## Arguments

- igraph_obj:

  An igraph object whose vertex names correspond to rows in
  `channel_nodes`.

- channel_nodes:

  Data frame of node metadata with columns `X`, `Y`, and `module` (or
  matching vertex attributes).

- color_map:

  Data frame with columns `cluster` and `color`, as returned by
  [`assign_cluster_colors()`](https://cabajr.github.io/ClockCyteR.spatial/reference/assign_cluster_colors.md).
