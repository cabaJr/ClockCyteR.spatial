# Render an igraph network with one of several preset styles

Render an igraph network with one of several preset styles

## Usage

``` r
network_plot(
  igraph_obj,
  color_map,
  type,
  layout_list,
  network_dir,
  width = 6,
  height = 8,
  plot_suffix = "",
  membership_vec = NULL
)
```

## Arguments

- igraph_obj:

  An igraph object to plot.

- color_map:

  Data frame with columns `cluster` and `color`, as returned by
  [`assign_cluster_colors()`](https://cabajr.github.io/ClockCyteR.spatial/reference/assign_cluster_colors.md).

- type:

  Character; plot style. One of `"Vertex_edges"`, `"Vertex"`,
  `"Vertex_edges_empty"`, `"Edges"`, `"Clusters_skeleton"`, or
  `"Clusters_connected"`.

- layout_list:

  List of layout components as returned by
  [`network_plot_layout()`](https://cabajr.github.io/ClockCyteR.spatial/reference/network_plot_layout.md).

- network_dir:

  Character; directory where SVG files are saved.

- width:

  Numeric; SVG device width in inches. Defaults to `6`.

- height:

  Numeric; SVG device height in inches. Defaults to `8`.

- plot_suffix:

  Character; optional suffix appended to the output filename. Defaults
  to `""`.

- membership_vec:

  Integer vector mapping each vertex to a cluster, used for
  `"Clusters_skeleton"` and `"Clusters_connected"` types. Defaults to
  `NULL`.
