# Jitter + boxplot of a variable stratified by cluster

Jitter + boxplot of a variable stratified by cluster

## Usage

``` r
cluster_plot(
  data,
  x_var,
  y_var,
  color_var,
  alpha_point = 0.3,
  alpha_box = 0.2,
  colorscale,
  x_label = "Cluster",
  y_label = "",
  ylims = NULL
)
```

## Arguments

- data:

  Data frame containing the variables to plot.

- x_var:

  Character; column name to use on the x-axis (cluster grouping).

- y_var:

  Character; column name to use on the y-axis (numeric variable).

- color_var:

  Character; column name used for point and box color.

- alpha_point:

  Numeric; transparency of jittered points. Defaults to `0.3`.

- alpha_box:

  Numeric; transparency of the boxplot fill. Defaults to `0.2`.

- colorscale:

  Named character vector mapping cluster levels to colors.

- x_label:

  Character; x-axis label. Defaults to `"Cluster"`.

- y_label:

  Character; y-axis label. Defaults to `""`.

- ylims:

  Numeric vector of length 2 passed to `ylim()`; or `NULL` to use
  automatic limits. Defaults to `NULL`.

## Value

A `ggplot` object.
