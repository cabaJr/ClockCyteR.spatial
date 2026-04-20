# Boxplot of a metric stratified by group with optional significance brackets

Boxplot of a metric stratified by group with optional significance
brackets

## Usage

``` r
metric_boxplot(
  data,
  metric,
  grouping_var,
  ylabel = "",
  xlabel = "",
  comparison = TRUE,
  test = "bonferroni",
  norm_test = TRUE,
  plot_colors,
  ...
)
```

## Arguments

- data:

  Data frame containing `metric` and `grouping_var`.

- metric:

  Unquoted column name of the numeric variable to plot.

- grouping_var:

  Unquoted column name of the grouping factor.

- ylabel:

  Character; y-axis label. Defaults to `""`.

- xlabel:

  Character; x-axis label. Defaults to `""`.

- comparison:

  Logical; whether to add pairwise significance brackets. Defaults to
  `TRUE`.

- test:

  Character; p-value correction method passed to the stat test (e.g.
  `"bonferroni"`). Defaults to `"bonferroni"`.

- norm_test:

  Logical; whether to run a Shapiro–Wilk normality test before selecting
  the statistical test. Defaults to `TRUE`.

- plot_colors:

  Named character vector of colors, one per group level.

- ...:

  Currently unused.

## Value

A `ggplot` object.
