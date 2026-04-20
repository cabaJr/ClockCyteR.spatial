# collection of funtions used in network analysis Compute pairwise cell correlations and return edge list

collection of funtions used in network analysis Compute pairwise cell
correlations and return edge list

## Usage

``` r
traces_correlation(
  method = "combined",
  traces,
  network_dir,
  filename,
  interval_name = "Full",
  grid_coord,
  cell_nodes,
  plot_matrix = TRUE,
  filter_nonrhythmic = FALSE
)
```

## Arguments

- method:

  Character; correlation method, either `"spearman"` or `"combined"`
  (Spearman + FFT-based rhythm similarity). Defaults to `"combined"`.

- traces:

  Numeric matrix of cell fluorescence traces (cells × frames).

- network_dir:

  Character; directory for saving correlation matrix plots.

- filename:

  Character; file identifier used in plot filenames.

- interval_name:

  Character; interval label used in plot filenames. Defaults to
  `"Full"`.

- grid_coord:

  Data frame of grid coordinates with columns `X` and `Y`, used for
  spatial weighting in `"combined"` mode.

- cell_nodes:

  Data frame of node metadata; must contain a `name` column matching row
  indices of `traces`.

- plot_matrix:

  Logical; whether to save a heatmap of the correlation matrix. Defaults
  to `TRUE`.

- filter_nonrhythmic:

  Logical; whether to remove edges with `NA` weights before returning.
  Defaults to `FALSE`.

## Value

A data frame of edges with columns `from`, `to`, and `weight` (Fisher
z-transformed correlation).
