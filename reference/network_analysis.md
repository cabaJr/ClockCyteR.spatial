# Build and cluster a cell correlation network

Build and cluster a cell correlation network

## Usage

``` r
network_analysis(
  filename,
  interval_name,
  cell_traces,
  grid_coord,
  period_table,
  network_dir,
  ...
)
```

## Arguments

- filename:

  Character; file identifier used in plot filenames.

- interval_name:

  Character; interval label used in plot filenames.

- cell_traces:

  Numeric matrix of fluorescence traces (cells × frames); row names must
  be cell IDs.

- grid_coord:

  Data frame of grid coordinates with columns `ID`, `X`, and `Y`.

- period_table:

  Data frame of period-analysis results containing an `ID` column.

- network_dir:

  Character; directory for saving network output files.

- ...:

  Optional: `plot_matrix` (logical, save correlation heatmap);
  `filter_nonrhythmic` (logical, remove NA-weight edges).

## Value

A named list with elements `igraph_obj`, `node_data`, and
`network_vars`.
