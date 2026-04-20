# Compute local spatial coherence for a period-analysis variable

Compute local spatial coherence for a period-analysis variable

## Usage

``` r
coherence_analysis(
  period_table,
  grid_coord,
  variable,
  radius = 100,
  threshold = 0.1,
  merge = TRUE,
  ...
)
```

## Arguments

- period_table:

  Data frame of period-analysis results; must contain an `ID` column and
  the column named by `variable`.

- grid_coord:

  Data frame of grid coordinates with three columns: ID, X, Y.

- variable:

  Character; name of the column in `period_table` to use as the
  coherence metric.

- radius:

  Numeric; neighbourhood radius in micrometres. Defaults to `100`.

- threshold:

  Numeric; minimum coherence value to retain a cell. Defaults to `0.1`.

- merge:

  Logical; if `TRUE`, merges coherence results back into `period_table`.
  Defaults to `TRUE`.

- ...:

  Currently unused.

## Value

A data frame of per-cell coherence scores merged with the input
`period_table`.
