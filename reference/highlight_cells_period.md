# Spatial map of a period-analysis variable with linear color scale

Spatial map of a period-analysis variable with linear color scale

## Usage

``` r
highlight_cells_period(
  all_cells,
  period_table,
  variable,
  filename = "",
  colorcode = "blue",
  sdFactor = 1,
  shape = 15,
  size = 2,
  ID_col = "ID",
  ...
)
```

## Arguments

- all_cells:

  Data frame (or matrix) with three columns: cell ID, X coordinate, and
  Y coordinate.

- period_table:

  Data frame of period-analysis results; must contain a column matching
  `ID_col`.

- variable:

  Character; name of the column in `period_table` to map to color.

- filename:

  Character; used in the plot title. Defaults to `""`.

- colorcode:

  Character; color used for the gradient high end. Defaults to `"blue"`.

- sdFactor:

  Numeric; number of SDs around the mean used to clip the color scale.
  Defaults to `1`.

- shape:

  Integer; point shape code. Defaults to `15`.

- size:

  Numeric; point size. Defaults to `2`.

- ID_col:

  Character; name of the ID column shared between `all_cells` and
  `period_table`. Defaults to `"ID"`.

- ...:

  Currently unused.

## Value

A `ggplot` object.
