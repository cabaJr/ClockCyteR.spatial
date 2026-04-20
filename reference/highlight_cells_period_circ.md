# Spatial map of a period-analysis variable with circular (phase) color scale

Spatial map of a period-analysis variable with circular (phase) color
scale

## Usage

``` r
highlight_cells_period_circ(
  all_cells,
  period_table,
  variable,
  filename = "",
  shape = 15,
  size = 2,
  normalized = FALSE,
  colorcode = "rainbow",
  invert_color = FALSE,
  ID_col = "ID",
  ...
)
```

## Arguments

- all_cells:

  Data frame (or matrix) with three columns: cell ID, X coordinate, and
  Y coordinate.

- period_table:

  Data frame of period-analysis results; must contain a column named
  `"ID"`.

- variable:

  Character; name of the column in `period_table` to map to color.

- filename:

  Character; used in the plot title. Defaults to `""`.

- shape:

  Integer; point shape code. Defaults to `15`.

- size:

  Numeric; point size. Defaults to `2`.

- normalized:

  Logical; if `TRUE`, the variable is scaled to `[0, 1]` before mapping.
  Defaults to `FALSE`.

- colorcode:

  Character; name of the color scheme to use. One of `"rainbow"`,
  `"regular"`, `"inverse"`, or `"grayscale"`. Defaults to `"rainbow"`.

- invert_color:

  Logical; if `TRUE`, reverses the color palette. Defaults to `FALSE`.

- ID_col:

  Character; name of the ID column. Defaults to `"ID"`.

- ...:

  Currently unused.

## Value

A `ggplot` object.
