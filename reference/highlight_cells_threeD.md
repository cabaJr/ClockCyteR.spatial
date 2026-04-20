# 3D scatter map with XY spatial position and a Z-axis variable

3D scatter map with XY spatial position and a Z-axis variable

## Usage

``` r
highlight_cells_threeD(
  all_cells = grid_coord,
  period_table,
  var_z,
  var_col,
  filename = "",
  colorcode = "blue",
  sdFactor = 1,
  shape = 15,
  size = 2,
  outlier_mode = "squish",
  xlim = c(-12, 12),
  ID_col = "ID",
  ...
)
```

## Arguments

- all_cells:

  Data frame of grid coordinates (defaults to `grid_coord`); three
  columns: ID, X, Y.

- period_table:

  Data frame of period-analysis results containing `var_z` and `var_col`
  columns.

- var_z:

  Character; column name mapped to the Z axis.

- var_col:

  Character; column name mapped to point color.

- filename:

  Character; used in the plot title. Defaults to `""`.

- colorcode:

  Character; color palette name for the color axis. Defaults to
  `"blue"`.

- sdFactor:

  Numeric; number of SDs used to clip the Z-axis range. Defaults to `1`.

- shape:

  Integer; point shape code. Defaults to `15`.

- size:

  Numeric; point size. Defaults to `2`.

- outlier_mode:

  Character; how out-of-range values are handled; `"squish"` (default)
  or `"na"`.

- xlim:

  Numeric vector of length 2; X-axis limits for the color scale.
  Defaults to `c(-12, 12)`.

- ID_col:

  Character; name of the ID column. Defaults to `"ID"`.

- ...:

  Currently unused.

## Value

A `plotly` figure object.
