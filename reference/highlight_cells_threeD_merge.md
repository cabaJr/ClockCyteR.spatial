# 3D scatter map combining two channels in a single plotly figure

3D scatter map combining two channels in a single plotly figure

## Usage

``` r
highlight_cells_threeD_merge(
  all_cells = grid_coord,
  period_table1,
  period_table2,
  var_z,
  var_col,
  filename = "",
  sdFactor = 1.5,
  shape = 15,
  size = 10,
  Ch1_name = "Ch1",
  Ch2_name = "Ch2",
  xlim = c(-12, 12),
  ...
)
```

## Arguments

- all_cells:

  Data frame of grid coordinates (defaults to `grid_coord`); three
  columns: ID, X, Y.

- period_table1:

  Data frame of period-analysis results for channel 1.

- period_table2:

  Data frame of period-analysis results for channel 2.

- var_z:

  Character; column name mapped to the Z axis.

- var_col:

  Character; column name mapped to point color.

- filename:

  Character; used in the plot title. Defaults to `""`.

- sdFactor:

  Numeric; number of SDs used to clip the Z-axis range. Defaults to
  `1.5`.

- shape:

  Integer; point shape code. Defaults to `15`.

- size:

  Numeric; point size. Defaults to `10`.

- Ch1_name:

  Character; label for channel 1 in the legend. Defaults to `"Ch1"`.

- Ch2_name:

  Character; label for channel 2 in the legend. Defaults to `"Ch2"`.

- xlim:

  Numeric vector of length 2; limits for the color axis. Defaults to
  `c(-12, 12)`.

- ...:

  Currently unused.

## Value

A `plotly` figure object.
