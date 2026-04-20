# Interactive 3D scatter plot via plotly

Interactive 3D scatter plot via plotly

## Usage

``` r
threeDplot(
  data,
  x_var,
  y_var,
  z_var,
  color_var,
  col_palette = NULL,
  continuous = FALSE,
  x_lab = "x",
  y_lab = "y",
  z_lab = "z",
  title = "",
  xrange = NULL,
  yrange = NULL,
  zrange = NULL,
  eye = "pos1",
  zoom = 1
)
```

## Arguments

- data:

  Data frame containing the variables to plot.

- x_var:

  Character; column name for the X axis.

- y_var:

  Character; column name for the Y axis.

- z_var:

  Character; column name for the Z axis.

- color_var:

  Character; column name mapped to point color.

- col_palette:

  Character vector of colors for the color scale; or `NULL` to use the
  plotly default. Defaults to `NULL`.

- continuous:

  Logical; if `TRUE`, treats `color_var` as a continuous scale; if
  `FALSE`, as a discrete factor. Defaults to `FALSE`.

- x_lab:

  Character; X-axis label. Defaults to `"x"`.

- y_lab:

  Character; Y-axis label. Defaults to `"y"`.

- z_lab:

  Character; Z-axis label. Defaults to `"z"`.

- title:

  Character; plot title. Defaults to `""`.

- xrange:

  Numeric vector of length 2 for X-axis limits, or `NULL` for automatic.
  Defaults to `NULL`.

- yrange:

  Numeric vector of length 2 for Y-axis limits, or `NULL`. Defaults to
  `NULL`.

- zrange:

  Numeric vector of length 2 for Z-axis limits, or `NULL`. Defaults to
  `NULL`.

- eye:

  Character preset (`"pos1"`, `"pos2"`, `"pos3"`) or a named list with
  `x`, `y`, `z` for the camera position. Defaults to `"pos1"`.

- zoom:

  Numeric; camera zoom factor. Defaults to `1`.

## Value

A `plotly` figure object.
