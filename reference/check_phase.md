# Plot phase distribution across cells

Plot phase distribution across cells

## Usage

``` r
check_phase(
  data,
  phase_var,
  x_lab = "Phase (h)",
  ylab = "Cell ID",
  xlim = c(0, 24),
  pt_size = 6,
  pt_color = "black",
  plotly = TRUE,
  multi = FALSE,
  data2 = NULL,
  pt_color2 = "red",
  alpha = 1,
  ...
)
```

## Arguments

- data:

  Data frame with columns `ID` and `phase_var`.

- phase_var:

  Column name containing phase (in hours).

- x_lab, ylab:

  Axis labels.

- xlim:

  X-axis limits (default 0–24).

- pt_size:

  Point size (plotly only).

- pt_color:

  Point color.

- plotly:

  Logical; return interactive plot if TRUE.

- multi:

  Logical; overlay second dataset.

- data2:

  Optional second dataset (same structure as `data`).

- pt_color2:

  Color for second dataset (default "red").

- alpha:

  Point transparency (ggplot only).

- ...:

  Unused.

## Value

A ggplot or plotly object.
