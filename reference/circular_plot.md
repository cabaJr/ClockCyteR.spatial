# Circular phase plot with optional mean-phase alignment

Circular phase plot with optional mean-phase alignment

## Usage

``` r
circular_plot(
  data,
  path,
  saving = TRUE,
  align = TRUE,
  align_to = 6,
  inside_plot = TRUE,
  pt_size = 0.3,
  pt_col = "#eb0000",
  ...
)
```

## Arguments

- data:

  Numeric vector of phases in radians.

- path:

  Character; file path where the SVG is saved when `saving = TRUE`.

- saving:

  Logical; whether to write the plot to file. Defaults to `TRUE`.

- align:

  Logical; whether to rotate the distribution so the mean phase falls at
  `align_to`. Defaults to `TRUE`.

- align_to:

  Numeric; target hour for the mean-phase alignment. Defaults to `6`.

- inside_plot:

  Logical; reserved for future use. Defaults to `TRUE`.

- pt_size:

  Numeric; point size for individual observations. Defaults to `0.30`.

- pt_col:

  Character; color for individual observation points. Defaults to
  `"#eb0000"`.

- ...:

  Optional arguments passed to downstream calls; supports `plot_title`
  (character title shown above the plot).

## Value

Invisibly `NULL`; the plot is written to file when `saving = TRUE`.
