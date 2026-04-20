# Histogram or bar chart of a node variable

Histogram or bar chart of a node variable

## Usage

``` r
hist_plot(
  var,
  bin_size = 30,
  bin_color,
  x_label,
  y_label = "Count",
  title = "",
  mode = "hist",
  ...
)
```

## Arguments

- var:

  Numeric vector to plot; `NA`s are removed.

- bin_size:

  Integer; number of bins for histogram mode. Defaults to `30`.

- bin_color:

  Character; fill color for bars.

- x_label:

  Character; x-axis label.

- y_label:

  Character; y-axis label. Defaults to `"Count"`.

- title:

  Character; plot title. Defaults to `""`.

- mode:

  Character; `"hist"` for histogram or `"bar"` for bar chart. Defaults
  to `"hist"`.

- ...:

  Optional arguments: `xlims` (numeric vector of length 3: min, max,
  step) to set axis limits and breaks; `mean` (any value) to add a
  vertical mean line.

## Value

A `ggplot` object.
