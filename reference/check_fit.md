# Plot observed vs fitted time series for a single cell

Plot observed vs fitted time series for a single cell

## Usage

``` r
check_fit(
  data,
  ID,
  title = "",
  xlab = "Time (h)",
  ylab = "Value",
  smooth = TRUE,
  time_res = 0.5,
  ...
)
```

## Arguments

- data:

  List containing `traces`, `fit_traces`, and optionally `smooth_traces`
  (matrices: cells × timepoints).

- ID:

  Row index of the cell to plot.

- title:

  Plot title.

- xlab, ylab:

  Axis labels.

- smooth:

  Logical; overlay smoothed trace if available.

- time_res:

  Time resolution in hours.

- ...:

  Unused.

## Value

A ggplot object.
