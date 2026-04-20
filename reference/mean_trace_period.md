# Run period analysis on the mean fluorescence trace of a channel

Run period analysis on the mean fluorescence trace of a channel

## Usage

``` r
mean_trace_period(channel_ctx, filename, time_res)
```

## Arguments

- channel_ctx:

  List; channel context object containing elements `grid_vals` (numeric
  matrix, cells × frames) and `invert` (logical).

- filename:

  Character; file identifier passed to
  [`computePeriod()`](https://cabajr.github.io/ClockCyteR.spatial/reference/computePeriod.md).

- time_res:

  Numeric; temporal resolution in hours per frame.

## Value

A one-row data frame of period-analysis results for the mean trace.
