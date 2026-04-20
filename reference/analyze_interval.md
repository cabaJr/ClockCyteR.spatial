# analyze_interval

Subsets channel data to a time interval and runs per-channel analysis,
returning results aggregated across all enabled channels.

## Usage

``` r
analyze_interval(file_ctx, interval_name, interval_range, params)
```

## Arguments

- file_ctx:

  A named list of file context data as produced by
  [`prepare_file()`](https://cabajr.github.io/ClockCyteR.spatial/reference/prepare_file.md).

- interval_name:

  A character string naming the interval (e.g. `"baseline"`).

- interval_range:

  A numeric vector of length 2 giving the start and end of the interval
  in the same units as `params$time$time_res`.

- params:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

## Value

A named list with elements `interval_name` (character), `interval_range`
(numeric vector), and `channels` (a named list of per-channel results
including a combined `summary` data frame).

## Examples

``` r
if (FALSE) { # \dontrun{
interval_result <- analyze_interval(file_ctx, "baseline", c(0, 24), params)
} # }
```
