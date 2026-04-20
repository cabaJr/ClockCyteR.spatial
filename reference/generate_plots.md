# generate_plots

Generates and saves all configured plot types for each file, interval,
and channel, including network plots if enabled.

## Usage

``` r
generate_plots(file_rows, params, ...)
```

## Arguments

- file_rows:

  A tibble of file metadata as produced by
  [`index_files()`](https://cabajr.github.io/ClockCyteR.spatial/reference/index_files.md).

- params:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

- ...:

  Additional arguments passed to plotting functions. Currently supports
  `p.width` (integer, plot width in pixels).

## Value

Called for its side effects (plot files written to disk). Returns `NULL`
invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_plots(file_rows, params, p.width = 1600)
} # }
```
