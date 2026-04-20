# analyze_file

Runs interval and channel analysis for a single file, saving per-channel
RDS outputs and a summary table to the file's results folder.

## Usage

``` r
analyze_file(file_ctx, params)
```

## Arguments

- file_ctx:

  A named list of file context data as produced by
  [`prepare_file()`](https://cabajr.github.io/ClockCyteR.spatial/reference/prepare_file.md),
  containing elements including `file_id`, `folder_path`, `channels`,
  `grid_points_sf`, `scn_roi`, and `grid_coord`.

- params:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

## Value

A named list with elements `file_id` (character), `filename`
(character), `intervals` (a named list of interval-level results), and
`summaries` (a combined data frame of per-channel period summary
statistics across all intervals).

## Examples

``` r
if (FALSE) { # \dontrun{
file_result <- analyze_file(file_ctx, params)
} # }
```
