# analyze_channel

Runs the full analysis pipeline for a single channel within a given
interval, including period estimation, coherence metrics, AUC, network
analysis, and summary statistics.

## Usage

``` r
analyze_channel(channel_name, channel_ctx, file_ctx, params)
```

## Arguments

- channel_name:

  A character string identifying the channel (e.g. `"Ch1"`).

- channel_ctx:

  A named list of channel context data containing `grid_vals` (matrix of
  cell fluorescence traces, cells × frames), `grid_points_sf` (sf object
  of grid points), `scn_roi` (sf object), `grid_coord` (data frame of
  grid coordinates), `invert` (logical), and `ch_label` (character).

- file_ctx:

  A named list of file context data as produced by
  [`prepare_file()`](https://cabajr.github.io/ClockCyteR.spatial/reference/prepare_file.md).

- params:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

## Value

A named list with elements `ch_id`, `ch_label`, `cells` (input grid
values matrix), `period_table`, `auc_results`, `network_results`,
`circular_stats`, `period_summary` (data frame), `detrended_traces`,
`mut_inf`, and `status`.

## Examples

``` r
if (FALSE) { # \dontrun{
ch_result <- analyze_channel("Ch1", channel_ctx, file_ctx, params)
} # }
```
