# Circularly shift traces so their phases align to the group mean

Circularly shift traces so their phases align to the group mean

## Usage

``` r
phase_align_trace(
  traces_table,
  period_table,
  remove_start = 0,
  remove_end = 0,
  align_to = NA,
  debug = FALSE
)
```

## Arguments

- traces_table:

  Numeric matrix of fluorescence traces (cells × frames).

- period_table:

  Data frame of period-analysis results with a `phase_rad` column.

- remove_start:

  Integer; frames to discard from the start after shifting. Defaults to
  `0`.

- remove_end:

  Integer; frames to discard from the end after shifting. Defaults to
  `0`.

- align_to:

  Numeric; target phase in hours; if `NA`, uses the circular mean of the
  group. Defaults to `NA`.

- debug:

  Logical; if `TRUE`, enables debugging output. Defaults to `FALSE`.

## Value

A numeric matrix of phase-aligned traces, same dimensions as
`traces_table` (minus any removed frames).
