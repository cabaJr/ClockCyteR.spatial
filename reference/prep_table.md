# Reshape and optionally preprocess a trace matrix into long format

Reshape and optionally preprocess a trace matrix into long format

## Usage

``` r
prep_table(
  data,
  rm.start = 0,
  preprocess = TRUE,
  transp = TRUE,
  add_t = TRUE,
  time_res
)
```

## Arguments

- data:

  Numeric matrix of fluorescence traces (cells × frames), with an
  optional leading ID column.

- rm.start:

  Integer; number of initial time points to remove. Defaults to `0`.

- preprocess:

  Logical; if `TRUE`, calls
  [`preprocess_data()`](https://cabajr.github.io/ClockCyteR.spatial/reference/preprocess_data.md)
  to smooth and detrend. Defaults to `TRUE`.

- transp:

  Logical; if `TRUE`, transposes `data` before processing. Defaults to
  `TRUE`.

- add_t:

  Logical; if `TRUE`, adds a time column based on `time_res`. Defaults
  to `TRUE`.

- time_res:

  Numeric; temporal resolution in hours per frame.

## Value

A long-format `data.table` with columns `ID`, `t`, `value`, and
`smooth`.
