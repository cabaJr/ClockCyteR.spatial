# Run FFT-NLLS period analysis on a fluorescence trace matrix

Run FFT-NLLS period analysis on a fluorescence trace matrix

## Usage

``` r
computePeriod(
  df,
  excludeNC = FALSE,
  top = 30,
  bottom = 18,
  save.trace = FALSE,
  rm.start = 0,
  prep_tbl = TRUE,
  preprocess = TRUE,
  transp = TRUE,
  add_t = TRUE,
  time_res = 0.5,
  smooth = TRUE,
  invert = FALSE,
  filterRAE = NULL,
  ...
)
```

## Arguments

- df:

  Numeric matrix or data frame of fluorescence traces (cells × frames),
  or a long-format table if `prep_tbl = FALSE`.

- excludeNC:

  Logical; if `TRUE`, excludes non-circadian cells from the returned
  table. Defaults to `FALSE`.

- top:

  Numeric; upper period bound in hours. Defaults to `30`.

- bottom:

  Numeric; lower period bound in hours. Defaults to `18`.

- save.trace:

  Logical; whether to store individual fitted traces. Defaults to
  `FALSE`.

- rm.start:

  Integer; number of initial time points to discard before analysis.
  Defaults to `0`.

- prep_tbl:

  Logical; if `TRUE`, calls
  [`prep_table()`](https://cabajr.github.io/ClockCyteR.spatial/reference/prep_table.md)
  to reshape `df` before analysis. Defaults to `TRUE`.

- preprocess:

  Logical; if `TRUE`, smooths and detrends traces during table
  preparation. Defaults to `TRUE`.

- transp:

  Logical; if `TRUE`, transposes `df` during table preparation. Defaults
  to `TRUE`.

- add_t:

  Logical; if `TRUE`, adds a time column during table preparation.
  Defaults to `TRUE`.

- time_res:

  Numeric; temporal resolution in hours per frame. Defaults to `0.5`.

- smooth:

  Logical; if `TRUE`, uses the smoothed trace for analysis. Defaults to
  `TRUE`.

- invert:

  Logical; if `TRUE`, inverts the fluorescence signal before analysis.
  Defaults to `FALSE`.

- filterRAE:

  Numeric threshold for the Relative Amplitude Error; cells with RAE
  above this value are excluded. Pass `NULL` to disable filtering.
  Defaults to `NULL`.

- ...:

  Currently unused.

## Value

A named list with elements `period_table` (data frame of per-cell
results), `period_table_unfiltered`, and optionally `traces` and
`fit_traces`.
