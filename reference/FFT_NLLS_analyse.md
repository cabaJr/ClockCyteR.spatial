# Estimate period, phase, and amplitude using FFT initialisation and NLLS fitting

Estimate period, phase, and amplitude using FFT initialisation and NLLS
fitting

## Usage

``` r
FFT_NLLS_analyse(data, minPer = 16, maxPer = 32, smooth = TRUE, invert = FALSE)
```

## Arguments

- data:

  Data frame with columns `t` (time in hours), `value` (raw
  fluorescence), and `smooth` (smoothed fluorescence).

- minPer:

  Numeric; lower period bound in hours. Defaults to `16`.

- maxPer:

  Numeric; upper period bound in hours. Defaults to `32`.

- smooth:

  Logical; if `TRUE`, fits the smoothed trace; if `FALSE`, fits the raw
  trace. Defaults to `TRUE`.

- invert:

  Logical; if `TRUE`, inverts the signal before fitting. Defaults to
  `FALSE`.

## Value

A named list with fitted `period`, `phase`, `amplitude`, `RAE`, and
`fitted_trace`.
