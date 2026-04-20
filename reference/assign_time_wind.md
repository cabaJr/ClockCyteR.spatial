# Assign analysis time window from file length

Assign analysis time window from file length

## Usage

``` r
assign_time_wind(time_wind, foldername, time_res = 0.5, Channel1_an = FALSE)
```

## Arguments

- time_wind:

  Logical; if `FALSE`, the full recording length is used as the
  interval. If `TRUE`, the pre-defined `intervals` object in the calling
  environment is returned unchanged.

- foldername:

  Character; path to the folder containing grid-value CSV files.

- time_res:

  Numeric; temporal resolution in hours per frame. Defaults to `0.5`.

- Channel1_an:

  Logical; if `TRUE`, reads from the channel-1 grid file; otherwise
  reads from channel 2. Defaults to `FALSE`.
