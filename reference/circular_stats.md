# Compute circular statistics from a period table

Compute circular statistics from a period table

## Usage

``` r
circular_stats(period_table)
```

## Arguments

- period_table:

  Data frame of period-analysis results; must contain a `phase_rad`
  column.

## Value

A named list with elements `mean_phase`, `RT` (Rayleigh test result),
`vectorLength`, `circStats` (Rayleigh p-value), and `phase_var` (angular
variance).
