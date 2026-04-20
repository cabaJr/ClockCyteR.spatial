# add_coherence_metrics

Computes spatial coherence scores for period, phase, and amplitude, and
calculates normalised mutual information between period and phase
coherence.

## Usage

``` r
add_coherence_metrics(period_tbl, grid_coord)
```

## Arguments

- period_tbl:

  A data frame of per-cell period analysis results containing at minimum
  columns `ID`, `period`, `phase_circ`, and `amplitude`.

- grid_coord:

  A data frame of grid coordinates used to define spatial neighbourhoods
  for coherence calculation.

## Value

A named list with elements `period_table` (the input data frame
augmented with columns `period_crn`, `amp_crn`, and `phase_crn`) and
`mi` (a numeric scalar for normalised mutual information between period
and phase coherence).

## Examples

``` r
if (FALSE) { # \dontrun{
coh <- add_coherence_metrics(period_tbl, grid_coord)
} # }
```
