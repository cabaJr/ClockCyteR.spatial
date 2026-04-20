# Summarise period analysis results into a single-row table

Summarise period analysis results into a single-row table

## Usage

``` r
summarizePeriod(period_res, circ_stats, Ch_rep = "Ch", coherence = FALSE)
```

## Arguments

- period_res:

  Named list as returned by
  [`computePeriod()`](https://cabajr.github.io/ClockCyteR.spatial/reference/computePeriod.md);
  must contain `period_table` and `period_table_unfiltered`.

- circ_stats:

  Named list as returned by
  [`circular_stats()`](https://cabajr.github.io/ClockCyteR.spatial/reference/circular_stats.md);
  must contain `vectorLength` and `circStats`.

- Ch_rep:

  Character; channel label used as a column prefix. Defaults to `"Ch"`.

- coherence:

  Logical; if `TRUE`, includes coherence metrics in the summary.
  Defaults to `FALSE`.

## Value

A one-row data frame of summary statistics for the analysis.
