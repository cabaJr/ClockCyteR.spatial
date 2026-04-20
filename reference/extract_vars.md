# Extract variables from analysis results across files and save as wide tables

Extract variables from analysis results across files and save as wide
tables

## Usage

``` r
extract_vars(params, file_rows, vars, network = FALSE, individual = FALSE, ...)
```

## Arguments

- params:

  List of analysis parameters produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md);
  must contain `paths$base_dir`, `time$intervals`, and `channels`.

- file_rows:

  Data frame of file metadata; must contain columns `folder_path` and
  `file_id`.

- vars:

  Character vector of column names to extract from each result table.

- network:

  Logical; if `TRUE`, reads network result RDS files instead of
  period-table RDS files. Defaults to `FALSE`.

- individual:

  Logical; if `TRUE`, saves one row per cell instead of pivoting to a
  wide table. Defaults to `FALSE`.

- ...:

  Optional: `sub` (character) selects a sub-element from network
  results.

## Value

Called for its side effect of writing CSV tables to a `tables`
subdirectory; returns `NULL` invisibly.
