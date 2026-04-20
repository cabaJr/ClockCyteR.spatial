# generate_reports

Renders a per-file HTML or PDF report for each file in the project,
collecting all plot types across intervals and channels.

## Usage

``` r
generate_reports(params, file_rows)
```

## Arguments

- params:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

- file_rows:

  A tibble of file metadata as produced by
  [`index_files()`](https://cabajr.github.io/ClockCyteR.spatial/reference/index_files.md).

## Value

Called for its side effects (report files written to disk). Returns
`NULL` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_reports(params, file_rows)
} # }
```
