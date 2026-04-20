# analyze_project

Runs the full analysis pipeline across all files in a project,
aggregating results and saving summary outputs to disk.

## Usage

``` r
analyze_project(file_rows, params)
```

## Arguments

- file_rows:

  A tibble of file metadata with columns `file_id`, `file_path`, and
  `folder_path`, as produced by
  [`index_files()`](https://cabajr.github.io/ClockCyteR.spatial/reference/index_files.md).

- params:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

## Value

A named list with elements `params` (the input parameter list),
`file_results` (a list of per-file analysis results), and
`file_contexts` (a list of per-file data contexts).

## Examples

``` r
if (FALSE) { # \dontrun{
results <- analyze_project(file_rows, params)
} # }
```
