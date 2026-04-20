# generate_plot_type_reports

Renders one PDF report per plot type, collating the corresponding plot
across all files, intervals, and channels using an Rmd template.

## Usage

``` r
generate_plot_type_reports(
  params,
  file_rows,
  storage_fold = "plots",
  plot_report_template = "plot_type_report_template.Rmd",
  plot_report_dir = file.path(params$paths$base_dir, "reports", "plot_reports")
)
```

## Arguments

- params:

  A named list of analysis parameters as produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md).

- file_rows:

  A tibble of file metadata as produced by
  [`index_files()`](https://cabajr.github.io/ClockCyteR.spatial/reference/index_files.md).

- storage_fold:

  Character string naming the subfolder within each file's results
  directory where plots are stored. Defaults to `"plots"`.

- plot_report_template:

  Character string giving the filename of the Rmd template to use.
  Defaults to `"plot_type_report_template.Rmd"`.

- plot_report_dir:

  Character string giving the output directory for rendered reports.
  Defaults to
  `file.path(params$paths$base_dir, "reports", "plot_reports")`.

## Value

Called for its side effects (PDF reports written to `plot_report_dir`).
Returns `NULL` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_plot_type_reports(params, file_rows)
} # }
```
