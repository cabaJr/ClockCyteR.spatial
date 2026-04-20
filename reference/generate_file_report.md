# Render an Rmd report for a single file

Render an Rmd report for a single file

## Usage

``` r
generate_file_report(
  file_id,
  params,
  base_dir,
  intervals,
  channels,
  plot_types,
  storage_fold = "plots",
  report_template = "report_template.Rmd",
  output_dir = file.path(base_dir, "reports")
)
```

## Arguments

- file_id:

  Character; file identifier.

- params:

  List of analysis parameters.

- base_dir:

  Character; base directory of the project.

- intervals:

  Named list of analysis intervals.

- channels:

  Character vector of channel identifiers.

- plot_types:

  Character vector of plot-type keys to include in the report.

- storage_fold:

  Character; subdirectory containing the plots. Defaults to `"plots"`.

- report_template:

  Character; filename of the Rmd template. Defaults to
  `"report_template.Rmd"`.

- output_dir:

  Character; directory where the rendered report is saved. Defaults to a
  `reports` subdirectory inside `base_dir`.

## Value

Called for its side effect of rendering an HTML report; returns the
output file path invisibly.
