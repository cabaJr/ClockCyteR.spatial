# Copy plots into a subdirectory (deprecated, use `pull_plots()`)

Copy plots into a subdirectory (deprecated, use
[`pull_plots()`](https://cabajr.github.io/ClockCyteR.spatial/reference/pull_plots.md))

## Usage

``` r
pull_plots_old(
  base.dir,
  plot.name,
  dir.name,
  file.names,
  mainfold = FALSE,
  second_fold = "",
  channel,
  storage_fold = "plots",
  short = FALSE,
  shortname = "",
  ...
)
```

## Arguments

- base.dir:

  Character; base directory of the project.

- plot.name:

  Character; plot filename stem.

- dir.name:

  Character; name of the destination subdirectory.

- file.names:

  Character vector of file identifiers.

- mainfold:

  Logical; if `TRUE`, looks for plots directly inside each file's
  results folder. Defaults to `FALSE`.

- second_fold:

  Character; optional intermediate subdirectory path. Defaults to `""`.

- channel:

  Character; channel identifier used in plot filenames.

- storage_fold:

  Character; top-level output folder. Defaults to `"plots"`.

- short:

  Logical; if `TRUE`, uses `shortname` as the destination filename.
  Defaults to `FALSE`.

- shortname:

  Character; filename used when `short = TRUE`. Defaults to `""`.

- ...:

  Currently unused.

## Value

Character vector of destination file paths (invisibly).
