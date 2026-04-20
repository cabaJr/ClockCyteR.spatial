# Copy plot files into a centralised output directory

Copy plot files into a centralised output directory

## Usage

``` r
pull_plots(params, file_rows)
```

## Arguments

- params:

  List of analysis parameters produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md);
  must contain `paths$base_dir`, `time$intervals`, `channels`, and
  `plotting$plot_types`.

- file_rows:

  Data frame of file metadata; must contain columns `folder_path` and
  `file_id`.

## Value

Called for its side effect of copying files; returns `NULL` invisibly.
