# Compute per-variable display ranges across all files and intervals

Compute per-variable display ranges across all files and intervals

## Usage

``` r
ranges_calculation(params, file_rows)
```

## Arguments

- params:

  List of analysis parameters produced by
  [`make_params()`](https://cabajr.github.io/ClockCyteR.spatial/reference/make_params.md);
  must contain `channels` and `time$intervals`.

- file_rows:

  Data frame of file metadata; must contain columns `file_id`,
  `file_path`, and `folder_path`.

## Value

A named list (by channel) of per-variable min/max ranges used to set
consistent colour scales across plots.
