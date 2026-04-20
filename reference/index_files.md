# index_files

index_files

## Usage

``` r
index_files(paths)
```

## Arguments

- paths:

  file paths of the time series values to import

## Value

A tibble with one row per dataset and columns: `file_id` (filename
without extension), `file_path` (full path to the original TIFF, or `NA`
if not present), and `folder_path` (path to the corresponding results
folder).

## Details

Discovery is attempted in three stages, stopping at the first success:

1.  Scan for `*_results` subdirectories containing a
    `grid_centroids.csv` sentinel file.

2.  Read a `manifest.csv` in `paths$base_dir` with columns `file_id` and
    `folder_path`.

3.  Scan for TIFF files (original behaviour, requires images present).

## Examples

``` r
if (FALSE) { # \dontrun{
index_files(paths = params$paths)
} # }
```
