# Build the file path for a single plot output

Build the file path for a single plot output

## Usage

``` r
build_plot_path(file_id, ch_id, ch_id_folder, plot_name, save_format)
```

## Arguments

- file_id:

  Character; file identifier used as the base of the filename.

- ch_id:

  Character; channel identifier (e.g. `"ch1"`).

- ch_id_folder:

  Character; directory where the plot file will be saved.

- plot_name:

  Character; plot-type name appended to the filename.

- save_format:

  Character; file extension (e.g. `"svg"`).

## Value

Character; full file path for the plot.
