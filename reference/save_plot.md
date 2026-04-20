# Save a single ggplot object to file

Save a single ggplot object to file

## Usage

``` r
save_plot(plot, saving_path, format, p.width = 560, p.height = 1100)
```

## Arguments

- plot:

  A `ggplot` object to save.

- saving_path:

  Character; full file path for the output.

- format:

  Character; file extension / device type (e.g. `"svg"`).

- p.width:

  Numeric; output width in pixels. Defaults to `560`.

- p.height:

  Numeric; output height in pixels. Defaults to `1100`.

## Value

Called for its side effect of writing a file; returns `NULL` invisibly.
